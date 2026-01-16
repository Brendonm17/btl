#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

static void resetStack(VM* vm) {
    vm->stackTop = vm->stack;
    vm->frameCount = 0;
    vm->openUpvalues = NULL;
}

static void runtimeError(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm->frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in script\n", function->chunk.lines[instruction]);
    }
    resetStack(vm);
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) return NULL;
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    char* buffer = (char*) malloc(fileSize + 1);
    size_t bytesRead = fread(buffer, 1, fileSize, file);
    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

static struct ObjString* findGlobalName(ObjModule* module, uint8_t index) {
    for (int i = 0; i < module->globalNames.capacity; i++) {
        Entry* entry = &module->globalNames.entries[i];
        if (entry->key != NULL && (int) AS_NUMBER(entry->value) == (int) index) {
            return entry->key;
        }
    }
    return NULL;
}

static Value clockNative(int argCount, Value* args) {
    (void) argCount; (void) args;
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

static void defineNative(VM* vm, const char* name, NativeFn function) {
    struct ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    push(vm, OBJ_VAL(newNative(vm, function)));

    Value indexValue;
    int index;
    if (tableGet(&vm->rootModule->globalNames, nameStr, &indexValue)) {
        index = (int) AS_NUMBER(indexValue);
    } else {
        index = vm->rootModule->globalValues.count;
        writeValueArray(vm, &vm->rootModule->globalValues, EMPTY_VAL);
        tableSet(vm, &vm->rootModule->globalNames, nameStr, NUMBER_VAL((double) index));
    }
    vm->rootModule->globalValues.values[index] = vm->stack[1];
    pop(vm);
    pop(vm);
}

void initVM(VM* vm) {
    resetStack(vm);
    vm->objects = NULL;
    vm->bytesAllocated = 0;
    vm->nextGC = 1024 * 1024;
    vm->grayCount = 0;
    vm->grayCapacity = 0;
    vm->grayStack = NULL;
    vm->compiler = NULL;

    initTable(&vm->strings);
    initTable(&vm->modules);
    vm->rootModule = newModule(vm, copyString(vm, "main", 4));
    vm->initString = copyString(vm, "init", 4);
    defineNative(vm, "clock", clockNative);
}

void freeVM(VM* vm) {
    freeTable(vm, &vm->strings);
    freeTable(vm, &vm->modules);
    vm->initString = NULL;
    freeObjects(vm);
}

void push(VM* vm, Value value) {
    *vm->stackTop = value; vm->stackTop++;
}
Value pop(VM* vm) {
    vm->stackTop--; return *vm->stackTop;
}
static Value peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

static bool call(VM* vm, ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }
    if (vm->frameCount == FRAMES_MAX) {
        runtimeError(vm, "Stack overflow."); return false;
    }
    CallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm->stackTop - argCount - 1;
    return true;
}

static bool callValue(VM* vm, Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
            vm->stackTop[-argCount - 1] = bound->receiver;
            return call(vm, bound->method, argCount);
        }
        case OBJ_CLASS: {
            ObjClass* klass = AS_CLASS(callee);
            vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(vm, klass));
            Value initializer;
            if (tableGet(&klass->methods, vm->initString, &initializer)) {
                return call(vm, AS_CLOSURE(initializer), argCount);
            }
            return true;
        }
        case OBJ_CLOSURE: return call(vm, AS_CLOSURE(callee), argCount);
        case OBJ_NATIVE: {
            NativeFn native = AS_NATIVE(callee);
            Value result = native(argCount, vm->stackTop - argCount);
            vm->stackTop -= argCount + 1;
            push(vm, result);
            return true;
        }
        default: break;
        }
    }
    return false;
}

static bool invokeFromClass(VM* vm, ObjClass* klass, struct ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) return false;
    return call(vm, AS_CLOSURE(method), argCount);
}

static bool invoke(VM* vm, struct ObjString* name, int argCount) {
    Value receiver = peek(vm, argCount);
    if (!IS_OBJ(receiver)) return false;
    if (IS_INSTANCE(receiver)) {
        ObjInstance* i = AS_INSTANCE(receiver);
        Value v;
        if (tableGet(&i->fields, name, &v)) {
            vm->stackTop[-argCount - 1] = v; return callValue(vm, v, argCount);
        }
        return invokeFromClass(vm, i->klass, name, argCount);
    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        Value idx;
        if (tableGet(&m->globalNames, name, &idx)) {
            int i = (int) AS_NUMBER(idx);
            vm->stackTop[-argCount - 1] = m->globalValues.values[i];
            return callValue(vm, m->globalValues.values[i], argCount);
        }
    }
    return false;
}

static bool bindMethod(VM* vm, ObjClass* klass, struct ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) return false;
    ObjBoundMethod* bound = newBoundMethod(vm, peek(vm, 0), AS_CLOSURE(method));
    pop(vm); push(vm, OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(VM* vm, Value* local) {
    ObjUpvalue* prev = NULL; ObjUpvalue* u = vm->openUpvalues;
    while (u != NULL && u->location > local) {
        prev = u; u = u->next;
    }
    if (u != NULL && u->location == local) return u;
    ObjUpvalue* created = newUpvalue(vm, local); created->next = u;
    if (prev == NULL) vm->openUpvalues = created; else prev->next = created;
    return created;
}

static void closeUpvalues(VM* vm, Value* last) {
    while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
        ObjUpvalue* u = vm->openUpvalues; u->closed = *u->location; u->location = &u->closed; vm->openUpvalues = u->next;
    }
}

static InterpretResult run(VM* vm) {
    CallFrame* frame = &vm->frames[vm->frameCount - 1];
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(vType, op) do { \
    double b = AS_NUMBER(pop(vm)); double a = AS_NUMBER(pop(vm)); push(vm, vType(a op b)); \
  } while (false)

#ifdef HAS_COMPUTED_GOTOS
    static void* dispatchTable [] = {
      && L_OP_CONSTANT,&& L_OP_NIL,&& L_OP_TRUE,&& L_OP_FALSE,&& L_OP_POP,
      && L_OP_GET_LOCAL,&& L_OP_SET_LOCAL,&& L_OP_GET_GLOBAL,&& L_OP_DEFINE_GLOBAL,
      && L_OP_SET_GLOBAL,&& L_OP_GET_UPVALUE,&& L_OP_SET_UPVALUE,&& L_OP_GET_PROPERTY,
      && L_OP_SET_PROPERTY,&& L_OP_GET_SUPER,&& L_OP_EQUAL,&& L_OP_GREATER,&& L_OP_LESS,
      && L_OP_ADD,&& L_OP_SUBTRACT,&& L_OP_MULTIPLY,&& L_OP_DIVIDE,&& L_OP_NOT,
      && L_OP_NEGATE,&& L_OP_PRINT,&& L_OP_JUMP,&& L_OP_JUMP_IF_FALSE,&& L_OP_LOOP,
      && L_OP_CALL,&& L_OP_INVOKE,&& L_OP_SUPER_INVOKE,&& L_OP_CLOSURE,
      && L_OP_CLOSE_UPVALUE,&& L_OP_RETURN,&& L_OP_CLASS,&& L_OP_INHERIT,&& L_OP_METHOD,
      && L_OP_BUILD_LIST,&& L_OP_INDEX_GET,&& L_OP_INDEX_SET,&& L_OP_IMPORT
    };
#define DISPATCH() goto *dispatchTable[READ_BYTE()]
#define OPCODE(name) L_##name
    DISPATCH();
#else
#define DISPATCH() break
#define OPCODE(name) case name
    while (true) {
        switch (READ_BYTE()) {
#endif

            OPCODE(OP_CONSTANT) : push(vm, READ_CONSTANT()); DISPATCH();
            OPCODE(OP_NIL) : push(vm, NIL_VAL); DISPATCH();
            OPCODE(OP_TRUE) : push(vm, BOOL_VAL(true)); DISPATCH();
            OPCODE(OP_FALSE) : push(vm, BOOL_VAL(false)); DISPATCH();
            OPCODE(OP_POP) : pop(vm); DISPATCH();
            OPCODE(OP_GET_LOCAL) : push(vm, frame->slots[READ_BYTE()]); DISPATCH();
            OPCODE(OP_SET_LOCAL) : frame->slots[READ_BYTE()] = peek(vm, 0); DISPATCH();
            OPCODE(OP_GET_GLOBAL) : {
                uint8_t i = READ_BYTE(); ObjModule* m = frame->closure->function->module;
                if (IS_EMPTY(m->globalValues.values[i])) {
                    struct ObjString* n = findGlobalName(m, i); runtimeError(vm, "Undefined variable '%s'.", n ? n->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, m->globalValues.values[i]); DISPATCH();
            }
            OPCODE(OP_DEFINE_GLOBAL) : { uint8_t i = READ_BYTE(); frame->closure->function->module->globalValues.values[i] = peek(vm, 0); pop(vm); DISPATCH(); }
            OPCODE(OP_SET_GLOBAL) : {
                uint8_t i = READ_BYTE(); ObjModule* m = frame->closure->function->module;
                if (IS_EMPTY(m->globalValues.values[i])) {
                    runtimeError(vm, "Undefined."); return INTERPRET_RUNTIME_ERROR;
                }
                m->globalValues.values[i] = peek(vm, 0); DISPATCH();
            }
            OPCODE(OP_GET_UPVALUE) : push(vm, *frame->closure->upvalues[READ_BYTE()]->location); DISPATCH();
            OPCODE(OP_SET_UPVALUE) : *frame->closure->upvalues[READ_BYTE()]->location = peek(vm, 0); DISPATCH();
            OPCODE(OP_IMPORT) : {
                struct ObjString* filename = AS_STRING(READ_CONSTANT()); Value mVal;
                if (tableGet(&vm->modules, filename, &mVal)) {
                    push(vm, mVal); DISPATCH();
                }
                char* src = readFile(filename->chars); if (!src) {
                    runtimeError(vm, "File error."); return INTERPRET_RUNTIME_ERROR;
                }
                ObjModule* m = newModule(vm, filename); push(vm, OBJ_VAL(m)); ObjFunction* f = compile(vm, m, src); free(src);
                if (!f) return INTERPRET_RUNTIME_ERROR;
                push(vm, OBJ_VAL(f));
                ObjClosure* c = newClosure(vm, f);
                pop(vm);
                push(vm, OBJ_VAL(c));
                if (!call(vm, c, 0)) return INTERPRET_RUNTIME_ERROR;
                tableSet(vm, &vm->modules, filename, OBJ_VAL(m)); pop(vm); push(vm, OBJ_VAL(m));
                frame = &vm->frames[vm->frameCount - 1]; DISPATCH();
            }
            OPCODE(OP_GET_PROPERTY) : {
                Value receiver = peek(vm, 0); struct ObjString* name = AS_STRING(READ_CONSTANT());
                if (IS_INSTANCE(receiver)) {
                    ObjInstance* i = AS_INSTANCE(receiver); Value v;
                    if (tableGet(&i->fields, name, &v)) {
                        pop(vm); push(vm, v); DISPATCH();
                    }
                    if (bindMethod(vm, i->klass, name)) {
                        frame = &vm->frames[vm->frameCount - 1]; DISPATCH();
                    }
                } else if (IS_MODULE(receiver)) {
                    ObjModule* m = AS_MODULE(receiver); Value idx;
                    if (tableGet(&m->globalNames, name, &idx)) {
                        pop(vm); push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]); DISPATCH();
                    }
                }
                runtimeError(vm, "Property error."); return INTERPRET_RUNTIME_ERROR;
            }
            OPCODE(OP_BUILD_LIST) : {
                int count = READ_BYTE(); ObjList* l = newList(vm); push(vm, OBJ_VAL(l));
                for (int i = 0; i < count; i++) writeValueArray(vm, &l->items, peek(vm, count - i + 1));
                vm->stackTop -= (count + 1); push(vm, OBJ_VAL(l)); DISPATCH();
            }
            OPCODE(OP_INDEX_GET) : {
                Value idxVal = pop(vm); Value lVal = pop(vm); int i = (int) AS_NUMBER(idxVal); ObjList* l = AS_LIST(lVal);
                if (i < 0 || i >= l->items.count) {
                    runtimeError(vm, "Bounds error."); return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, l->items.values[i]); DISPATCH();
            }
            OPCODE(OP_INDEX_SET) : {
                Value v = pop(vm); Value idxVal = pop(vm); Value lVal = pop(vm); int i = (int) AS_NUMBER(idxVal); ObjList* l = AS_LIST(lVal);
                if (i < 0 || i >= l->items.count) {
                    runtimeError(vm, "Bounds error."); return INTERPRET_RUNTIME_ERROR;
                }
                l->items.values[i] = v; push(vm, v); DISPATCH();
            }
            OPCODE(OP_RETURN) : {
                Value res = pop(vm); closeUpvalues(vm, frame->slots); vm->frameCount--;
                if (vm->frameCount == 0) {
                    pop(vm); return INTERPRET_OK;
                }
                vm->stackTop = frame->slots; push(vm, res); frame = &vm->frames[vm->frameCount - 1]; DISPATCH();
            }
            OPCODE(OP_ADD) : { if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
                struct ObjString* b = AS_STRING(pop(vm)); struct ObjString* a = AS_STRING(pop(vm)); int len = a->length + b->length; char* chars = (char*) malloc(len + 1);
                memcpy(chars, a->chars, a->length); memcpy(chars + a->length, b->chars, b->length); chars[len] = '\0'; push(vm, OBJ_VAL(takeString(vm, chars, len)));
            } else BINARY_OP(NUMBER_VAL, +); DISPATCH(); }
            OPCODE(OP_EQUAL) : { Value b = pop(vm); Value a = pop(vm); push(vm, BOOL_VAL(valuesEqual(a, b))); DISPATCH(); }
            OPCODE(OP_GREATER) : BINARY_OP(BOOL_VAL, > ); DISPATCH(); OPCODE(OP_LESS) : BINARY_OP(BOOL_VAL, < ); DISPATCH();
            OPCODE(OP_SUBTRACT) : BINARY_OP(NUMBER_VAL, -); DISPATCH(); OPCODE(OP_MULTIPLY) : BINARY_OP(NUMBER_VAL, *); DISPATCH(); OPCODE(OP_DIVIDE) : BINARY_OP(NUMBER_VAL, / ); DISPATCH();
            OPCODE(OP_NOT) : { Value v = pop(vm); push(vm, BOOL_VAL(IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v)))); DISPATCH(); }
            OPCODE(OP_NEGATE) : { if (!IS_NUMBER(peek(vm, 0))) {
                runtimeError(vm, "Must be number."); return INTERPRET_RUNTIME_ERROR;
            } push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm)))); DISPATCH(); }
            OPCODE(OP_PRINT) : printValue(pop(vm)); printf("\n"); DISPATCH();
            OPCODE(OP_JUMP) : {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                DISPATCH();
            }
            OPCODE(OP_JUMP_IF_FALSE) : { int o = READ_SHORT(); Value v = peek(vm, 0); if (IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v))) frame->ip += o; DISPATCH(); }
            OPCODE(OP_LOOP) : {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                DISPATCH();
            }
            OPCODE(OP_CALL) : { int a = READ_BYTE(); if (!callValue(vm, peek(vm, a), a)) return INTERPRET_RUNTIME_ERROR; frame = &vm->frames[vm->frameCount - 1]; DISPATCH(); }
            OPCODE(OP_INVOKE) : { struct ObjString* m = AS_STRING(READ_CONSTANT()); int a = READ_BYTE(); if (!invoke(vm, m, a)) return INTERPRET_RUNTIME_ERROR; frame = &vm->frames[vm->frameCount - 1]; DISPATCH(); }
            OPCODE(OP_CLOSURE) : {
                ObjFunction* f = AS_FUNCTION(READ_CONSTANT()); ObjClosure* c = newClosure(vm, f); push(vm, OBJ_VAL(c));
                for (int i = 0; i < f->upvalueCount; i++) {
                    uint8_t isL = READ_BYTE(); uint8_t idx = READ_BYTE(); if (isL) c->upvalues[i] = captureUpvalue(vm, frame->slots + idx); else c->upvalues[i] = frame->closure->upvalues[idx];
                } DISPATCH();
            }
            OPCODE(OP_CLOSE_UPVALUE) : closeUpvalues(vm, vm->stackTop - 1); pop(vm); DISPATCH();
            OPCODE(OP_CLASS) : push(vm, OBJ_VAL(newClass(vm, AS_STRING(READ_CONSTANT())))); DISPATCH();
            OPCODE(OP_INHERIT) : { ObjClass* s = AS_CLASS(peek(vm, 1)); ObjClass* sub = AS_CLASS(peek(vm, 0)); tableAddAll(vm, &s->methods, &sub->methods); pop(vm); DISPATCH(); }
            OPCODE(OP_METHOD) : { struct ObjString* n = AS_STRING(READ_CONSTANT()); ObjClass* k = AS_CLASS(peek(vm, 1)); tableSet(vm, &k->methods, n, peek(vm, 0)); pop(vm); DISPATCH(); }
            OPCODE(OP_SET_PROPERTY) : {
                Value val = pop(vm); if (!IS_INSTANCE(peek(vm, 0))) {
                    runtimeError(vm, "Only instances have fields."); return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* i = AS_INSTANCE(pop(vm)); tableSet(vm, &i->fields, AS_STRING(READ_CONSTANT()), val); push(vm, val); DISPATCH();
            }
            OPCODE(OP_GET_SUPER) : { struct ObjString* n = AS_STRING(READ_CONSTANT()); ObjClass* s = AS_CLASS(pop(vm)); if (!bindMethod(vm, s, n)) return INTERPRET_RUNTIME_ERROR; frame = &vm->frames[vm->frameCount - 1]; DISPATCH(); }
            OPCODE(OP_SUPER_INVOKE) : { struct ObjString* m = AS_STRING(READ_CONSTANT()); int a = READ_BYTE(); ObjClass* s = AS_CLASS(pop(vm)); if (!invokeFromClass(vm, s, m, a)) return INTERPRET_RUNTIME_ERROR; frame = &vm->frames[vm->frameCount - 1]; DISPATCH(); }

#ifndef HAS_COMPUTED_GOTOS
        }
    }
#endif
}

InterpretResult interpret(VM* vm, ObjModule* m, const char* src) {
    ObjFunction* f = compile(vm, m, src); if (!f) return INTERPRET_COMPILE_ERROR;
    push(vm, OBJ_VAL(f)); ObjClosure* c = newClosure(vm, f); pop(vm); push(vm, OBJ_VAL(c)); call(vm, c, 0); return run(vm);
}