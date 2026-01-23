#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"
#include "value.h"

// --- Stack & Error Handling ---

static void resetStack(VM* vm) {
    vm->stackTop = vm->stack;
    vm->frameCount = 0;
    vm->openUpvalues = NULL;
    for (int i = 0; i < STACK_MAX; i++) {
        vm->stack[i] = NIL_VAL;
    }
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

// --- IO & Natives ---

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) return NULL;
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    char* buffer = (char*) malloc(fileSize + 1);
    if (!buffer) {
        fclose(file); return NULL;
    }
    size_t bytesRead = fread(buffer, 1, fileSize, file);
    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

static struct ObjString* findGlobalName(ObjModule* module, int index) {
    for (int i = 0; i < module->globalNames.capacity; i++) {
        Entry* entry = &module->globalNames.entries[i];
        if (!IS_EMPTY(entry->key) && IS_STRING(entry->key) &&
            (int) AS_NUMBER(entry->value) == index) {
            return AS_STRING(entry->key);
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
    if (tableGet(&vm->rootModule->globalNames, OBJ_VAL(nameStr), &indexValue)) {
        index = (int) AS_NUMBER(indexValue);
    } else {
        index = vm->rootModule->globalValues.count;
        writeValueArray(vm, &vm->rootModule->globalValues, EMPTY_VAL);
        tableSet(vm, &vm->rootModule->globalNames, OBJ_VAL(nameStr), NUMBER_VAL((double) index));
    }
    vm->rootModule->globalValues.values[index] = vm->stack[1];
    pop(vm);
    pop(vm);
}

// --- VM Lifecycle ---

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

// --- Stack Operations ---

void push(VM* vm, Value value) {
    *vm->stackTop = value; vm->stackTop++;
}

Value pop(VM* vm) {
    vm->stackTop--; return *vm->stackTop;
}

static Value peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

// --- Logic Helpers ---

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
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

// --- Call Logic ---

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
            if (tableGet(&klass->methods, OBJ_VAL(vm->initString), &initializer)) {
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
    runtimeError(vm, "Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(VM* vm, ObjClass* klass, struct ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) return false;

    ObjClosure* closure = AS_CLOSURE(method);
    if (vm->frameCount == FRAMES_MAX) {
        runtimeError(vm, "Stack overflow."); return false;
    }
    CallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm->stackTop - argCount - 1;
    return true;
}

static bool invoke(VM* vm, struct ObjString* name, int argCount) {
    Value receiver = peek(vm, argCount);
    if (!IS_OBJ(receiver)) {
        runtimeError(vm, "Only instances have methods."); return false;
    }

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        Value value;
        if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
            vm->stackTop[-argCount - 1] = value;
            return callValue(vm, value, argCount);
        }
        if (invokeFromClass(vm, instance->klass, name, argCount)) return true;
    }

    if (name->length > 0) {
        ObjString* rawName = copyString(vm, name->chars, name->length - 1);
        push(vm, OBJ_VAL(rawName));

        bool found = false;
        Value methodVal = NIL_VAL;

        if (IS_MODULE(receiver)) {
            ObjModule* m = AS_MODULE(receiver);
            Value idx;
            if (tableGet(&m->globalNames, OBJ_VAL(rawName), &idx)) {
                methodVal = m->globalValues.values[(int) AS_NUMBER(idx)];
                found = true;
            }
        } else if (IS_INSTANCE(receiver)) {
            ObjInstance* i = AS_INSTANCE(receiver);
            if (tableGet(&i->fields, OBJ_VAL(rawName), &methodVal)) {
                found = true;
            } else if (tableGet(&i->klass->methods, OBJ_VAL(rawName), &methodVal)) {
                found = true;
            }
        }

        pop(vm); // Pop rawName

        if (found) {
            vm->stackTop[-argCount - 1] = methodVal;
            return callValue(vm, methodVal, argCount);
        }
    }

    runtimeError(vm, "Undefined property '%s'.", name->chars);
    return false;
}

static bool bindMethod(VM* vm, ObjClass* klass, struct ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) return false;
    ObjBoundMethod* bound = newBoundMethod(vm, peek(vm, 0), AS_CLOSURE(method));
    pop(vm); push(vm, OBJ_VAL(bound));
    return true;
}

static void defineMethod(VM* vm, ObjClass* klass, ObjString* name, Value method) {
    tableSet(vm, &klass->methods, OBJ_VAL(name), method);
    if (name->length > 0) {
        ObjString* rawName = copyString(vm, name->chars, name->length - 1);
        push(vm, OBJ_VAL(rawName));
        tableSet(vm, &klass->methods, OBJ_VAL(rawName), method);
        pop(vm);
    }
}

static ObjString* valueToString(struct VM* vm, Value value) {
    if (IS_STRING(value)) return AS_STRING(value);
    char buf[32];
    if (IS_NUMBER(value)) {
        int len = snprintf(buf, 32, "%g", AS_NUMBER(value));
        return copyString(vm, buf, len);
    }
    if (IS_BOOL(value)) return copyString(vm, AS_BOOL(value) ? "true" : "false", AS_BOOL(value) ? 4 : 5);
    if (IS_NIL(value)) return copyString(vm, "nil", 3);
    return copyString(vm, "<object>", 8);
}

static void concatenate(struct VM* vm) {
    Value bVal = peek(vm, 0); Value aVal = peek(vm, 1);
    ObjString* a = valueToString(vm, aVal); push(vm, OBJ_VAL(a));
    ObjString* b = valueToString(vm, bVal); push(vm, OBJ_VAL(b));
    int length = a->length + b->length;
    char* chars = ALLOCATE(vm, char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';
    ObjString* result = takeString(vm, chars, length);
    pop(vm); pop(vm); pop(vm); pop(vm);
    push(vm, OBJ_VAL(result));
}

#ifdef DEBUG_TRACE_EXECUTION
static void traceExecution(VM* vm) {
    CallFrame* frame = &vm->frames[vm->frameCount - 1];
    fprintf(stderr, "          ");
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        fprintf(stderr, "[ "); printValueStderr(*slot); fprintf(stderr, " ]");
    }
    fprintf(stderr, "\n");
    int offset = (int) (frame->ip - frame->closure->function->chunk.code);
    disassembleInstruction(&frame->closure->function->chunk, offset);
}
#endif

// --- Interpreter ---

static InterpretResult run(VM* vm) {
    register CallFrame* frame = &vm->frames[vm->frameCount - 1];
    register uint8_t* ip = frame->ip;
    int argCount;

#define STORE_FRAME() (frame->ip = ip)
#define REFRESH_FRAME() (frame = &vm->frames[vm->frameCount - 1], ip = frame->ip)
#define READ_BYTE() (*ip++)
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->closure->function->chunk.constants.values[READ_SHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

#define BINARY_OP(vType, op) do { \
    if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) { \
        double b = AS_NUMBER(pop(vm)); double a = AS_NUMBER(pop(vm)); \
        push(vm, vType(a op b)); \
    } else { \
        STORE_FRAME(); runtimeError(vm, "Operands must be numbers."); return INTERPRET_RUNTIME_ERROR; \
    } \
  } while (false)

#ifdef DEBUG_TRACE_EXECUTION
#define TRACE_IF_ENABLED() traceExecution(vm)
#else
#define TRACE_IF_ENABLED() do { } while (0)
#endif

#ifdef HAS_COMPUTED_GOTOS
    static void* dispatchTable [] = {
        && L_OP_CONSTANT,&& L_OP_CONSTANT_LONG,&& L_OP_NIL,&& L_OP_TRUE,&& L_OP_FALSE,&& L_OP_POP,&& L_OP_POP_N,
        && L_OP_GET_LOCAL,&& L_OP_SET_LOCAL,&& L_OP_GET_GLOBAL,&& L_OP_GET_GLOBAL_LONG,
        && L_OP_DEFINE_GLOBAL,&& L_OP_DEFINE_GLOBAL_LONG,&& L_OP_SET_GLOBAL,&& L_OP_SET_GLOBAL_LONG,
        && L_OP_GET_UPVALUE,&& L_OP_SET_UPVALUE,&& L_OP_GET_PROPERTY,&& L_OP_GET_PROPERTY_LONG,
        && L_OP_SET_PROPERTY,&& L_OP_SET_PROPERTY_LONG,&& L_OP_GET_SUPER,&& L_OP_GET_SUPER_LONG,
        && L_OP_EQUAL,&& L_OP_GREATER,&& L_OP_LESS,&& L_OP_ADD,&& L_OP_SUBTRACT,&& L_OP_MULTIPLY,
        && L_OP_DIVIDE,&& L_OP_MODULO,&& L_OP_NOT,&& L_OP_NEGATE,&& L_OP_PRINT,&& L_OP_JUMP,
        && L_OP_JUMP_IF_FALSE,&& L_OP_LOOP,
        && L_OP_CALL_0,&& L_OP_CALL_1,&& L_OP_CALL_2,&& L_OP_CALL_3,&& L_OP_CALL_4,
        && L_OP_CALL_5,&& L_OP_CALL_6,&& L_OP_CALL_7,&& L_OP_CALL_8,&& L_OP_CALL,
        && L_OP_TAIL_CALL_0,&& L_OP_TAIL_CALL_1,&& L_OP_TAIL_CALL_2,&& L_OP_TAIL_CALL_3,&& L_OP_TAIL_CALL_4,
        && L_OP_TAIL_CALL_5,&& L_OP_TAIL_CALL_6,&& L_OP_TAIL_CALL_7,&& L_OP_TAIL_CALL_8,&& L_OP_TAIL_CALL,
        && L_OP_INVOKE_0,&& L_OP_INVOKE_1,&& L_OP_INVOKE_2,&& L_OP_INVOKE_3,&& L_OP_INVOKE_4,
        && L_OP_INVOKE_5,&& L_OP_INVOKE_6,&& L_OP_INVOKE_7,&& L_OP_INVOKE_8,
        && L_OP_TAIL_INVOKE_0,&& L_OP_TAIL_INVOKE_1,&& L_OP_TAIL_INVOKE_2,&& L_OP_TAIL_INVOKE_3,&& L_OP_TAIL_INVOKE_4,
        && L_OP_TAIL_INVOKE_5,&& L_OP_TAIL_INVOKE_6,&& L_OP_TAIL_INVOKE_7,&& L_OP_TAIL_INVOKE_8,
        && L_OP_INVOKE,&& L_OP_INVOKE_LONG,&& L_OP_TAIL_INVOKE,&& L_OP_TAIL_INVOKE_LONG,
        && L_OP_SUPER_INVOKE_0,&& L_OP_SUPER_INVOKE_1,&& L_OP_SUPER_INVOKE_2,&& L_OP_SUPER_INVOKE_3,&& L_OP_SUPER_INVOKE_4,
        && L_OP_SUPER_INVOKE_5,&& L_OP_SUPER_INVOKE_6,&& L_OP_SUPER_INVOKE_7,&& L_OP_SUPER_INVOKE_8,
        && L_OP_TAIL_SUPER_INVOKE_0,&& L_OP_TAIL_SUPER_INVOKE_1,&& L_OP_TAIL_SUPER_INVOKE_2,&& L_OP_TAIL_SUPER_INVOKE_3,&& L_OP_TAIL_SUPER_INVOKE_4,
        && L_OP_TAIL_SUPER_INVOKE_5,&& L_OP_TAIL_SUPER_INVOKE_6,&& L_OP_TAIL_SUPER_INVOKE_7,&& L_OP_TAIL_SUPER_INVOKE_8,
        && L_OP_SUPER_INVOKE,&& L_OP_SUPER_INVOKE_LONG,&& L_OP_TAIL_SUPER_INVOKE,&& L_OP_TAIL_SUPER_INVOKE_LONG,
        && L_OP_CLOSURE,&& L_OP_CLOSURE_LONG,&& L_OP_CLOSE_UPVALUE,&& L_OP_RETURN,
        && L_OP_CLASS,&& L_OP_CLASS_LONG,&& L_OP_INHERIT,&& L_OP_METHOD,&& L_OP_METHOD_LONG,
        && L_OP_BUILD_LIST,&& L_OP_INDEX_GET,&& L_OP_INDEX_SET,&& L_OP_IMPORT,&& L_OP_IMPORT_LONG
    };
#define DISPATCH() do { TRACE_IF_ENABLED(); goto *dispatchTable[*ip++]; } while (0)
#define OPCODE(name) L_##name
    DISPATCH();
#else
#define DISPATCH() break
#define OPCODE(name) case name
    while (true) {
        TRACE_IF_ENABLED();
        switch (READ_BYTE()) {
#endif

            /* --- STACK --- */
            OPCODE(OP_CONSTANT) : push(vm, READ_CONSTANT()); DISPATCH();
            OPCODE(OP_CONSTANT_LONG) : push(vm, READ_CONSTANT_LONG()); DISPATCH();
            OPCODE(OP_NIL) : push(vm, NIL_VAL); DISPATCH();
            OPCODE(OP_TRUE) : push(vm, BOOL_VAL(true)); DISPATCH();
            OPCODE(OP_FALSE) : push(vm, BOOL_VAL(false)); DISPATCH();
            OPCODE(OP_POP) : pop(vm); DISPATCH();
            OPCODE(OP_POP_N) : vm->stackTop -= READ_BYTE(); DISPATCH();
            OPCODE(OP_GET_LOCAL) : push(vm, frame->slots[READ_BYTE()]); DISPATCH();
            OPCODE(OP_SET_LOCAL) : frame->slots[READ_BYTE()] = peek(vm, 0); DISPATCH();

            /* --- GLOBALS --- */
            OPCODE(OP_GET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                Value val = frame->closure->function->module->globalValues.values[index];
                if (IS_EMPTY(val)) {
                    STORE_FRAME();
                    struct ObjString* n = findGlobalName(frame->closure->function->module, (int) index);
                    runtimeError(vm, "Undefined variable '%s'.", n ? n->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, val); DISPATCH();
            }
            OPCODE(OP_GET_GLOBAL_LONG) : {
                uint16_t index = READ_SHORT();
                Value val = frame->closure->function->module->globalValues.values[index];
                if (IS_EMPTY(val)) {
                    STORE_FRAME();
                    struct ObjString* n = findGlobalName(frame->closure->function->module, (int) index);
                    runtimeError(vm, "Undefined variable '%s'.", n ? n->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, val); DISPATCH();
            }

            OPCODE(OP_DEFINE_GLOBAL) : frame->closure->function->module->globalValues.values[READ_BYTE()] = pop(vm); DISPATCH();
            OPCODE(OP_DEFINE_GLOBAL_LONG) : frame->closure->function->module->globalValues.values[READ_SHORT()] = pop(vm); DISPATCH();

            OPCODE(OP_SET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                if (IS_EMPTY(frame->closure->function->module->globalValues.values[index])) {
                    STORE_FRAME();
                    struct ObjString* n = findGlobalName(frame->closure->function->module, (int) index);
                    runtimeError(vm, "Undefined variable '%s'.", n ? n->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame->closure->function->module->globalValues.values[index] = peek(vm, 0);
                DISPATCH();
            }
            OPCODE(OP_SET_GLOBAL_LONG) : {
                uint16_t index = READ_SHORT();
                if (IS_EMPTY(frame->closure->function->module->globalValues.values[index])) {
                    STORE_FRAME();
                    struct ObjString* n = findGlobalName(frame->closure->function->module, (int) index);
                    runtimeError(vm, "Undefined variable '%s'.", n ? n->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame->closure->function->module->globalValues.values[index] = peek(vm, 0);
                DISPATCH();
            }

            OPCODE(OP_GET_UPVALUE) : push(vm, *frame->closure->upvalues[READ_BYTE()]->location); DISPATCH();
            OPCODE(OP_SET_UPVALUE) : *frame->closure->upvalues[READ_BYTE()]->location = peek(vm, 0); DISPATCH();

            /* --- ACCESS --- */
            OPCODE(OP_GET_PROPERTY) : {
                ObjString* name = READ_STRING();
                Value receiver = peek(vm, 0);
                if (IS_INSTANCE(receiver)) {
                    ObjInstance* i = AS_INSTANCE(receiver); Value v;
                    if (tableGet(&i->fields, OBJ_VAL(name), &v)) {
                        pop(vm); push(vm, v); DISPATCH();
                    }
                    if (bindMethod(vm, i->klass, name)) DISPATCH();
                } else if (IS_MODULE(receiver)) {
                    ObjModule* m = AS_MODULE(receiver); Value idx;
                    if (tableGet(&m->globalNames, OBJ_VAL(name), &idx)) {
                        pop(vm); push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]); DISPATCH();
                    }
                    STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", name->chars); return INTERPRET_RUNTIME_ERROR;
                }
                STORE_FRAME(); runtimeError(vm, "Only instances and modules have properties."); return INTERPRET_RUNTIME_ERROR;
            }
            OPCODE(OP_GET_PROPERTY_LONG) : {
                ObjString* name = READ_STRING_LONG();
                Value receiver = peek(vm, 0);
                if (IS_INSTANCE(receiver)) {
                    ObjInstance* i = AS_INSTANCE(receiver); Value v;
                    if (tableGet(&i->fields, OBJ_VAL(name), &v)) {
                        pop(vm); push(vm, v); DISPATCH();
                    }
                    if (bindMethod(vm, i->klass, name)) DISPATCH();
                } else if (IS_MODULE(receiver)) {
                    ObjModule* m = AS_MODULE(receiver); Value idx;
                    if (tableGet(&m->globalNames, OBJ_VAL(name), &idx)) {
                        pop(vm); push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]); DISPATCH();
                    }
                    STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", name->chars); return INTERPRET_RUNTIME_ERROR;
                }
                STORE_FRAME(); runtimeError(vm, "Only instances and modules have properties."); return INTERPRET_RUNTIME_ERROR;
            }

            OPCODE(OP_SET_PROPERTY) : {
                ObjString* name = READ_STRING();
                Value val = pop(vm);
                if (IS_INSTANCE(peek(vm, 0))) {
                    ObjInstance* i = AS_INSTANCE(pop(vm));
                    tableSet(vm, &i->fields, OBJ_VAL(name), val);
                    push(vm, val);
                    DISPATCH();
                }
                STORE_FRAME(); runtimeError(vm, "Only instances have fields."); return INTERPRET_RUNTIME_ERROR;
            }
            OPCODE(OP_SET_PROPERTY_LONG) : {
                ObjString* name = READ_STRING_LONG();
                Value val = pop(vm);
                if (IS_INSTANCE(peek(vm, 0))) {
                    ObjInstance* i = AS_INSTANCE(pop(vm));
                    tableSet(vm, &i->fields, OBJ_VAL(name), val);
                    push(vm, val);
                    DISPATCH();
                }
                STORE_FRAME(); runtimeError(vm, "Only instances have fields."); return INTERPRET_RUNTIME_ERROR;
            }

            OPCODE(OP_GET_SUPER) : {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop(vm));
                STORE_FRAME();
                if (!bindMethod(vm, superclass, name)) return INTERPRET_RUNTIME_ERROR;
                DISPATCH();
            }
            OPCODE(OP_GET_SUPER_LONG) : {
                ObjString* name = READ_STRING_LONG();
                ObjClass* superclass = AS_CLASS(pop(vm));
                STORE_FRAME();
                if (!bindMethod(vm, superclass, name)) return INTERPRET_RUNTIME_ERROR;
                DISPATCH();
            }

            /* --- MATH --- */
            OPCODE(OP_EQUAL) : { Value b = pop(vm); Value a = pop(vm); push(vm, BOOL_VAL(valuesEqual(a, b))); DISPATCH(); }
            OPCODE(OP_GREATER) : { BINARY_OP(BOOL_VAL, > ); DISPATCH(); }
            OPCODE(OP_LESS) : { BINARY_OP(BOOL_VAL, < ); DISPATCH(); }
            OPCODE(OP_ADD) : {
                if (IS_STRING(peek(vm, 0)) || IS_STRING(peek(vm, 1))) {
                    if ((IS_STRING(peek(vm, 0)) || IS_NUMBER(peek(vm, 0))) &&
                        (IS_STRING(peek(vm, 1)) || IS_NUMBER(peek(vm, 1)))) {
                        STORE_FRAME(); concatenate(vm);
                    } else {
                        STORE_FRAME(); runtimeError(vm, "Operands must be two numbers or two strings."); return INTERPRET_RUNTIME_ERROR;
                    }
                } else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
                    double b = AS_NUMBER(pop(vm)); double a = AS_NUMBER(pop(vm)); push(vm, NUMBER_VAL(a + b));
                } else {
                    STORE_FRAME(); runtimeError(vm, "Operands must be two numbers or two strings."); return INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(OP_SUBTRACT) : { BINARY_OP(NUMBER_VAL, -); DISPATCH(); }
            OPCODE(OP_MULTIPLY) : { BINARY_OP(NUMBER_VAL, *); DISPATCH(); }
            OPCODE(OP_DIVIDE) : { BINARY_OP(NUMBER_VAL, / ); DISPATCH(); }
            OPCODE(OP_MODULO) : {
                if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) {
                    STORE_FRAME(); runtimeError(vm, "Operands must be numbers."); return INTERPRET_RUNTIME_ERROR;
                }
                double b = AS_NUMBER(pop(vm)); double a = AS_NUMBER(pop(vm));
                push(vm, NUMBER_VAL(fmod(a, b))); DISPATCH();
            }
            OPCODE(OP_NOT) : { push(vm, BOOL_VAL(isFalsey(pop(vm)))); DISPATCH(); }
            OPCODE(OP_NEGATE) : {
                if (!IS_NUMBER(peek(vm, 0))) {
                    STORE_FRAME(); runtimeError(vm, "Must be number."); return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm)))); DISPATCH();
            }

            /* --- FLOW --- */
            OPCODE(OP_PRINT) : { printValue(pop(vm)); printf("\n"); DISPATCH(); }
            OPCODE(OP_JUMP) : { uint16_t offset = READ_SHORT(); ip += offset; DISPATCH(); }
            OPCODE(OP_JUMP_IF_FALSE) : { uint16_t offset = READ_SHORT(); if (isFalsey(peek(vm, 0))) ip += offset; DISPATCH(); }
            OPCODE(OP_LOOP) : { uint16_t offset = READ_SHORT(); ip -= offset; DISPATCH(); }
            OPCODE(OP_RETURN) : {
                Value result = pop(vm);
                closeUpvalues(vm, frame->slots);
                vm->frameCount--;
                if (vm->frameCount == 0) {
                    pop(vm); return INTERPRET_OK;
                }
                vm->stackTop = frame->slots;
                push(vm, result);
                REFRESH_FRAME();
                DISPATCH();
            }

            /* --- CALLS --- */
            OPCODE(OP_CALL_0) : argCount = 0; goto do_call;
            OPCODE(OP_CALL_1) : argCount = 1; goto do_call;
            OPCODE(OP_CALL_2) : argCount = 2; goto do_call;
            OPCODE(OP_CALL_3) : argCount = 3; goto do_call;
            OPCODE(OP_CALL_4) : argCount = 4; goto do_call;
            OPCODE(OP_CALL_5) : argCount = 5; goto do_call;
            OPCODE(OP_CALL_6) : argCount = 6; goto do_call;
            OPCODE(OP_CALL_7) : argCount = 7; goto do_call;
            OPCODE(OP_CALL_8) : argCount = 8; goto do_call;
            OPCODE(OP_CALL) : argCount = READ_BYTE();
        do_call:
            STORE_FRAME();
            if (!callValue(vm, peek(vm, argCount), argCount)) return INTERPRET_RUNTIME_ERROR;
            REFRESH_FRAME();
            DISPATCH();

            OPCODE(OP_TAIL_CALL_0) : argCount = 0; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_1) : argCount = 1; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_2) : argCount = 2; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_3) : argCount = 3; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_4) : argCount = 4; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_5) : argCount = 5; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_6) : argCount = 6; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_7) : argCount = 7; goto do_tail_call;
            OPCODE(OP_TAIL_CALL_8) : argCount = 8; goto do_tail_call;
            OPCODE(OP_TAIL_CALL) : argCount = READ_BYTE();
        do_tail_call: {
            Value callee = peek(vm, argCount);
            if (!IS_CLOSURE(callee)) {
                STORE_FRAME(); if (!callValue(vm, callee, argCount)) return INTERPRET_RUNTIME_ERROR; REFRESH_FRAME(); DISPATCH();
            }
            ObjClosure* c = AS_CLOSURE(callee);
            if (argCount != c->function->arity) {
                STORE_FRAME(); runtimeError(vm, "Expected %d arguments but got %d.", c->function->arity, argCount); return INTERPRET_RUNTIME_ERROR;
            }
            closeUpvalues(vm, frame->slots);
            memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(Value) * (argCount + 1));
            vm->stackTop = frame->slots + (argCount + 1);
            frame->closure = c;
            frame->ip = c->function->chunk.code;
            ip = frame->ip;
            DISPATCH();
            }

        /* --- INVOKES --- */
        OPCODE(OP_INVOKE_0) : argCount = 0; goto do_invoke;
        OPCODE(OP_INVOKE_1) : argCount = 1; goto do_invoke;
        OPCODE(OP_INVOKE_2) : argCount = 2; goto do_invoke;
        OPCODE(OP_INVOKE_3) : argCount = 3; goto do_invoke;
        OPCODE(OP_INVOKE_4) : argCount = 4; goto do_invoke;
        OPCODE(OP_INVOKE_5) : argCount = 5; goto do_invoke;
        OPCODE(OP_INVOKE_6) : argCount = 6; goto do_invoke;
        OPCODE(OP_INVOKE_7) : argCount = 7; goto do_invoke;
        OPCODE(OP_INVOKE_8) : argCount = 8; goto do_invoke;
        OPCODE(OP_INVOKE) : argCount = READ_BYTE(); goto do_invoke;
    do_invoke: {
        ObjString* method = READ_STRING();
        STORE_FRAME();
        if (!invoke(vm, method, argCount)) return INTERPRET_RUNTIME_ERROR;
        REFRESH_FRAME();
        DISPATCH();
        }
    OPCODE(OP_INVOKE_LONG) : {
        ObjString* method = READ_STRING_LONG();
        argCount = READ_BYTE();
        STORE_FRAME();
        if (!invoke(vm, method, argCount)) return INTERPRET_RUNTIME_ERROR;
        REFRESH_FRAME();
        DISPATCH();
    }

    OPCODE(OP_TAIL_INVOKE_0) : argCount = 0; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_1) : argCount = 1; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_2) : argCount = 2; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_3) : argCount = 3; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_4) : argCount = 4; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_5) : argCount = 5; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_6) : argCount = 6; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_7) : argCount = 7; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE_8) : argCount = 8; goto do_tail_invoke;
    OPCODE(OP_TAIL_INVOKE) : argCount = READ_BYTE(); goto do_tail_invoke;
do_tail_invoke: {
    ObjString* method = READ_STRING();
    Value rec = peek(vm, argCount);
    if (!IS_INSTANCE(rec)) {
        STORE_FRAME(); runtimeError(vm, "Only instances have methods."); return INTERPRET_RUNTIME_ERROR;
    }
    ObjInstance* i = AS_INSTANCE(rec);
    Value meth;
    if (!tableGet(&i->klass->methods, OBJ_VAL(method), &meth)) {
        STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", method->chars); return INTERPRET_RUNTIME_ERROR;
    }
    ObjClosure* c = AS_CLOSURE(meth);
    closeUpvalues(vm, frame->slots);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(Value) * (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = c;
    frame->ip = c->function->chunk.code;
    ip = frame->ip;
    DISPATCH();
    }
OPCODE(OP_TAIL_INVOKE_LONG) : {
    ObjString* method = READ_STRING_LONG();
    argCount = READ_BYTE();
    Value rec = peek(vm, argCount);
    if (!IS_INSTANCE(rec)) {
        STORE_FRAME(); runtimeError(vm, "Only instances have methods."); return INTERPRET_RUNTIME_ERROR;
    }
    ObjInstance* i = AS_INSTANCE(rec);
    Value meth;
    if (!tableGet(&i->klass->methods, OBJ_VAL(method), &meth)) {
        STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", method->chars); return INTERPRET_RUNTIME_ERROR;
    }
    ObjClosure* c = AS_CLOSURE(meth);
    closeUpvalues(vm, frame->slots);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(Value)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = c;
    frame->ip = c->function->chunk.code;
    ip = frame->ip;
    DISPATCH();
}

/* --- SUPER --- */
OPCODE(OP_SUPER_INVOKE_0) : argCount = 0; goto do_super;
OPCODE(OP_SUPER_INVOKE_1) : argCount = 1; goto do_super;
OPCODE(OP_SUPER_INVOKE_2) : argCount = 2; goto do_super;
OPCODE(OP_SUPER_INVOKE_3) : argCount = 3; goto do_super;
OPCODE(OP_SUPER_INVOKE_4) : argCount = 4; goto do_super;
OPCODE(OP_SUPER_INVOKE_5) : argCount = 5; goto do_super;
OPCODE(OP_SUPER_INVOKE_6) : argCount = 6; goto do_super;
OPCODE(OP_SUPER_INVOKE_7) : argCount = 7; goto do_super;
OPCODE(OP_SUPER_INVOKE_8) : argCount = 8; goto do_super;
OPCODE(OP_SUPER_INVOKE) : argCount = READ_BYTE(); goto do_super;
do_super: {
ObjString* method = READ_STRING();
ObjClass* superclass = AS_CLASS(pop(vm));
STORE_FRAME();
if (!invokeFromClass(vm, superclass, method, argCount)) return INTERPRET_RUNTIME_ERROR;
REFRESH_FRAME();
DISPATCH();
}
OPCODE(OP_SUPER_INVOKE_LONG) : {
    ObjString* method = READ_STRING_LONG();
    argCount = READ_BYTE();
    ObjClass* superclass = AS_CLASS(pop(vm));
    STORE_FRAME();
    if (!invokeFromClass(vm, superclass, method, argCount)) return INTERPRET_RUNTIME_ERROR;
    REFRESH_FRAME();
    DISPATCH();
}

/* --- TAIL SUPER --- */
OPCODE(OP_TAIL_SUPER_INVOKE_0) : argCount = 0; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_1) : argCount = 1; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_2) : argCount = 2; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_3) : argCount = 3; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_4) : argCount = 4; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_5) : argCount = 5; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_6) : argCount = 6; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_7) : argCount = 7; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE_8) : argCount = 8; goto do_tail_super;
OPCODE(OP_TAIL_SUPER_INVOKE) : argCount = READ_BYTE(); goto do_tail_super;
do_tail_super: {
ObjString* m = READ_STRING();
ObjClass* s = AS_CLASS(pop(vm));
Value meth;
if (!tableGet(&s->methods, OBJ_VAL(m), &meth)) {
    STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", m->chars); return INTERPRET_RUNTIME_ERROR;
}
ObjClosure* c = AS_CLOSURE(meth);
closeUpvalues(vm, frame->slots);
memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(Value) * (argCount + 1));
vm->stackTop = frame->slots + (argCount + 1);
frame->closure = c;
frame->ip = c->function->chunk.code;
ip = frame->ip;
DISPATCH();
}
OPCODE(OP_TAIL_SUPER_INVOKE_LONG) : {
    ObjString* m = READ_STRING_LONG();
    argCount = READ_BYTE();
    ObjClass* s = AS_CLASS(pop(vm));
    Value meth;
    if (!tableGet(&s->methods, OBJ_VAL(m), &meth)) {
        STORE_FRAME(); runtimeError(vm, "Undefined property '%s'.", m->chars); return INTERPRET_RUNTIME_ERROR;
    }
    ObjClosure* c = AS_CLOSURE(meth);
    closeUpvalues(vm, frame->slots);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(Value)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = c;
    frame->ip = c->function->chunk.code;
    ip = frame->ip;
    DISPATCH();
}

/* --- OBJECTS --- */
OPCODE(OP_CLOSURE) : {
    ObjFunction* f = AS_FUNCTION(READ_CONSTANT());
    ObjClosure* c = newClosure(vm, f);
    push(vm, OBJ_VAL(c));
    for (int i = 0; i < f->upvalueCount; i++) {
        uint8_t isLocal = READ_BYTE();
        uint8_t index = READ_BYTE();
        c->upvalues[i] = isLocal ? captureUpvalue(vm, frame->slots + index)
            : frame->closure->upvalues[index];
    }
    DISPATCH();
}
OPCODE(OP_CLOSURE_LONG) : {
    ObjFunction* f = AS_FUNCTION(READ_CONSTANT_LONG());
    ObjClosure* c = newClosure(vm, f);
    push(vm, OBJ_VAL(c));
    for (int i = 0; i < f->upvalueCount; i++) {
        uint8_t isLocal = READ_BYTE();
        uint8_t index = READ_BYTE();
        c->upvalues[i] = isLocal ? captureUpvalue(vm, frame->slots + index)
            : frame->closure->upvalues[index];
    }
    DISPATCH();
}

OPCODE(OP_CLOSE_UPVALUE) : closeUpvalues(vm, vm->stackTop - 1); pop(vm); DISPATCH();

OPCODE(OP_CLASS) : push(vm, OBJ_VAL(newClass(vm, READ_STRING()))); DISPATCH();
OPCODE(OP_CLASS_LONG) : push(vm, OBJ_VAL(newClass(vm, READ_STRING_LONG()))); DISPATCH();

OPCODE(OP_INHERIT) : {
    Value sVal = peek(vm, 1);
    if (!IS_CLASS(sVal)) {
        STORE_FRAME(); runtimeError(vm, "Superclass must be a class."); return INTERPRET_RUNTIME_ERROR;
    }
    tableAddAll(vm, &AS_CLASS(sVal)->methods, &AS_CLASS(peek(vm, 0))->methods);
    pop(vm);
    DISPATCH();
}

OPCODE(OP_METHOD) : { ObjString* n = READ_STRING(); defineMethod(vm, AS_CLASS(peek(vm, 1)), n, peek(vm, 0)); pop(vm); DISPATCH(); }
OPCODE(OP_METHOD_LONG) : { ObjString* n = READ_STRING_LONG(); defineMethod(vm, AS_CLASS(peek(vm, 1)), n, peek(vm, 0)); pop(vm); DISPATCH(); }

OPCODE(OP_BUILD_LIST) : {
    uint8_t count = READ_BYTE(); ObjList* l = newList(vm); push(vm, OBJ_VAL(l));
    for (int i = 0; i < count; i++) writeValueArray(vm, &l->items, peek(vm, count - i));
    vm->stackTop -= (count + 1); push(vm, OBJ_VAL(l)); DISPATCH();
}
OPCODE(OP_INDEX_GET) : {
    Value iVal = pop(vm); Value lVal = pop(vm);
    if (!IS_LIST(lVal)) {
        STORE_FRAME(); runtimeError(vm, "Only lists indexed."); return INTERPRET_RUNTIME_ERROR;
    }
    ObjList* l = AS_LIST(lVal); int idx = (int) AS_NUMBER(iVal);
    if (idx < 0 || idx >= l->items.count) {
        STORE_FRAME(); runtimeError(vm, "Index out of bounds."); return INTERPRET_RUNTIME_ERROR;
    }
    push(vm, l->items.values[idx]); DISPATCH();
}
OPCODE(OP_INDEX_SET) : {
    Value v = pop(vm); Value iVal = pop(vm); Value lVal = pop(vm);
    if (!IS_LIST(lVal)) {
        STORE_FRAME(); runtimeError(vm, "Only lists indexed."); return INTERPRET_RUNTIME_ERROR;
    }
    ObjList* l = AS_LIST(lVal); int idx = (int) AS_NUMBER(iVal);
    if (idx < 0 || idx > l->items.count) {
        STORE_FRAME(); runtimeError(vm, "Index out of bounds."); return INTERPRET_RUNTIME_ERROR;
    }
    if (idx == l->items.count) writeValueArray(vm, &l->items, v); else l->items.values[idx] = v;
    push(vm, v); DISPATCH();
}

OPCODE(OP_IMPORT) : {
    ObjString* fName = READ_STRING();
    Value mVal;
    if (tableGet(&vm->modules, OBJ_VAL(fName), &mVal)) {
        push(vm, mVal); DISPATCH();
    }
    char* src = readFile(fName->chars);
    if (!src) {
        runtimeError(vm, "Could not open file \"%s\".", fName->chars);
        return INTERPRET_RUNTIME_ERROR;
    }
    STORE_FRAME();
    ObjModule* m = newModule(vm, fName);
    ObjFunction* f = compile(vm, m, src);
    free(src);
    if (!f) return INTERPRET_RUNTIME_ERROR;
    ObjClosure* c = newClosure(vm, f);
    push(vm, OBJ_VAL(c));
    if (!call(vm, c, 0)) return INTERPRET_RUNTIME_ERROR;
    vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
    tableSet(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));
    REFRESH_FRAME();
    DISPATCH();
}
OPCODE(OP_IMPORT_LONG) : {
    ObjString* fName = READ_STRING_LONG();
    Value mVal;
    if (tableGet(&vm->modules, OBJ_VAL(fName), &mVal)) {
        push(vm, mVal); DISPATCH();
    }
    char* src = readFile(fName->chars);
    if (!src) {
        runtimeError(vm, "Could not open file \"%s\".", fName->chars);
        return INTERPRET_RUNTIME_ERROR;
    }
    STORE_FRAME();
    ObjModule* m = newModule(vm, fName);
    ObjFunction* f = compile(vm, m, src);
    free(src);
    if (!f) return INTERPRET_RUNTIME_ERROR;
    ObjClosure* c = newClosure(vm, f);
    push(vm, OBJ_VAL(c));
    if (!call(vm, c, 0)) return INTERPRET_RUNTIME_ERROR;
    vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
    tableSet(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));
    REFRESH_FRAME();
    DISPATCH();
}

#ifndef HAS_COMPUTED_GOTOS
        }
    }
#endif
}

InterpretResult interpret(VM* vm, ObjModule* m, const char* src) {
    ObjFunction* f = compile(vm, m, src); if (f == NULL) return INTERPRET_COMPILE_ERROR;
    push(vm, OBJ_VAL(f)); ObjClosure* c = newClosure(vm, f); pop(vm); push(vm, OBJ_VAL(c));
    call(vm, c, 0); return run(vm);
}