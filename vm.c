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

static void resetStack(VM* vm) {
    vm->stackTop = vm->stack;
    vm->frameCount = 0;
    vm->openUpvalues = NULL;
    // HACK: Zeroing the stack ensures NaN tagging doesn't see garbage 
    // as object pointers during debug traces.
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
    runtimeError(vm, "Can only call functions and classes.");
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

static ObjString* valueToString(struct VM* vm, Value value) {
    if (IS_STRING(value)) {
        return AS_STRING(value);
    } else if (IS_NUMBER(value)) {
        char buf[32];
        // Use %g to match the expected output format of numbers
        int len = snprintf(buf, 32, "%g", AS_NUMBER(value));
        return copyString(vm, buf, len);
    } else if (IS_BOOL(value)) {
        const char* str = AS_BOOL(value) ? "true" : "false";
        return copyString(vm, str, AS_BOOL(value) ? 4 : 5);
    } else if (IS_NIL(value)) {
        return copyString(vm, "nil", 3);
    } else {
        // Fallback for objects (Lists, Classes, etc.)
        return copyString(vm, "<object>", 8);
    }
}

static void concatenate(struct VM* vm) {
    // 1. PEEK the values instead of popping. 
    // This keeps them as "Roots" so the GC won't kill them.
    Value bVal = peek(vm, 0);
    Value aVal = peek(vm, 1);

    // 2. Convert to strings. If these allocate, the originals are safe on stack.
    ObjString* a = valueToString(vm, aVal);
    push(vm, OBJ_VAL(a)); // Temporarily push a to protect it

    ObjString* b = valueToString(vm, bVal);
    push(vm, OBJ_VAL(b)); // Temporarily push b to protect it

    // 3. Now we have a, b on stack. Allocate the buffer.
    int length = a->length + b->length;
    char* chars = ALLOCATE(vm, char, length + 1);

    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(vm, chars, length);

    // 4. Clean up the stack: pop b_obj, a_obj, and the original bVal, aVal
    pop(vm); // pop b_obj
    pop(vm); // pop a_obj
    pop(vm); // pop original bVal
    pop(vm); // pop original aVal

    push(vm, OBJ_VAL(result));
}

#ifdef DEBUG_TRACE_EXECUTION
static void traceExecution(VM* vm) {
    // Always grab the freshest frame pointer directly from the VM
    CallFrame* frame = &vm->frames[vm->frameCount - 1];

    // Safety check: Don't trace if the frame is being torn down
    if (frame->closure == NULL || frame->closure->function == NULL) return;

    fprintf(stderr, "          ");
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        fprintf(stderr, "[ ");
        // If this crashes, your NaN Tagging IS_OBJ check is too aggressive
        // and is seeing a non-pointer as a pointer.
        printValueStderr(*slot);
        fprintf(stderr, " ]");
    }
    fprintf(stderr, "\n");

    // Use the IP we just synced in the DISPATCH macro
    int offset = (int) (frame->ip - frame->closure->function->chunk.code);

    // Final safety: don't disassemble if offset is out of bounds
    if (offset < 0 || offset >= frame->closure->function->chunk.count) {
        fprintf(stderr, "Invalid IP offset: %d\n", offset);
        return;
    }

    disassembleInstruction(&frame->closure->function->chunk, offset);
}
#endif

static InterpretResult run(VM* vm) {
    CallFrame* frame = &vm->frames[vm->frameCount - 1];
    // Use 'register' as a hint to the compiler
    register uint8_t* ip = frame->ip;

#define STORE_FRAME() (frame->ip = ip)
#define REFRESH_FRAME() (frame = &vm->frames[vm->frameCount - 1], ip = frame->ip)
#define READ_BYTE() (*ip++)
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(vType, op) do { \
    if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
            STORE_FRAME(); \
            runtimeError(vm, "Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(pop(vm)); \
        double a = AS_NUMBER(pop(vm)); \
        push(vm, vType(a op b)); \
  } while (false)

#ifdef HAS_COMPUTED_GOTOS
    static void* dispatchTable [] = {
      && L_OP_CONSTANT,&& L_OP_NIL,&& L_OP_TRUE,&& L_OP_FALSE,&& L_OP_POP,
      && L_OP_GET_LOCAL,&& L_OP_SET_LOCAL,&& L_OP_GET_GLOBAL,&& L_OP_DEFINE_GLOBAL,
      && L_OP_SET_GLOBAL,&& L_OP_GET_UPVALUE,&& L_OP_SET_UPVALUE,&& L_OP_GET_PROPERTY,
      && L_OP_SET_PROPERTY,&& L_OP_GET_SUPER,&& L_OP_EQUAL,&& L_OP_GREATER,&& L_OP_LESS,
      && L_OP_ADD,&& L_OP_SUBTRACT,&& L_OP_MULTIPLY,&& L_OP_DIVIDE,&& L_OP_MODULO,&& L_OP_NOT,
      && L_OP_NEGATE,&& L_OP_PRINT,&& L_OP_JUMP,&& L_OP_JUMP_IF_FALSE,&& L_OP_LOOP,
      && L_OP_CALL,&& L_OP_TAIL_CALL,
      && L_OP_INVOKE,&& L_OP_TAIL_INVOKE,&& L_OP_SUPER_INVOKE,&& L_OP_TAIL_SUPER_INVOKE,
      && L_OP_CLOSURE,&& L_OP_CLOSE_UPVALUE,&& L_OP_RETURN,
      && L_OP_CLASS,&& L_OP_INHERIT,&& L_OP_METHOD,
      && L_OP_BUILD_LIST,&& L_OP_INDEX_GET,&& L_OP_INDEX_SET,&& L_OP_IMPORT
    };

#ifdef DEBUG_TRACE_EXECUTION
#define DISPATCH() do { \
        frame->ip = ip; /* 1. Tell the VM exactly where we are RIGHT NOW */ \
        traceExecution(vm); /* 2. Trace based on that synced IP */ \
        uint8_t next_op = *ip++; /* 3. Fetch and move to next instruction */ \
        goto *dispatchTable[next_op]; /* 4. Jump */ \
    } while (0)
#else
#define DISPATCH() goto *dispatchTable[READ_BYTE()]
#endif

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
                uint8_t index = READ_BYTE();
                ObjModule* module = frame->closure->function->module;
                Value value = module->globalValues.values[index];
                if (IS_EMPTY(value)) {
                    STORE_FRAME();
                    // Use that function you already defined!
                    struct ObjString* name = findGlobalName(module, index);
                    runtimeError(vm, "Undefined variable '%s'.", name ? name->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, value);
                DISPATCH();}
            OPCODE(OP_DEFINE_GLOBAL) : {
                uint8_t index = READ_BYTE();
                // Use the current frame's module, not vm.globals
                ObjModule* module = frame->closure->function->module;
                module->globalValues.values[index] = pop(vm);
                DISPATCH(); }
            OPCODE(OP_SET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                ObjModule* module = frame->closure->function->module;
                if (IS_EMPTY(module->globalValues.values[index])) {
                    STORE_FRAME();
                    struct ObjString* name = findGlobalName(module, index);
                    runtimeError(vm, "Undefined variable '%s'.", name ? name->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                module->globalValues.values[index] = peek(vm, 0);
                DISPATCH();}
            OPCODE(OP_GET_UPVALUE) : {
                push(vm, *frame->closure->upvalues[READ_BYTE()]->location);
                DISPATCH(); }
            OPCODE(OP_SET_UPVALUE) : {
                *frame->closure->upvalues[READ_BYTE()]->location = peek(vm, 0);
                DISPATCH(); }
            OPCODE(OP_IMPORT) : {
                struct ObjString* filename = AS_STRING(READ_CONSTANT());
                Value mVal;
                if (tableGet(&vm->modules, filename, &mVal)) {
                    push(vm, mVal);
                    DISPATCH();
                }
                char* src = readFile(filename->chars);
                if (!src) {
                    runtimeError(vm, "Could not open file \"%s\".", filename->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                STORE_FRAME();
                ObjModule* m = newModule(vm, filename);
                ObjFunction* f = compile(vm, m, src);
                free(src);
                if (!f)
                    return INTERPRET_RUNTIME_ERROR;
                ObjClosure* c = newClosure(vm, f);
                // 1. Push the closure to the stack
                push(vm, OBJ_VAL(c));
                // 2. Call the closure (sets up the new frame)
                if (!call(vm, c, 0))
                    return INTERPRET_RUNTIME_ERROR;
                // 3. FIX: Replace slot 0 of the NEW frame with the Module object.
                // Because we updated endCompiler, the module will now return 'm' when it finishes.
                vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
                // 4. Cache the module
                tableSet(vm, &vm->modules, filename, OBJ_VAL(m));
                REFRESH_FRAME();
                DISPATCH();}
            OPCODE(OP_GET_PROPERTY) : {
                ObjString* name = READ_STRING();
                Value receiver = peek(vm, 0);
                if (IS_INSTANCE(receiver)) {
                    ObjInstance* instance = AS_INSTANCE(receiver);
                    Value value;
                    if (tableGet(&instance->fields, name, &value)) {
                        pop(vm); // instance
                        push(vm, value);
                        DISPATCH();
                    }
                    if (bindMethod(vm, instance->klass, name)) {
                        DISPATCH();
                    }
                    STORE_FRAME();
                    runtimeError(vm, "Undefined property '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (IS_MODULE(receiver)) {
                    ObjModule* module = AS_MODULE(receiver);
                    Value indexValue;
                    if (tableGet(&module->globalNames, name, &indexValue)) {
                        int index = (int) AS_NUMBER(indexValue);
                        pop(vm); // module
                        push(vm, module->globalValues.values[index]);
                        DISPATCH();
                    }
                    STORE_FRAME();
                    runtimeError(vm, "Undefined property '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                STORE_FRAME();
                runtimeError(vm, "Only instances and modules have properties.");
                return INTERPRET_RUNTIME_ERROR;}
            OPCODE(OP_BUILD_LIST) : {
                uint8_t count = READ_BYTE();
                ObjList* list = newList(vm);
                // 1. Push list to stack immediately so the GC sees it!
                push(vm, OBJ_VAL(list));
                // 2. Fill it
                for (int i = 0; i < count; i++) {
                    // Peek past the list object we just pushed
                    Value item = peek(vm, count - i);
                    writeValueArray(vm, &list->items, item);
                }
                // 3. Pop the items and the temporary list object
                vm->stackTop -= (count + 1);
                // 4. Push the final list back
                push(vm, OBJ_VAL(list));
                DISPATCH();}
            OPCODE(OP_INDEX_GET) : {
                Value indexValue = pop(vm);
                Value listValue = pop(vm);
                if (!IS_LIST(listValue)) {
                    STORE_FRAME();
                    runtimeError(vm, "Only lists can be indexed.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* list = AS_LIST(listValue);
                int index = (int) AS_NUMBER(indexValue);
                if (index < 0 || index >= list->items.count) {
                    STORE_FRAME();
                    runtimeError(vm, "Index out of bounds.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, list->items.values[index]);
                DISPATCH();}
            OPCODE(OP_INDEX_SET) : {
                Value value = pop(vm);
                Value indexValue = pop(vm);
                Value listValue = pop(vm);
                if (!IS_LIST(listValue)) {
                    STORE_FRAME();
                    runtimeError(vm, "Only lists can be indexed.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* list = AS_LIST(listValue);
                if (!IS_NUMBER(indexValue)) {
                    STORE_FRAME();
                    runtimeError(vm, "List index must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int index = (int) AS_NUMBER(indexValue);
                if (index < 0 || index > list->items.count) {
                    STORE_FRAME();
                    runtimeError(vm, "Index out of bounds.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (index == list->items.count) {
                    // Appending to the end of the list: res[length] = val
                    writeValueArray(vm, &list->items, value);
                } else {
                    // Overwriting an existing element
                    list->items.values[index] = value;
                }
                // Assignment expressions evaluate to the value assigned
                push(vm, value);
                DISPATCH();}
            OPCODE(OP_RETURN) : {
                Value res = pop(vm); closeUpvalues(vm, frame->slots); vm->frameCount--;
                if (vm->frameCount == 0) {
                    pop(vm); return INTERPRET_OK;
                }
                vm->stackTop = frame->slots; push(vm, res);
                REFRESH_FRAME();
                DISPATCH();}
            OPCODE(OP_ADD) : {
                if (IS_STRING(peek(vm, 0)) || IS_STRING(peek(vm, 1))) {
                    // Concatenation: Allow String + String, String + Number, or Number + String
                    if ((IS_STRING(peek(vm, 0)) || IS_NUMBER(peek(vm, 0))) &&
                        (IS_STRING(peek(vm, 1)) || IS_NUMBER(peek(vm, 1)))) {
                        STORE_FRAME();
                        concatenate(vm);
                    } else {
                        STORE_FRAME();
                        runtimeError(vm, "Operands must be two numbers or two strings.");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                } else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
                    // Standard Addition
                    double b = AS_NUMBER(pop(vm));
                    double a = AS_NUMBER(pop(vm));
                    push(vm, NUMBER_VAL(a + b));
                } else {
                    STORE_FRAME();
                    runtimeError(vm, "Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH(); }
            OPCODE(OP_EQUAL) : { Value b = pop(vm); Value a = pop(vm); push(vm, BOOL_VAL(valuesEqual(a, b))); DISPATCH(); }
            OPCODE(OP_GREATER) : {
                BINARY_OP(BOOL_VAL, > ); DISPATCH(); }
            OPCODE(OP_LESS) : {
                BINARY_OP(BOOL_VAL, < ); DISPATCH(); }
            OPCODE(OP_SUBTRACT) : {
                BINARY_OP(NUMBER_VAL, -); DISPATCH(); }
            OPCODE(OP_MULTIPLY) : {
                BINARY_OP(NUMBER_VAL, *); DISPATCH(); }
            OPCODE(OP_DIVIDE) : {
                BINARY_OP(NUMBER_VAL, / ); DISPATCH(); }
            OPCODE(OP_MODULO) : {
                double b = AS_NUMBER(pop(vm));
                double a = AS_NUMBER(pop(vm));
                push(vm, NUMBER_VAL(fmod(a, b)));
                DISPATCH();}
            OPCODE(OP_NOT) : {
                Value v = pop(vm); push(vm, BOOL_VAL(IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v)))); DISPATCH(); }
            OPCODE(OP_NEGATE) : {
                if (!IS_NUMBER(peek(vm, 0))) {
                    STORE_FRAME();
                    runtimeError(vm, "Must be number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
                DISPATCH(); }
            OPCODE(OP_PRINT) : {
                printValue(pop(vm));
                printf("\n");
                DISPATCH(); }
            OPCODE(OP_JUMP) : {
                uint16_t offset = READ_SHORT();
                ip += offset;
                DISPATCH();}
            OPCODE(OP_JUMP_IF_FALSE) : {
                int o = READ_SHORT();
                Value v = peek(vm, 0);
                if (IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v)))
                    ip += o;
                DISPATCH(); }
            OPCODE(OP_LOOP) : {
                uint16_t offset = READ_SHORT();
                ip -= offset;
                DISPATCH();}
            OPCODE(OP_CALL) : {
                int a = READ_BYTE();
                STORE_FRAME();
                if (!callValue(vm, peek(vm, a), a)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH(); }
            OPCODE(OP_TAIL_CALL) : {
                int argCount = READ_BYTE();
                Value callee = peek(vm, argCount);
                // We can only tail-call closures
                // Natives don't have a bytecode frame to recycle.
                if (!IS_CLOSURE(callee)) {
                    STORE_FRAME();
                    // Fallback to a normal call logic or error
                    if (!callValue(vm, callee, argCount)) {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    REFRESH_FRAME();
                    DISPATCH();
                }
                ObjClosure* closure = AS_CLOSURE(callee);
                if (argCount != closure->function->arity) {
                    STORE_FRAME();
                    runtimeError(vm, "Expected %d arguments but got %d.",
                        closure->function->arity, argCount);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // --- RECYCLE THE FRAME ---
                frame = &vm->frames[vm->frameCount - 1];
                // 1. VERY IMPORTANT: Close any upvalues in the current frame.
                // If we overwrite these slots without closing, we break closures.
                closeUpvalues(vm, frame->slots);
                // 2. Shift the new function and its arguments down the stack
                // into the slots used by the current function.
                // Use memmove because the memory regions overlap.
                int totalSlotsToMove = argCount + 1; // callee + args
                memmove(frame->slots, vm->stackTop - totalSlotsToMove, sizeof(Value)* totalSlotsToMove);
                // 3. Reset the stack top to just after our new shifted arguments.
                vm->stackTop = frame->slots + totalSlotsToMove;
                // 4. Update the frame pointers to the new closure.
                frame->closure = closure;
                frame->ip = closure->function->chunk.code;
                ip = frame->ip; // Load the new IP for the next instruction
                // Notice: We DO NOT increment vm->frameCount.
                DISPATCH();}
            OPCODE(OP_INVOKE) : {
                struct ObjString* m = AS_STRING(READ_CONSTANT());
                int a = READ_BYTE();
                STORE_FRAME();
                if (!invoke(vm, m, a)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH(); }
            OPCODE(OP_TAIL_INVOKE) : {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                // 1. Find the receiver
                Value receiver = peek(vm, argCount);
                if (!IS_INSTANCE(receiver)) {
                    STORE_FRAME();
                    runtimeError(vm, "Only instances have methods.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(receiver);
                // 2. Look up the method
                Value methodVal;
                if (!tableGet(&instance->klass->methods, method, &methodVal)) {
                    STORE_FRAME();
                    runtimeError(vm, "Undefined property '%s'.", method->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // We can only tail-call closures/methods
                ObjClosure* closure = AS_CLOSURE(methodVal);
                // 3. Check Arity
                if (argCount != closure->function->arity) {
                    STORE_FRAME();
                    runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // 4. --- RECYCLE THE FRAME (Same as OP_TAIL_CALL) ---
                frame = &vm->frames[vm->frameCount - 1]; // Use existing frame variable
                closeUpvalues(vm, frame->slots);
                int totalSlotsToMove = argCount + 1;
                memmove(frame->slots, vm->stackTop - totalSlotsToMove, sizeof(Value)* totalSlotsToMove);
                vm->stackTop = frame->slots + totalSlotsToMove;
                frame->closure = closure;
                frame->ip = closure->function->chunk.code;
                ip = frame->ip;
                DISPATCH();}
            OPCODE(OP_CLOSURE) : {
                ObjFunction* f = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* c = newClosure(vm, f);
                push(vm, OBJ_VAL(c));
                for (int i = 0; i < f->upvalueCount; i++) {
                    uint8_t isL = READ_BYTE(); uint8_t idx = READ_BYTE(); if (isL) c->upvalues[i] = captureUpvalue(vm, frame->slots + idx); else c->upvalues[i] = frame->closure->upvalues[idx];
                }
                DISPATCH(); }
            OPCODE(OP_CLOSE_UPVALUE) : {
                closeUpvalues(vm, vm->stackTop - 1);
                pop(vm);
                DISPATCH(); }
            OPCODE(OP_CLASS) : {
                push(vm, OBJ_VAL(newClass(vm, AS_STRING(READ_CONSTANT()))));
                DISPATCH(); }
            OPCODE(OP_INHERIT) : {
                Value superclass = peek(vm, 1);
                ObjClass* subclass = AS_CLASS(peek(vm, 0));
                if (!IS_CLASS(superclass)) {
                    STORE_FRAME();
                    runtimeError(vm, "Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                tableAddAll(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
                pop(vm);
                DISPATCH();}
            OPCODE(OP_METHOD) : {
                ObjString* name = READ_STRING(); // Get method name from constants
                Value method = peek(vm, 0);          // Get closure from stack top
                ObjClass* klass = AS_CLASS(peek(vm, 1)); // Get class from below closure
                tableSet(vm, &klass->methods, name, method);
                pop(vm); // Pop the closure
                DISPATCH(); }
            OPCODE(OP_SET_PROPERTY) : {
                Value val = pop(vm);
                if (!IS_INSTANCE(peek(vm, 0))) {
                    STORE_FRAME();
                    runtimeError(vm, "Only instances have fields."); return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* i = AS_INSTANCE(pop(vm));
                tableSet(vm, &i->fields, AS_STRING(READ_CONSTANT()), val);
                push(vm, val);
                DISPATCH();}
            OPCODE(OP_GET_SUPER) : {
                struct ObjString* n = AS_STRING(READ_CONSTANT());
                ObjClass* s = AS_CLASS(pop(vm));
                STORE_FRAME();
                if (!bindMethod(vm, s, n))
                    return INTERPRET_RUNTIME_ERROR;
                DISPATCH();}
            OPCODE(OP_SUPER_INVOKE) : {
                struct ObjString* m = AS_STRING(READ_CONSTANT());
                int a = READ_BYTE();
                ObjClass* s = AS_CLASS(pop(vm));
                STORE_FRAME();
                if (!invokeFromClass(vm, s, m, a))
                    return INTERPRET_RUNTIME_ERROR;
                REFRESH_FRAME();
                DISPATCH();}
            OPCODE(OP_TAIL_SUPER_INVOKE) : {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop(vm)); // Superclass is pushed by the compiler
                Value methodVal;
                if (!tableGet(&superclass->methods, method, &methodVal)) {
                    STORE_FRAME();
                    runtimeError(vm, "Undefined property '%s'.", method->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjClosure* closure = AS_CLOSURE(methodVal);
                if (argCount != closure->function->arity) {
                    STORE_FRAME();
                    runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // --- RECYCLE THE FRAME ---
                frame = &vm->frames[vm->frameCount - 1];
                closeUpvalues(vm, frame->slots);
                int totalSlotsToMove = argCount + 1;
                memmove(frame->slots, vm->stackTop - totalSlotsToMove, sizeof(Value)* totalSlotsToMove);
                vm->stackTop = frame->slots + totalSlotsToMove;
                frame->closure = closure;
                frame->ip = closure->function->chunk.code;
                ip = frame->ip;
                DISPATCH();}

#ifndef HAS_COMPUTED_GOTOS
        }
    }
#endif
    }

InterpretResult interpret(VM* vm, ObjModule* m, const char* src) {
    ObjFunction* f = compile(vm, m, src);
    if (f == NULL)
        return INTERPRET_COMPILE_ERROR;
    push(vm, OBJ_VAL(f));
    ObjClosure* c = newClosure(vm, f);
    pop(vm);
    push(vm, OBJ_VAL(c));
    call(vm, c, 0);
    return run(vm);
}