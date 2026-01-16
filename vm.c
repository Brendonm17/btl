#include <stdarg.h>
#include <stdio.h>
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
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack(vm);
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

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) return NULL;

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*) malloc(fileSize + 1);
    if (buffer == NULL) return NULL;

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        free(buffer);
        fclose(file);
        return NULL;
    }
    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static Value clockNative(int argCount, Value* args) {
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

    // Initialize the primary module
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
    *vm->stackTop = value;
    vm->stackTop++;
}

Value pop(VM* vm) {
    vm->stackTop--;
    return *vm->stackTop;
}

static Value peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate(VM* vm) {
    struct ObjString* b = AS_STRING(peek(vm, 0));
    struct ObjString* a = AS_STRING(peek(vm, 1));

    int length = a->length + b->length;
    char* chars = ALLOCATE(vm, char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    struct ObjString* result = takeString(vm, chars, length);
    pop(vm);
    pop(vm);
    push(vm, OBJ_VAL(result));
}

static bool call(VM* vm, ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm->frameCount == FRAMES_MAX) {
        runtimeError(vm, "Stack overflow.");
        return false;
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
            } else if (argCount != 0) {
                runtimeError(vm, "Expected 0 arguments but got %d.", argCount);
                return false;
            }
            return true;
        }
        case OBJ_CLOSURE:
            return call(vm, AS_CLOSURE(callee), argCount);
        case OBJ_NATIVE: {
            NativeFn native = AS_NATIVE(callee);
            Value result = native(argCount, vm->stackTop - argCount);
            vm->stackTop -= argCount + 1;
            push(vm, result);
            return true;
        }
        default:
            break;
        }
    }

    runtimeError(vm, "Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(VM* vm, ObjClass* klass, struct ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;
    }
    return call(vm, AS_CLOSURE(method), argCount);
}

static bool invoke(VM* vm, struct ObjString* name, int argCount) {
    Value receiver = peek(vm, argCount);

    if (!IS_OBJ(receiver)) {
        runtimeError(vm, "Only objects have methods.");
        return false;
    }

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);

        Value value;
        if (tableGet(&instance->fields, name, &value)) {
            vm->stackTop[-argCount - 1] = value;
            return callValue(vm, value, argCount);
        }

        return invokeFromClass(vm, instance->klass, name, argCount);
    }

    if (IS_MODULE(receiver)) {
        ObjModule* module = AS_MODULE(receiver);
        Value indexVal;
        if (tableGet(&module->globalNames, name, &indexVal)) {
            int idx = (int) AS_NUMBER(indexVal);
            vm->stackTop[-argCount - 1] = module->globalValues.values[idx];
            return callValue(vm, module->globalValues.values[idx], argCount);
        }
        runtimeError(vm, "Module has no export '%s'.", name->chars);
        return false;
    }

    runtimeError(vm, "Only instances and modules have methods.");
    return false;
}

static bool bindMethod(VM* vm, ObjClass* klass, struct ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(vm, peek(vm, 0), AS_CLOSURE(method));
    pop(vm);
    push(vm, OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(VM* vm, Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm->openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(vm, local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm->openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(VM* vm, Value* last) {
    while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm->openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->openUpvalues = upvalue->next;
    }
}

static InterpretResult run(VM* vm) {
    CallFrame* frame = &vm->frames[vm->frameCount - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())

#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
        runtimeError(vm, "Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop(vm)); \
      double a = AS_NUMBER(pop(vm)); \
      push(vm, valueType(a op b)); \
    } while (false)

    /* --- DISPATCH MACROS --- */
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

            OPCODE(OP_CONSTANT) : {
                Value constant = READ_CONSTANT();
                push(vm, constant);
                DISPATCH();
            }
            OPCODE(OP_NIL) : push(vm, NIL_VAL); DISPATCH();
            OPCODE(OP_TRUE) : push(vm, BOOL_VAL(true)); DISPATCH();
            OPCODE(OP_FALSE) : push(vm, BOOL_VAL(false)); DISPATCH();
            OPCODE(OP_POP) : pop(vm); DISPATCH();

            OPCODE(OP_GET_LOCAL) : {
                uint8_t slot = READ_BYTE();
                push(vm, frame->slots[slot]);
                DISPATCH();
            }

            OPCODE(OP_SET_LOCAL) : {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(vm, 0);
                DISPATCH();
            }

            OPCODE(OP_GET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                ObjModule* module = frame->closure->function->module;
                Value value = module->globalValues.values[index];
                if (IS_EMPTY(value)) {
                    struct ObjString* name = findGlobalName(module, index);
                    runtimeError(vm, "Undefined variable '%s'.", name ? name->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, value);
                DISPATCH();
            }

            OPCODE(OP_DEFINE_GLOBAL) : {
                uint8_t index = READ_BYTE();
                ObjModule* module = frame->closure->function->module;
                module->globalValues.values[index] = peek(vm, 0);
                pop(vm);
                DISPATCH();
            }

            OPCODE(OP_SET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                ObjModule* module = frame->closure->function->module;
                if (IS_EMPTY(module->globalValues.values[index])) {
                    struct ObjString* name = findGlobalName(module, index);
                    runtimeError(vm, "Undefined variable '%s'.", name ? name->chars : "unknown");
                    return INTERPRET_RUNTIME_ERROR;
                }
                module->globalValues.values[index] = peek(vm, 0);
                DISPATCH();
            }

            OPCODE(OP_GET_UPVALUE) : {
                uint8_t slot = READ_BYTE();
                push(vm, *frame->closure->upvalues[slot]->location);
                DISPATCH();
            }

            OPCODE(OP_SET_UPVALUE) : {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(vm, 0);
                DISPATCH();
            }

            OPCODE(OP_IMPORT) : {
                struct ObjString* filename = AS_STRING(READ_CONSTANT());
                Value moduleValue;

                if (tableGet(&vm->modules, filename, &moduleValue)) {
                    push(vm, moduleValue);
                    DISPATCH();
                }

                char* source = readFile(filename->chars);
                if (source == NULL) {
                    runtimeError(vm, "Could not load file '%s'.", filename->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjModule* module = newModule(vm, filename);
                push(vm, OBJ_VAL(module));

                ObjFunction* function = compile(vm, module, source);
                free(source);

                if (function == NULL) return INTERPRET_RUNTIME_ERROR;
                push(vm, OBJ_VAL(function));
                ObjClosure* closure = newClosure(vm, function);
                pop(vm);
                push(vm, OBJ_VAL(closure));

                if (!call(vm, closure, 0)) return INTERPRET_RUNTIME_ERROR;

                tableSet(vm, &vm->modules, filename, OBJ_VAL(module));
                pop(vm); // remove safety module ptr
                push(vm, OBJ_VAL(module));

                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_GET_PROPERTY) : {
                Value receiver = peek(vm, 0);
                struct ObjString* name = AS_STRING(READ_CONSTANT());

                if (IS_INSTANCE(receiver)) {
                    ObjInstance* instance = AS_INSTANCE(receiver);
                    Value value;
                    if (tableGet(&instance->fields, name, &value)) {
                        pop(vm);
                        push(vm, value);
                        DISPATCH();
                    }
                    if (bindMethod(vm, instance->klass, name)) {
                        frame = &vm->frames[vm->frameCount - 1];
                        DISPATCH();
                    }
                } else if (IS_MODULE(receiver)) {
                    ObjModule* module = AS_MODULE(receiver);
                    Value indexVal;
                    if (tableGet(&module->globalNames, name, &indexVal)) {
                        int idx = (int) AS_NUMBER(indexVal);
                        pop(vm);
                        push(vm, module->globalValues.values[idx]);
                        DISPATCH();
                    }
                }
                runtimeError(vm, "Property error.");
                return INTERPRET_RUNTIME_ERROR;
            }

            OPCODE(OP_SET_PROPERTY) : {
                Value val = pop(vm);
                if (!IS_INSTANCE(peek(vm, 0))) {
                    runtimeError(vm, "Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(pop(vm));
                tableSet(vm, &instance->fields, AS_STRING(READ_CONSTANT()), val);
                push(vm, val);
                DISPATCH();
            }

            OPCODE(OP_GET_SUPER) : {
                struct ObjString* name = AS_STRING(READ_CONSTANT());
                ObjClass* superclass = AS_CLASS(pop(vm));
                if (!bindMethod(vm, superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_EQUAL) : {
                Value b = pop(vm);
                Value a = pop(vm);
                push(vm, BOOL_VAL(valuesEqual(a, b)));
                DISPATCH();
            }

            OPCODE(OP_BUILD_LIST) : {
                int itemCount = READ_BYTE();
                ObjList* list = newList(vm);
                push(vm, OBJ_VAL(list));
                for (int i = 0; i < itemCount; i++) {
                    writeValueArray(vm, &list->items, peek(vm, itemCount - i + 1));
                }
                vm->stackTop -= (itemCount + 1);
                push(vm, OBJ_VAL(list));
                DISPATCH();
            }

            OPCODE(OP_INDEX_GET) : {
                Value indexVal = pop(vm);
                Value listVal = pop(vm);
                if (!IS_LIST(listVal)) {
                    runtimeError(vm, "Can only index lists.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* list = AS_LIST(listVal);
                if (!IS_NUMBER(indexVal)) {
                    runtimeError(vm, "Index must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int index = (int) AS_NUMBER(indexVal);
                if (index < 0 || index >= list->items.count) {
                    runtimeError(vm, "List index out of bounds.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, list->items.values[index]);
                DISPATCH();
            }

            OPCODE(OP_INDEX_SET) : {
                Value value = pop(vm);
                Value indexVal = pop(vm);
                Value listVal = pop(vm);
                if (!IS_LIST(listVal)) {
                    runtimeError(vm, "Can only index lists.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* list = AS_LIST(listVal);
                if (!IS_NUMBER(indexVal)) {
                    runtimeError(vm, "Index must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int index = (int) AS_NUMBER(indexVal);
                if (index < 0 || index >= list->items.count) {
                    runtimeError(vm, "List index out of bounds.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                list->items.values[index] = value;
                push(vm, value);
                DISPATCH();
            }

            OPCODE(OP_GREATER) : {
                double b = AS_NUMBER(pop(vm));
                double a = AS_NUMBER(pop(vm));
                push(vm, BOOL_VAL(a > b));
                DISPATCH();
            }
            OPCODE(OP_LESS) : {
                double b = AS_NUMBER(pop(vm));
                double a = AS_NUMBER(pop(vm));
                push(vm, BOOL_VAL(a < b));
                DISPATCH();
            }
            OPCODE(OP_ADD) : {
                if (IS_STRING(peek(vm, 0)) && IS_STRING(peek(vm, 1))) {
                    concatenate(vm);
                } else if (IS_NUMBER(peek(vm, 0)) && IS_NUMBER(peek(vm, 1))) {
                    double b = AS_NUMBER(pop(vm));
                    double a = AS_NUMBER(pop(vm));
                    push(vm, NUMBER_VAL(a + b));
                } else {
                    runtimeError(vm, "Operands must be numbers or strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(OP_SUBTRACT) : BINARY_OP(NUMBER_VAL, -); DISPATCH();
            OPCODE(OP_MULTIPLY) : BINARY_OP(NUMBER_VAL, *); DISPATCH();
            OPCODE(OP_DIVIDE) : BINARY_OP(NUMBER_VAL, / ); DISPATCH();

            OPCODE(OP_NOT) : {
                push(vm, BOOL_VAL(isFalsey(pop(vm))));
                DISPATCH();
            }

            OPCODE(OP_NEGATE) : {
                if (!IS_NUMBER(peek(vm, 0))) {
                    runtimeError(vm, "Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
                DISPATCH();
            }

            OPCODE(OP_PRINT) : {
                printValue(pop(vm));
                printf("\n");
                DISPATCH();
            }

            OPCODE(OP_JUMP) : {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                DISPATCH();
            }

            OPCODE(OP_JUMP_IF_FALSE) : {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(vm, 0))) frame->ip += offset;
                DISPATCH();
            }

            OPCODE(OP_LOOP) : {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                DISPATCH();
            }

            OPCODE(OP_CALL) : {
                int argCount = READ_BYTE();
                if (!callValue(vm, peek(vm, argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_INVOKE) : {
                struct ObjString* method = AS_STRING(READ_CONSTANT());
                int argCount = READ_BYTE();
                if (!invoke(vm, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_SUPER_INVOKE) : {
                struct ObjString* method = AS_STRING(READ_CONSTANT());
                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop(vm));
                if (!invokeFromClass(vm, superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_CLOSURE) : {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(vm, function);
                push(vm, OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(vm, frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                DISPATCH();
            }

            OPCODE(OP_CLOSE_UPVALUE) : {
                closeUpvalues(vm, vm->stackTop - 1);
                pop(vm);
                DISPATCH();
            }

            OPCODE(OP_RETURN) : {
                Value result = pop(vm);
                closeUpvalues(vm, frame->slots);
                vm->frameCount--;
                if (vm->frameCount == 0) {
                    pop(vm);
                    return INTERPRET_OK;
                }

                vm->stackTop = frame->slots;
                push(vm, result);
                frame = &vm->frames[vm->frameCount - 1];
                DISPATCH();
            }

            OPCODE(OP_CLASS) : {
                push(vm, OBJ_VAL(newClass(vm, AS_STRING(READ_CONSTANT()))));
                DISPATCH();
            }

            OPCODE(OP_INHERIT) : {
                Value superclass = peek(vm, 1);
                if (!IS_CLASS(superclass)) {
                    runtimeError(vm, "Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjClass* subclass = AS_CLASS(peek(vm, 0));
                tableAddAll(vm, &AS_CLASS(superclass)->methods, &subclass->methods);
                pop(vm);
                DISPATCH();
            }

            OPCODE(OP_METHOD) : {
                struct ObjString* name = AS_STRING(READ_CONSTANT());
                ObjClass* klass = AS_CLASS(peek(vm, 1));
                tableSet(vm, &klass->methods, name, peek(vm, 0));
                pop(vm);
                DISPATCH();
            }

#ifndef HAS_COMPUTED_GOTOS
        }
    }
#endif

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
#undef DISPATCH
#undef OPCODE
}

InterpretResult interpret(VM* vm, struct ObjModule* module, const char* source) {
    ObjFunction* function = compile(vm, module, source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(vm, OBJ_VAL(function));
    ObjClosure* closure = newClosure(vm, function);
    pop(vm);
    push(vm, OBJ_VAL(closure));
    call(vm, closure, 0);

    return run(vm);
}