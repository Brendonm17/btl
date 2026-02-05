/*
 * BTL Compiled Support Library v2 â€” Implementation
 *
 * Each function here corresponds to a complex opcode that's too big
 * to inline in the generated code. The logic is extracted directly
 * from vm.c's dispatch cases.
 *
 * IMPORTANT: All functions here operate on vm->stackTop. The
 * generated code must sync its local 'sp' â†’ vm->stackTop before
 * calling any of these, and reload sp afterward.
 */

#include "compiled.h"
#include <stdio.h>

 /* ============================================================================
  * String helpers (extracted from vm.c)
  * ============================================================================ */

static ObjString* valueToStringCompiled(VM* vm, Value value) {
    if (IS_STRING(value)) return AS_STRING(value);
    char buf[32];
    if (IS_NUMBER(value)) {
        int len = snprintf(buf, 32, "%g", AS_NUMBER(value));
        return copyString(vm, buf, len);
    }
    if (IS_BOOL(value)) return copyString(vm, AS_BOOL(value) ? "true" : "false", AS_BOOL(value) ? 4 : 5);
    if (IS_NULL(value)) return copyString(vm, "null", 3);
    return copyString(vm, "<object>", 8);
}

/* ============================================================================
 * OP_ADD (slow path â€” string concatenation)
 *
 * The generated code inlines the number fast path. This is only called
 * when at least one operand is a string.
 * ============================================================================ */

bool btl_compiled_add(VM* vm) {
    Value b = vm->stackTop[-1];
    Value a = vm->stackTop[-2];

    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        vm->stackTop -= 2;
        push(vm, NUMBER_VAL(AS_NUMBER(a) + AS_NUMBER(b)));
        return true;
    }

    if (IS_STRING(a) || IS_STRING(b)) {
        if ((IS_STRING(a) || IS_NUMBER(a)) && (IS_STRING(b) || IS_NUMBER(b))) {
            ObjString* sa = valueToStringCompiled(vm, a); push(vm, OBJ_VAL(sa));
            ObjString* sb = valueToStringCompiled(vm, b); push(vm, OBJ_VAL(sb));
            int length = sa->length + sb->length;
            char* chars = ALLOCATE(vm, char, length + 1);
            memcpy(chars, sa->chars, sa->length);
            memcpy(chars + sa->length, sb->chars, sb->length);
            chars[length] = '\0';
            ObjString* result = takeString(vm, chars, length);
            pop(vm); pop(vm); pop(vm); pop(vm); /* sa, sb, a, b */
            push(vm, OBJ_VAL(result));
            return true;
        }
    }

    runtimeError(vm, "Operands must be two numbers or two strings.");
    return false;
}

/* ============================================================================
 * Upvalue closing
 * ============================================================================ */

void btl_compiled_close_upvalues(VM* vm, CallFrame* frame) {
    RuntimeUpvalue* uv = frame->openUpvalues;
    while (uv != NULL) {
        RuntimeUpvalue* next = uv->next;
        if (uv->isOpen) {
            Value val = *uv->loc.stack;
            Value* slotPtr = uv->loc.stack;
            if (uv->isMutable) {
                ObjUpvalue* box = newUpvalueBox(vm, val);
                uv->isOpen = false;
                uv->loc.box = box;
                RuntimeUpvalue* search = next;
                while (search != NULL) {
                    if (search->isOpen && search->loc.stack == slotPtr) {
                        search->isOpen = false;
                        search->loc.box = box;
                    }
                    search = search->next;
                }
            } else {
                uv->isOpen = false;
                uv->loc.immValue = val;
                RuntimeUpvalue* search = next;
                while (search != NULL) {
                    if (search->isOpen && search->loc.stack == slotPtr) {
                        search->isOpen = false;
                        search->loc.immValue = val;
                    }
                    search = search->next;
                }
            }
        }
        uv = next;
    }
    frame->openUpvalues = NULL;
}

/* ============================================================================
 * OP_FIELD
 * ============================================================================ */

void btl_compiled_field(VM* vm, CallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* klass = AS_CLASS(vm->stackTop[-1]);
    Value dummy;
    if (!tableGet(&klass->fieldIndices, OBJ_VAL(name), &dummy)) {
        tableSet(vm, &klass->fieldIndices, OBJ_VAL(name), NUMBER_VAL(klass->fieldCount));
        klass->fieldCount++;
    }
}

/* ============================================================================
 * Property access helpers
 * ============================================================================ */

static bool bindMethodCompiled(VM* vm, ObjClass* klass, ObjString* name) {
    for (int i = 0; i < klass->methodCount; i++) {
        if (klass->methods[i].closure != NULL &&
            klass->methods[i].name != NULL &&
            klass->methods[i].name == name) {
            ObjBoundMethod* bound = newBoundMethod(vm, vm->stackTop[-1], klass->methods[i].closure);
            vm->stackTop[-1] = OBJ_VAL(bound);
            return true;
        }
    }
    return false;
}

static ObjNativeClass* getNativeClassCompiled(VM* vm, Value value) {
    if (IS_STRING(value)) return vm->stringClass;
    if (IS_NUMBER(value)) return vm->numberClass;
    if (IS_LIST(value)) return vm->listClass;
    if (IS_TABLE(value)) return vm->tableClass;
    return NULL;
}

static ObjNativeMethod* findNativeMethodCompiled(ObjNativeClass* klass, ObjString* name) {
    if (klass == NULL) return NULL;
    Value method;
    if (tableGet(&klass->methods, OBJ_VAL(name), &method)) {
        return AS_NATIVE_METHOD(method);
    }
    return NULL;
}

bool btl_compiled_get_property(VM* vm, CallFrame* frame, int nameIdx, int icSlot) {
    Value receiver = vm->stackTop[-1];

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        FieldIC* ic = &frame->closure->fieldICs[icSlot];

        /* IC fast path */
        if (ic->cachedClass == instance->klass && ic->fieldIndex >= 0) {
            vm->stackTop[-1] = instance->fields[ic->fieldIndex];
            return true;
        }

        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value indexVal;
        if (tableGet(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
            int idx = (int) AS_NUMBER(indexVal);
            ic->cachedClass = instance->klass;
            ic->fieldIndex = idx;
            vm->stackTop[-1] = instance->fields[idx];
            return true;
        }
        if (bindMethodCompiled(vm, instance->klass, name)) return true;
        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;

    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value idx;
        if (tableGet(&m->globalNames, OBJ_VAL(name), &idx)) {
            pop(vm);
            push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]);
            return true;
        }
        runtimeError(vm, "Undefined property '%s' in module.", name->chars);
        return false;

    } else if (IS_NATIVE_MODULE(receiver)) {
        ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value value;
        if (tableGet(&module->globals, OBJ_VAL(name), &value)) {
            pop(vm);
            push(vm, value);
            return true;
        }
        runtimeError(vm, "Undefined property '%s' in native module.", name->chars);
        return false;

    } else {
        ObjNativeClass* nativeClass = getNativeClassCompiled(vm, receiver);
        if (nativeClass != NULL) {
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            ObjNativeMethod* method = findNativeMethodCompiled(nativeClass, name);
            if (method != NULL) {
                vm->stackTop[-1] = OBJ_VAL(method);
                return true;
            }
        }
    }

    runtimeError(vm, "Only instances and modules have properties.");
    return false;
}

bool btl_compiled_set_property(VM* vm, CallFrame* frame, int nameIdx, int icSlot) {
    Value receiver = vm->stackTop[-2];

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        FieldIC* ic = &frame->closure->fieldICs[icSlot];

        /* IC fast path */
        if (ic->cachedClass == instance->klass && ic->fieldIndex >= 0) {
            Value val = vm->stackTop[-1];
            instance->fields[ic->fieldIndex] = val;
            vm->stackTop -= 2;
            push(vm, val);
            return true;
        }

        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value indexVal;
        if (tableGet(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
            int idx = (int) AS_NUMBER(indexVal);
            ic->cachedClass = instance->klass;
            ic->fieldIndex = idx;
            Value val = vm->stackTop[-1];
            instance->fields[idx] = val;
            vm->stackTop -= 2;
            push(vm, val);
            return true;
        }

        runtimeError(vm, "Cannot add new property '%s' to fixed class layout.", name->chars);
        return false;
    }

    runtimeError(vm, "Only instances have fields.");
    return false;
}

/* ============================================================================
 * Super property
 * ============================================================================ */

bool btl_compiled_get_super(VM* vm, CallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* superclass = AS_CLASS(pop(vm));
    return bindMethodCompiled(vm, superclass, name);
}

bool btl_compiled_get_super_long(VM* vm, CallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* superclass = AS_CLASS(pop(vm));
    return bindMethodCompiled(vm, superclass, name);
}

/* ============================================================================
 * Call + run helper
 *
 * callValue() handles natives immediately (no frame push), but closures
 * push a frame that needs run(). We check frameCount to distinguish.
 * ============================================================================ */

static bool callAndRun(VM* vm, Value callee, int argCount) {
    int frameBefore = vm->frameCount;
    if (!callValue(vm, callee, argCount)) return false;
    /* If callValue pushed a new frame, we need to run it.
     * If it didn't (native function), result is already on the stack. */
    if (vm->frameCount > frameBefore) {
        int savedFloor = vm->runFloor;
        vm->runFloor = frameBefore;
        InterpretResult r = run(vm);
        vm->runFloor = savedFloor;
        return r == INTERPRET_OK;
    }
    return true;
}

/* Same but for a known closure (via btl_call_closure wrapper) */
static bool callClosureAndRun(VM* vm, ObjClosure* closure, int argCount) {
    return callAndRun(vm, OBJ_VAL(closure), argCount);
}

/* ============================================================================
 * Invoke helpers
 * ============================================================================ */

bool btl_compiled_invoke_indexed(VM* vm, int methodIndex, int argCount) {
    Value receiver = vm->stackTop[-argCount - 1];
    if (!IS_INSTANCE(receiver)) {
        runtimeError(vm, "Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);
    ObjClass* klass = instance->klass;

    if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
        runtimeError(vm, "Undefined method.");
        return false;
    }

    MethodEntry* entry = &klass->methods[methodIndex];
    return callClosureAndRun(vm, entry->closure, argCount);
}

bool btl_compiled_invoke_ic(VM* vm, CallFrame* frame, int nameIdx, int argCount, int icSlot) {
    Value receiver = vm->stackTop[-argCount - 1];

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        MethodIC* ic = &frame->closure->methodICs[icSlot];

        /* IC fast path */
        if (ic->cachedClass == instance->klass && ic->methodIndex >= 0) {
            MethodEntry* entry = &instance->klass->methods[ic->methodIndex];
            if (argCount != entry->arity) {
                runtimeError(vm, "Expected %d arguments but got %d.", entry->arity, argCount);
                return false;
            }
            return callClosureAndRun(vm, entry->closure, argCount);
        }

        /* Slow path: name lookup */
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

        /* Check callable fields */
        Value fieldIdx;
        if (tableGet(&instance->klass->fieldIndices, OBJ_VAL(name), &fieldIdx)) {
            int idx = (int) AS_NUMBER(fieldIdx);
            Value field = instance->fields[idx];
            vm->stackTop[-argCount - 1] = field;
            return callAndRun(vm, field, argCount);
        }

        /* Method search by name + arity */
        for (int i = 0; i < instance->klass->methodCount; i++) {
            ObjString* methodName = instance->klass->methods[i].name;
            if (instance->klass->methods[i].closure != NULL &&
                methodName != NULL &&
                methodName->length == name->length &&
                memcmp(methodName->chars, name->chars, name->length) == 0 &&
                instance->klass->methods[i].arity == argCount) {

                ic->cachedClass = instance->klass;
                ic->methodIndex = i;

                return callClosureAndRun(vm, instance->klass->methods[i].closure, argCount);
            }
        }

        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;

    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value idx;
        if (tableGet(&m->globalNames, OBJ_VAL(name), &idx)) {
            Value func = m->globalValues.values[(int) AS_NUMBER(idx)];
            vm->stackTop[-argCount - 1] = func;
            return callAndRun(vm, func, argCount);
        }
        runtimeError(vm, "Undefined function '%s' in module.", name->chars);
        return false;

    } else if (IS_NATIVE_MODULE(receiver)) {
        ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        Value func;
        if (tableGet(&module->globals, OBJ_VAL(name), &func)) {
            vm->stackTop[-argCount - 1] = func;
            return callAndRun(vm, func, argCount);
        }
        runtimeError(vm, "Undefined function '%s' in native module.", name->chars);
        return false;

    } else {
        ObjNativeClass* nativeClass = getNativeClassCompiled(vm, receiver);
        if (nativeClass != NULL) {
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            ObjNativeMethod* method = findNativeMethodCompiled(nativeClass, name);
            if (method != NULL) {
                if (method->arity >= 0 && argCount != method->arity) {
                    runtimeError(vm, "Expected %d arguments but got %d.", method->arity, argCount);
                    return false;
                }
                Value* args = vm->stackTop - argCount;
                Value result = method->function(vm, receiver, argCount, args);
                vm->stackTop -= argCount + 1;
                push(vm, result);
                return true;
            }
        }
    }

    runtimeError(vm, "Only instances and modules have methods.");
    return false;
}

bool btl_compiled_super_invoke(VM* vm, int methodIndex, int argCount) {
    ObjClass* superclass = AS_CLASS(pop(vm));
    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        runtimeError(vm, "Undefined method in superclass.");
        return false;
    }
    MethodEntry* entry = &superclass->methods[methodIndex];
    return callClosureAndRun(vm, entry->closure, argCount);
}

/* ============================================================================
 * Class operations
 * ============================================================================ */

void btl_compiled_class(VM* vm, ObjFunction* fn, int nameIdx) {
    ObjString* name = AS_STRING(fn->chunk.constants.values[nameIdx]);
    ObjClass* klass = newClass(vm, name);
    Value savedIndicesValue;
    if (tableGet(&vm->rootModule->classInfo, OBJ_VAL(name), &savedIndicesValue)) {
        Table* savedIndices = (Table*) (uintptr_t) AS_NUMBER(savedIndicesValue);
        tableAddAll(vm, savedIndices, &klass->methodIndices);
    }
    push(vm, OBJ_VAL(klass));
}

void btl_compiled_class_long(VM* vm, ObjFunction* fn, int nameIdx) {
    ObjString* name = AS_STRING(fn->chunk.constants.values[nameIdx]);
    ObjClass* klass = newClass(vm, name);
    Value savedIndicesValue;
    if (tableGet(&vm->rootModule->classInfo, OBJ_VAL(name), &savedIndicesValue)) {
        Table* savedIndices = (Table*) (uintptr_t) AS_NUMBER(savedIndicesValue);
        tableAddAll(vm, savedIndices, &klass->methodIndices);
    }
    push(vm, OBJ_VAL(klass));
}

static void growMethodTableCompiled(VM* vm, ObjClass* klass, int requiredIndex) {
    if (requiredIndex < klass->methodCapacity) return;
    int oldCapacity = klass->methodCapacity;
    int newCapacity = oldCapacity < 8 ? 8 : oldCapacity * 2;
    while (newCapacity <= requiredIndex) newCapacity *= 2;
    MethodEntry* newMethods = ALLOCATE(vm, MethodEntry, newCapacity);
    if (klass->methods != NULL) {
        memcpy(newMethods, klass->methods, sizeof(MethodEntry) * klass->methodCount);
        FREE_ARRAY(vm, MethodEntry, klass->methods, oldCapacity);
    }
    for (int i = klass->methodCount; i < newCapacity; i++) {
        newMethods[i].closure = NULL; newMethods[i].arity = 0;
    }
    klass->methods = newMethods;
    klass->methodCapacity = newCapacity;
}

bool btl_compiled_inherit(VM* vm) {
    Value superclassVal = vm->stackTop[-2];
    if (!IS_CLASS(superclassVal)) {
        runtimeError(vm, "Superclass must be a class.");
        return false;
    }
    ObjClass* superclass = AS_CLASS(superclassVal);
    ObjClass* subclass = AS_CLASS(vm->stackTop[-1]);

    if (superclass->methodCount > 0) {
        subclass->methodCapacity = superclass->methodCapacity;
        subclass->methodCount = superclass->methodCount;
        subclass->methods = ALLOCATE(vm, MethodEntry, subclass->methodCapacity);
        memcpy(subclass->methods, superclass->methods, sizeof(MethodEntry) * superclass->methodCount);
        tableAddAll(vm, &superclass->methodIndices, &subclass->methodIndices);
    }
    tableAddAll(vm, &superclass->fieldIndices, &subclass->fieldIndices);
    subclass->fieldCount = superclass->fieldCount;
    pop(vm); pop(vm);
    return true;
}

void btl_compiled_method(VM* vm, int methodIndex, int arity) {
    ObjClosure* method = AS_CLOSURE(vm->stackTop[-1]);
    ObjClass* klass = AS_CLASS(vm->stackTop[-2]);

    if (methodIndex >= klass->methodCapacity) {
        growMethodTableCompiled(vm, klass, methodIndex);
    }

    klass->methods[methodIndex].closure = method;
    klass->methods[methodIndex].arity = arity;
    klass->methods[methodIndex].name = method->function->name;
    if (methodIndex >= klass->methodCount) {
        klass->methodCount = methodIndex + 1;
    }

    ObjString* name = method->function->name;
    int nameLen = name->length;
    char* buffer = ALLOCATE(vm, char, nameLen + 2);
    memcpy(buffer, name->chars, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = copyString(vm, buffer, nameLen + 1);
    FREE_ARRAY(vm, char, buffer, nameLen + 2);

    push(vm, OBJ_VAL(signature));
    tableSet(vm, &klass->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    pop(vm);
    pop(vm); /* method closure */
}

/* ============================================================================
 * Collections
 * ============================================================================ */

void btl_compiled_build_list(VM* vm, int count) {
    ObjList* l = newList(vm);
    push(vm, OBJ_VAL(l));
    for (int i = 0; i < count; i++) {
        writeValueArray(vm, &l->items, vm->stackTop[-count - 1 + i]);
    }
    vm->stackTop -= (count + 1);
    push(vm, OBJ_VAL(l));
}

void btl_compiled_build_table(VM* vm, int count) {
    ObjTable* table = newTable(vm);
    push(vm, OBJ_VAL(table));
    Value* pairs = vm->stackTop - (count * 2) - 1;
    for (int i = 0; i < count; i++) {
        Value key = pairs[i * 2];
        Value value = pairs[i * 2 + 1];
        tableSet(vm, &table->table, key, value);
    }
    vm->stackTop -= (count * 2 + 1);
    push(vm, OBJ_VAL(table));
}

bool btl_compiled_index_get(VM* vm) {
    Value key = pop(vm);
    Value obj = pop(vm);

    if (IS_LIST(obj)) {
        if (!IS_NUMBER(key)) {
            runtimeError(vm, "List index must be a number."); return false;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) AS_NUMBER(key);
        if (idx < 0 || idx >= l->items.count) {
            runtimeError(vm, "List index out of bounds."); return false;
        }
        push(vm, l->items.values[idx]);
        return true;
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        Value value;
        push(vm, tableGet(&table->table, key, &value) ? value : NULL_VAL);
        return true;
    } else if (IS_STRING(obj)) {
        if (!IS_NUMBER(key)) {
            runtimeError(vm, "String index must be a number."); return false;
        }
        ObjString* str = AS_STRING(obj);
        int idx = (int) AS_NUMBER(key);
        if (idx < 0 || idx >= str->length) {
            runtimeError(vm, "String index out of bounds."); return false;
        }
        push(vm, OBJ_VAL(copyString(vm, &str->chars[idx], 1)));
        return true;
    }
    runtimeError(vm, "Only lists, tables, and strings can be indexed.");
    return false;
}

bool btl_compiled_index_set(VM* vm) {
    Value value = pop(vm);
    Value key = pop(vm);
    Value obj = pop(vm);

    if (IS_LIST(obj)) {
        if (!IS_NUMBER(key)) {
            runtimeError(vm, "List index must be a number."); return false;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) AS_NUMBER(key);
        if (idx < 0 || idx > l->items.count) {
            runtimeError(vm, "List index out of bounds."); return false;
        }
        if (idx == l->items.count) writeValueArray(vm, &l->items, value);
        else {
            l->items.values[idx] = value; writeBarrier(vm, (Obj*) l, value);
        }
        push(vm, value);
        return true;
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        tableSet(vm, &table->table, key, value);
        writeBarrier(vm, (Obj*) table, value);
        writeBarrier(vm, (Obj*) table, key);
        push(vm, value);
        return true;
    } else if (IS_STRING(obj)) {
        runtimeError(vm, "Strings are immutable.");
        return false;
    }
    runtimeError(vm, "Only lists and tables can be indexed for assignment.");
    return false;
}

/* ============================================================================
 * Modules
 * ============================================================================ */

static char* btl_compiled_readFile(VM* vm, const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) return NULL;
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    char* buffer = (char*) btl_realloc(vm, NULL, 0, fileSize + 1);
    if (!buffer) {
        fclose(file); return NULL;
    }
    size_t bytesRead = fread(buffer, 1, fileSize, file);
    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

bool btl_compiled_import(VM* vm, CallFrame* frame, int nameIdx) {
    ObjString* fName = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

    // Check native modules
    Value nativeModule;
    if (tableGet(&vm->nativeModules, OBJ_VAL(fName), &nativeModule)) {
        push(vm, nativeModule);
        return true;
    }

    // Check already-loaded modules
    Value mVal;
    if (tableGet(&vm->modules, OBJ_VAL(fName), &mVal)) {
        push(vm, mVal);
        return true;
    }

    // File-based import: read, compile, execute
    char* src = btl_compiled_readFile(vm, fName->chars);
    if (!src) {
        runtimeError(vm, "Could not open file \"%s\".", fName->chars);
        return false;
    }

    size_t srcLen = strlen(src);
    ObjModule* m = newModule(vm, fName);
    ObjFunction* f = compile(vm, m, src);
    btl_realloc(vm, src, srcLen + 1, 0);

    if (!f) return false;

    ObjClosure* c = newClosure(vm, f);
    push(vm, OBJ_VAL(c));

    if (!callValue(vm, OBJ_VAL(c), 0)) return false;

    // Run the imported module via the interpreter
    InterpretResult result = run(vm);
    if (result != INTERPRET_OK) return false;

    // The module's frame sets slots[0] = module
    vm->frames[vm->frameCount].slots[0] = OBJ_VAL(m);
    tableSet(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));

    push(vm, OBJ_VAL(m));
    return true;
}

bool btl_compiled_import_long(VM* vm, CallFrame* frame, int nameIdx) {
    return btl_compiled_import(vm, frame, nameIdx);
}

/* ============================================================================
 * Actors
 * ============================================================================ */

static inline Value peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

bool btl_compiled_do_new(VM* vm, int argCount) {
    Value classVal = peek(vm, argCount);
    if (!IS_CLASS(classVal)) {
        runtimeError(vm, "Can only create actors from classes.");
        return false;
    }
    ObjClass* klass = AS_CLASS(classVal);

    // Collect args from stack
    Value* args = NULL;
    if (argCount > 0) {
        args = btl_realloc(vm, NULL, 0, sizeof(Value) * argCount);
        for (int i = 0; i < argCount; i++) {
            args[i] = peek(vm, argCount - 1 - i);
        }
    }

    ObjActor* actor = newActor(vm, klass, args, argCount);

    if (args != NULL) btl_realloc(vm, args, sizeof(Value) * argCount, 0);

    // Pop args + class
    for (int i = 0; i <= argCount; i++) pop(vm);
    push(vm, OBJ_VAL(actor));
    return true;
}

bool btl_compiled_do_invoke(VM* vm, CallFrame* frame, int nameConst, int argCount) {
    ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameConst]);
    Value actorVal = peek(vm, argCount);

    if (IS_NULL(actorVal)) {
        for (int i = 0; i <= argCount; i++) pop(vm);
        push(vm, NULL_VAL);
        return true;
    }

    if (!IS_ACTOR(actorVal)) {
        runtimeError(vm, "Expected actor for 'do' method call.");
        return false;
    }

    ObjActor* actor = AS_ACTOR(actorVal);

    if (!actor->alive) {
        for (int i = 0; i <= argCount; i++) pop(vm);
        push(vm, NULL_VAL);
        return true;
    }

    ObjFuture* future = newFuture(vm);

    Value* args = NULL;
    if (argCount > 0) {
        args = btl_realloc(vm, NULL, 0, sizeof(Value) * argCount);
        for (int i = 0; i < argCount; i++) {
            args[i] = peek(vm, argCount - 1 - i);
        }
    }

    actorSend(actor, methodName, args, argCount, future);

    if (args != NULL) btl_realloc(vm, args, sizeof(Value) * argCount, 0);

    for (int i = 0; i <= argCount; i++) pop(vm);
    push(vm, OBJ_VAL(future));
    return true;
}