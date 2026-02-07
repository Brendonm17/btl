// ============================================================================
// compiled.c - BTL Compiled Support Library Implementation
//
// Each function here corresponds to a complex opcode that's too big
// to inline in the generated code. The logic is extracted directly
// from vm.c's dispatch cases.
//
// IMPORTANT: All functions here operate on vm->stackTop. The
// generated code must sync its local 'sp' -> vm->stackTop before
// calling any of these, and reload sp afterward.
// ============================================================================

#include "compiled.h"
#include <stdio.h>

// ----------------------------------------------------------------------------
// String helpers (extracted from vm.c)
// ----------------------------------------------------------------------------

static ObjString* valueToStringCompiled(VM* vm, BtlValue value) {
    if (IS_STRING(value)) return AS_STRING(value);
    char buf[32];
    if (IS_INT(value)) {
        int len = snprintf(buf, 32, "%" PRId64, AS_INT(value));
        return btl_string_copy(vm, buf, len);
    }
    if (IS_NUMBER(value)) {
        int len = snprintf(buf, 32, "%g", AS_NUMBER(value));
        return btl_string_copy(vm, buf, len);
    }
    if (IS_BOOL(value)) return btl_string_copy(vm, AS_BOOL(value) ? "true" : "false", AS_BOOL(value) ? 4 : 5);
    if (IS_NULL(value)) return btl_string_copy(vm, "null", 3);
    return btl_string_copy(vm, "<object>", 8);
}

// ----------------------------------------------------------------------------
// OP_ADD (slow path - string concatenation)
//
// The generated code inlines the number fast path. This is only called
// when at least one operand is a string.
// ----------------------------------------------------------------------------

bool btl_compiled_add(VM* vm) {
    BtlValue b = vm->stackTop[-1];
    BtlValue a = vm->stackTop[-2];

    if (IS_INT(a) && IS_INT(b)) {
        vm->stackTop -= 2;
        btl_push(vm, INT_VAL(AS_INT(a) + AS_INT(b)));
        return true;
    }

    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        vm->stackTop -= 2;
        btl_push(vm, NUMBER_VAL(AS_NUMBER(a) + AS_NUMBER(b)));
        return true;
    }

    if (IS_NUMERIC(a) && IS_NUMERIC(b)) {
        vm->stackTop -= 2;
        btl_push(vm, NUMBER_VAL(btl_numeric_to_double(a) + btl_numeric_to_double(b)));
        return true;
    }

    if (IS_STRING(a) || IS_STRING(b)) {
        if ((IS_STRING(a) || IS_NUMERIC(a)) && (IS_STRING(b) || IS_NUMERIC(b))) {
            ObjString* sa = valueToStringCompiled(vm, a); btl_push(vm, OBJ_VAL(sa));
            ObjString* sb = valueToStringCompiled(vm, b); btl_push(vm, OBJ_VAL(sb));
            int length = sa->length + sb->length;
            char* chars = BTL_ALLOCATE(vm, char, length + 1);
            memcpy(chars, sa->chars, sa->length);
            memcpy(chars + sa->length, sb->chars, sb->length);
            chars[length] = '\0';
            ObjString* result = btl_string_take(vm, chars, length);
            btl_pop(vm); btl_pop(vm); btl_pop(vm); btl_pop(vm); // sa, sb, a, b
            btl_push(vm, OBJ_VAL(result));
            return true;
        }
    }

    btl_runtime_error(vm, "Operands must be two numbers or two strings.");
    return false;
}

// ----------------------------------------------------------------------------
// Upvalue closing
// ----------------------------------------------------------------------------

void btl_compiled_close_upvalues(VM* vm, BtlCallFrame* frame) {
    BtlRuntimeUpvalue* uv = frame->openUpvalues;
    while (uv != NULL) {
        BtlRuntimeUpvalue* next = uv->next;
        if (uv->isOpen) {
            BtlValue val = *uv->loc.stack;
            BtlValue* slotPtr = uv->loc.stack;
            if (uv->isMutable) {
                ObjUpvalue* box = btl_upvalue_box_new(vm, val);
                uv->isOpen = false;
                uv->loc.box = box;
                BtlRuntimeUpvalue* search = next;
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
                BtlRuntimeUpvalue* search = next;
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

// ----------------------------------------------------------------------------
// OP_FIELD
// ----------------------------------------------------------------------------

void btl_compiled_field(VM* vm, BtlCallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* klass = AS_CLASS(vm->stackTop[-1]);
    BtlValue dummy;
    if (!btl_table_get(&klass->fieldIndices, OBJ_VAL(name), &dummy)) {
        btl_table_set(vm, &klass->fieldIndices, OBJ_VAL(name), NUMBER_VAL(klass->fieldCount));
        klass->fieldCount++;
    }
}

// ----------------------------------------------------------------------------
// Property access helpers
// ----------------------------------------------------------------------------

static bool bindMethodCompiled(VM* vm, ObjClass* klass, ObjString* name) {
    for (int i = 0; i < klass->methodCount; i++) {
        if (klass->methods[i].closure != NULL &&
            klass->methods[i].name != NULL &&
            klass->methods[i].name == name) {
            ObjBoundMethod* bound = btl_bound_method_new(vm, vm->stackTop[-1], klass->methods[i].closure);
            vm->stackTop[-1] = OBJ_VAL(bound);
            return true;
        }
    }
    return false;
}

static ObjNativeClass* getNativeClassCompiled(VM* vm, BtlValue value) {
    if (IS_STRING(value)) return vm->stringClass;
    if (IS_INT(value))    return vm->intClass;
    if (IS_NUMBER(value)) return vm->numberClass;
    if (IS_LIST(value)) return vm->listClass;
    if (IS_TABLE(value)) return vm->tableClass;
    return NULL;
}

static ObjNativeMethod* findNativeMethodCompiled(ObjNativeClass* klass, ObjString* name) {
    if (klass == NULL) return NULL;
    BtlValue method;
    if (btl_table_get(&klass->methods, OBJ_VAL(name), &method)) {
        return AS_NATIVE_METHOD(method);
    }
    return NULL;
}

bool btl_compiled_get_property(VM* vm, BtlCallFrame* frame, int nameIdx, int icSlot) {
    BtlValue receiver = vm->stackTop[-1];

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        BtlFieldIC* ic = &frame->closure->fieldICs[icSlot];

        // IC fast path
        if (__builtin_expect(ic->cachedClass == instance->klass && ic->fieldIndex >= 0, 1)) {
            vm->stackTop[-1] = instance->fields[ic->fieldIndex];
            return true;
        }

        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue indexVal;
        if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
            int idx = (int) AS_NUMBER(indexVal);
            ic->cachedClass = instance->klass;
            ic->fieldIndex = idx;
            vm->stackTop[-1] = instance->fields[idx];
            return true;
        }
        if (bindMethodCompiled(vm, instance->klass, name)) return true;
        btl_runtime_error(vm, "Undefined property '%s'.", name->chars);
        return false;

    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue idx;
        if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
            btl_pop(vm);
            btl_push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]);
            return true;
        }
        btl_runtime_error(vm, "Undefined property '%s' in module.", name->chars);
        return false;

    } else if (IS_NATIVE_MODULE(receiver)) {
        ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue value;
        if (btl_table_get(&module->globals, OBJ_VAL(name), &value)) {
            btl_pop(vm);
            btl_push(vm, value);
            return true;
        }
        btl_runtime_error(vm, "Undefined property '%s' in native module.", name->chars);
        return false;

    } else {
        ObjNativeClass* nativeClass = getNativeClassCompiled(vm, receiver);
        if (nativeClass != NULL) {
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            ObjNativeMethod* method = findNativeMethodCompiled(nativeClass, name);
            // Int fallback: if method not found on intClass, try numberClass
            if (method == NULL && IS_INT(receiver)) {
                method = findNativeMethodCompiled(vm->numberClass, name);
            }
            if (method != NULL) {
                vm->stackTop[-1] = OBJ_VAL(method);
                return true;
            }
        }
    }

    btl_runtime_error(vm, "Only instances and modules have properties.");
    return false;
}

bool btl_compiled_set_property(VM* vm, BtlCallFrame* frame, int nameIdx, int icSlot) {
    BtlValue receiver = vm->stackTop[-2];

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        BtlFieldIC* ic = &frame->closure->fieldICs[icSlot];

        // IC fast path
        if (__builtin_expect(ic->cachedClass == instance->klass && ic->fieldIndex >= 0, 1)) {
            BtlValue val = vm->stackTop[-1];
            instance->fields[ic->fieldIndex] = val;
            vm->stackTop -= 2;
            btl_push(vm, val);
            return true;
        }

        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue indexVal;
        if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
            int idx = (int) AS_NUMBER(indexVal);
            ic->cachedClass = instance->klass;
            ic->fieldIndex = idx;
            BtlValue val = vm->stackTop[-1];
            instance->fields[idx] = val;
            vm->stackTop -= 2;
            btl_push(vm, val);
            return true;
        }

        btl_runtime_error(vm, "Cannot add new property '%s' to fixed class layout.", name->chars);
        return false;
    }

    btl_runtime_error(vm, "Only instances have fields.");
    return false;
}

// ----------------------------------------------------------------------------
// Super property
// ----------------------------------------------------------------------------

bool btl_compiled_get_super(VM* vm, BtlCallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* superclass = AS_CLASS(btl_pop(vm));
    return bindMethodCompiled(vm, superclass, name);
}

bool btl_compiled_get_super_long(VM* vm, BtlCallFrame* frame, int nameIdx) {
    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
    ObjClass* superclass = AS_CLASS(btl_pop(vm));
    return bindMethodCompiled(vm, superclass, name);
}

// ----------------------------------------------------------------------------
// Call + run helper
//
// btl_call_value handles natives immediately (no frame push), but closures
// push a frame that needs btl_run(). We check frameCount to distinguish.
// ----------------------------------------------------------------------------

static bool callAndRun(VM* vm, BtlValue callee, int argCount) {
    int frameBefore = vm->frameCount;
    if (!btl_call_value(vm, callee, argCount)) return false;
    // If btl_call_value pushed a new frame, we need to run it.
    // If it didn't (native function), result is already on the stack.
    if (vm->frameCount > frameBefore) {
        int savedFloor = vm->runFloor;
        vm->runFloor = frameBefore;
        BtlInterpretResult r = btl_run(vm);
        vm->runFloor = savedFloor;
        return r == BTL_INTERPRET_OK;
    }
    return true;
}

// Direct closure call with transpiled handler dispatch.
// If the closure has a compiled handler, we call it directly instead of
// falling back to the interpreter. This is critical for performance when
// transpiled code calls methods on classes.
//
// Exposed as non-static so the generated inline IC fast path can call it.
bool btl_compiled_call_closure_and_run(VM* vm, ObjClosure* closure, int argCount) {
    ObjFunction* fn = closure->function;

    // Check arity
    if (argCount != fn->arity) {
        btl_runtime_error(vm, "Expected %d arguments but got %d.", fn->arity, argCount);
        return false;
    }

    // Push frame
    if (vm->frameCount >= vm->frameCapacity) {
        if (!btl_ensure_frame_capacity(vm)) {
            btl_runtime_error(vm, "Stack overflow.");
            return false;
        }
    }

    BtlCallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = closure;
    frame->ip = fn->chunk.code;
    frame->slots = vm->stackTop - argCount - 1;
    frame->openUpvalues = NULL;

    // If there's a compiled handler, call it directly
    if (fn->compiledHandler != NULL) {
        typedef BtlInterpretResult (*BtlFnPtr)(VM*);
        BtlFnPtr handler = (BtlFnPtr)fn->compiledHandler;
        BtlInterpretResult r = handler(vm);
        return r == BTL_INTERPRET_OK;
    }

    // Fall back to interpreter for non-transpiled functions
    int savedFloor = vm->runFloor;
    vm->runFloor = vm->frameCount - 1;
    BtlInterpretResult r = btl_run(vm);
    vm->runFloor = savedFloor;
    return r == BTL_INTERPRET_OK;
}

// ----------------------------------------------------------------------------
// Fast class instantiation
//
// This is called when we detect a class call at transpile time. It's faster
// than btl_call_value because:
// 1. We skip the type check (we know it's a class)
// 2. We use the class's initCache to avoid table lookup on every call
// ----------------------------------------------------------------------------

// Static cache for init method signatures - used only on cache miss
static ObjString* initSignatureCache[9] = {NULL};

static ObjString* getInitSignature(VM* vm, int argCount) {
    if (argCount > 8) {
        // Fallback for rare high-arity cases
        char initSig[6];
        memcpy(initSig, "init", 4);
        initSig[4] = (char) argCount;
        initSig[5] = '\0';
        return btl_string_copy(vm, initSig, 5);
    }

    if (initSignatureCache[argCount] == NULL) {
        char initSig[6];
        memcpy(initSig, "init", 4);
        initSig[4] = (char) argCount;
        initSig[5] = '\0';
        initSignatureCache[argCount] = btl_string_copy(vm, initSig, 5);
    }
    return initSignatureCache[argCount];
}

bool btl_compiled_call_class(VM* vm, ObjClass* klass, int argCount) {
    // Create instance and replace class on stack
    vm->stackTop[-argCount - 1] = OBJ_VAL(btl_instance_new(vm, klass));

    // Use cached init method index if available (arities 0-8)
    if (argCount <= 8) {
        int cachedIdx = klass->initCache[argCount];

        if (cachedIdx == -1) {
            // Cache miss - do table lookup and cache result
            ObjString* initSig = getInitSignature(vm, argCount);
            BtlValue indexValue;
            if (btl_table_get(&klass->methodIndices, OBJ_VAL(initSig), &indexValue)) {
                cachedIdx = (int) AS_NUMBER(indexValue);
                klass->initCache[argCount] = cachedIdx;
            } else {
                // No init method for this arity
                klass->initCache[argCount] = -2;
                cachedIdx = -2;
            }
        }

        if (cachedIdx >= 0 && cachedIdx < klass->methodCount) {
            ObjClosure* initializer = klass->methods[cachedIdx].closure;
            if (initializer != NULL) {
                return btl_compiled_call_closure_and_run(vm, initializer, argCount);
            }
        }

        // No init method
        if (argCount != 0) {
            btl_runtime_error(vm, "Expected 0 arguments but got %d.", argCount);
            return false;
        }
        return true;
    }

    // Rare high-arity case - do normal lookup
    ObjString* initSig = getInitSignature(vm, argCount);
    BtlValue indexValue;
    if (btl_table_get(&klass->methodIndices, OBJ_VAL(initSig), &indexValue)) {
        int methodIndex = (int) AS_NUMBER(indexValue);
        if (methodIndex >= 0 && methodIndex < klass->methodCount) {
            ObjClosure* initializer = klass->methods[methodIndex].closure;
            if (initializer != NULL) {
                return btl_compiled_call_closure_and_run(vm, initializer, argCount);
            }
        }
    }

    if (argCount != 0) {
        btl_runtime_error(vm, "Expected 0 arguments but got %d.", argCount);
        return false;
    }
    return true;
}

// ----------------------------------------------------------------------------
// Invoke helpers
// ----------------------------------------------------------------------------

bool btl_compiled_invoke_indexed(VM* vm, int methodIndex, int argCount) {
    BtlValue receiver = vm->stackTop[-argCount - 1];
    if (!IS_INSTANCE(receiver)) {
        btl_runtime_error(vm, "Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);
    ObjClass* klass = instance->klass;

    if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
        btl_runtime_error(vm, "Undefined method.");
        return false;
    }

    BtlMethodEntry* entry = &klass->methods[methodIndex];
    return btl_compiled_call_closure_and_run(vm, entry->closure, argCount);
}

bool btl_compiled_invoke_ic(VM* vm, BtlCallFrame* frame, int nameIdx, int argCount, int icSlot) {
    BtlValue receiver = vm->stackTop[-argCount - 1];

    // Actor check: async method dispatch (must come before IS_INSTANCE)
    if (IS_ACTOR(receiver)) {
        ObjActor* actor = AS_ACTOR(receiver);
        ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        if (!actor->alive) {
            for (int i = 0; i <= argCount; i++) btl_pop(vm);
            btl_push(vm, BTL_NULL_VAL);
            return true;
        }
        ObjFuture* future = btl_future_new(vm);
        BtlValue* args = NULL;
        if (argCount > 0) {
            args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                args[i] = vm->stackTop[-argCount + i];
            }
        }
        btl_actor_send(actor, methodName, args, argCount, future);
        if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, OBJ_VAL(future));
        return true;
    }

    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        BtlMethodIC* ic = &frame->closure->methodICs[icSlot];

        // IC fast path
        if (__builtin_expect(ic->cachedClass == instance->klass && ic->methodIndex >= 0, 1)) {
            BtlMethodEntry* entry = &instance->klass->methods[ic->methodIndex];
            if (argCount != entry->arity) {
                btl_runtime_error(vm, "Expected %d arguments but got %d.", entry->arity, argCount);
                return false;
            }
            return btl_compiled_call_closure_and_run(vm, entry->closure, argCount);
        }

        // Slow path: name lookup
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

        // Check callable fields
        BtlValue fieldIdx;
        if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &fieldIdx)) {
            int idx = (int) AS_NUMBER(fieldIdx);
            BtlValue field = instance->fields[idx];
            vm->stackTop[-argCount - 1] = field;
            return callAndRun(vm, field, argCount);
        }

        // Method search by name + arity
        for (int i = 0; i < instance->klass->methodCount; i++) {
            ObjString* methodName = instance->klass->methods[i].name;
            if (instance->klass->methods[i].closure != NULL &&
                methodName != NULL &&
                methodName == name &&
                instance->klass->methods[i].arity == argCount) {

                ic->cachedClass = instance->klass;
                ic->methodIndex = i;

                return btl_compiled_call_closure_and_run(vm, instance->klass->methods[i].closure, argCount);
            }
        }

        btl_runtime_error(vm, "Undefined property '%s'.", name->chars);
        return false;

    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue idx;
        if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
            BtlValue func = m->globalValues.values[(int) AS_NUMBER(idx)];
            vm->stackTop[-argCount - 1] = func;
            return callAndRun(vm, func, argCount);
        }
        btl_runtime_error(vm, "Undefined function '%s' in module.", name->chars);
        return false;

    } else if (IS_NATIVE_MODULE(receiver)) {
        ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue func;
        if (btl_table_get(&module->globals, OBJ_VAL(name), &func)) {
            vm->stackTop[-argCount - 1] = func;
            return callAndRun(vm, func, argCount);
        }
        btl_runtime_error(vm, "Undefined function '%s' in native module.", name->chars);
        return false;

    } else {
        ObjNativeClass* nativeClass = getNativeClassCompiled(vm, receiver);
        if (nativeClass != NULL) {
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            ObjNativeMethod* method = findNativeMethodCompiled(nativeClass, name);
            // Int fallback: if method not found on intClass, try numberClass
            if (method == NULL && IS_INT(receiver)) {
                method = findNativeMethodCompiled(vm->numberClass, name);
            }
            if (method != NULL) {
                if (method->arity >= 0 && argCount != method->arity) {
                    btl_runtime_error(vm, "Expected %d arguments but got %d.", method->arity, argCount);
                    return false;
                }
                BtlValue* args = vm->stackTop - argCount;
                BtlValue result = method->function(vm, receiver, argCount, args);
                vm->stackTop -= argCount + 1;
                btl_push(vm, result);
                return true;
            }
        }
    }

    btl_runtime_error(vm, "Only instances and modules have methods.");
    return false;
}

bool btl_compiled_super_invoke(VM* vm, int methodIndex, int argCount) {
    ObjClass* superclass = AS_CLASS(btl_pop(vm));
    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        btl_runtime_error(vm, "Undefined method in superclass.");
        return false;
    }
    BtlMethodEntry* entry = &superclass->methods[methodIndex];
    return btl_compiled_call_closure_and_run(vm, entry->closure, argCount);
}

// ----------------------------------------------------------------------------
// Class operations
// ----------------------------------------------------------------------------

void btl_compiled_class(VM* vm, ObjFunction* fn, int nameIdx) {
    ObjString* name = AS_STRING(fn->chunk.constants.values[nameIdx]);
    ObjClass* klass = btl_class_new(vm, name);
    BtlValue savedIndicesValue;
    if (btl_table_get(&vm->rootModule->classInfo, OBJ_VAL(name), &savedIndicesValue)) {
        BtlTable* savedIndices = (BtlTable*) (uintptr_t) AS_NUMBER(savedIndicesValue);
        btl_table_add_all(vm, savedIndices, &klass->methodIndices);
    }
    btl_push(vm, OBJ_VAL(klass));
}

void btl_compiled_class_long(VM* vm, ObjFunction* fn, int nameIdx) {
    ObjString* name = AS_STRING(fn->chunk.constants.values[nameIdx]);
    ObjClass* klass = btl_class_new(vm, name);
    BtlValue savedIndicesValue;
    if (btl_table_get(&vm->rootModule->classInfo, OBJ_VAL(name), &savedIndicesValue)) {
        BtlTable* savedIndices = (BtlTable*) (uintptr_t) AS_NUMBER(savedIndicesValue);
        btl_table_add_all(vm, savedIndices, &klass->methodIndices);
    }
    btl_push(vm, OBJ_VAL(klass));
}

static void growMethodTableCompiled(VM* vm, ObjClass* klass, int requiredIndex) {
    if (requiredIndex < klass->methodCapacity) return;
    int oldCapacity = klass->methodCapacity;
    int newCapacity = oldCapacity < 8 ? 8 : oldCapacity * 2;
    while (newCapacity <= requiredIndex) newCapacity *= 2;
    BtlMethodEntry* newMethods = BTL_ALLOCATE(vm, BtlMethodEntry, newCapacity);
    if (klass->methods != NULL) {
        memcpy(newMethods, klass->methods, sizeof(BtlMethodEntry) * klass->methodCount);
        BTL_FREE_ARRAY(vm, BtlMethodEntry, klass->methods, oldCapacity);
    }
    for (int i = klass->methodCount; i < newCapacity; i++) {
        newMethods[i].closure = NULL; newMethods[i].arity = 0;
    }
    klass->methods = newMethods;
    klass->methodCapacity = newCapacity;
}

bool btl_compiled_inherit(VM* vm) {
    BtlValue superclassVal = vm->stackTop[-2];
    if (!IS_CLASS(superclassVal)) {
        btl_runtime_error(vm, "Superclass must be a class.");
        return false;
    }
    ObjClass* superclass = AS_CLASS(superclassVal);
    ObjClass* subclass = AS_CLASS(vm->stackTop[-1]);

    if (superclass->methodCount > 0) {
        subclass->methodCapacity = superclass->methodCapacity;
        subclass->methodCount = superclass->methodCount;
        subclass->methods = BTL_ALLOCATE(vm, BtlMethodEntry, subclass->methodCapacity);
        memcpy(subclass->methods, superclass->methods, sizeof(BtlMethodEntry) * superclass->methodCount);
        btl_table_add_all(vm, &superclass->methodIndices, &subclass->methodIndices);
    }
    btl_table_add_all(vm, &superclass->fieldIndices, &subclass->fieldIndices);
    subclass->fieldCount = superclass->fieldCount;
    btl_pop(vm); btl_pop(vm);
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
    char* buffer = BTL_ALLOCATE(vm, char, nameLen + 2);
    memcpy(buffer, name->chars, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = btl_string_copy(vm, buffer, nameLen + 1);
    BTL_FREE_ARRAY(vm, char, buffer, nameLen + 2);

    btl_push(vm, OBJ_VAL(signature));
    btl_table_set(vm, &klass->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    btl_pop(vm);
    btl_pop(vm); // method closure
}

// ----------------------------------------------------------------------------
// Collections
// ----------------------------------------------------------------------------

// btl_compiled_build_list and btl_compiled_build_table are now
// static inline functions in compiled.h for better inlining.

bool btl_compiled_index_get(VM* vm) {
    BtlValue key = btl_pop(vm);
    BtlValue obj = btl_pop(vm);

    if (IS_LIST(obj)) {
        if (!IS_NUMERIC(key)) {
            btl_runtime_error(vm, "List index must be a number."); return false;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx >= l->items.count) {
            btl_runtime_error(vm, "List index out of bounds."); return false;
        }
        btl_push(vm, l->items.values[idx]);
        return true;
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        BtlValue value;
        btl_push(vm, btl_table_get(&table->table, key, &value) ? value : BTL_NULL_VAL);
        return true;
    } else if (IS_STRING(obj)) {
        if (!IS_NUMERIC(key)) {
            btl_runtime_error(vm, "String index must be a number."); return false;
        }
        ObjString* str = AS_STRING(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx >= str->length) {
            btl_runtime_error(vm, "String index out of bounds."); return false;
        }
        btl_push(vm, OBJ_VAL(btl_string_copy(vm, &str->chars[idx], 1)));
        return true;
    }
    btl_runtime_error(vm, "Only lists, tables, and strings can be indexed.");
    return false;
}

bool btl_compiled_index_set(VM* vm) {
    BtlValue value = btl_pop(vm);
    BtlValue key = btl_pop(vm);
    BtlValue obj = btl_pop(vm);

    if (IS_LIST(obj)) {
        if (!IS_NUMERIC(key)) {
            btl_runtime_error(vm, "List index must be a number."); return false;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx > l->items.count) {
            btl_runtime_error(vm, "List index out of bounds."); return false;
        }
        if (idx == l->items.count) btl_value_array_write(vm, &l->items, value);
        else {
            l->items.values[idx] = value; btl_gc_write_barrier(vm, (BtlObj*) l, value);
        }
        btl_push(vm, value);
        return true;
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        btl_table_set(vm, &table->table, key, value);
        btl_gc_write_barrier(vm, (BtlObj*) table, value);
        btl_gc_write_barrier(vm, (BtlObj*) table, key);
        btl_push(vm, value);
        return true;
    } else if (IS_STRING(obj)) {
        btl_runtime_error(vm, "Strings are immutable.");
        return false;
    }
    btl_runtime_error(vm, "Only lists and tables can be indexed for assignment.");
    return false;
}

// ----------------------------------------------------------------------------
// Modules
// ----------------------------------------------------------------------------

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

bool btl_compiled_import(VM* vm, BtlCallFrame* frame, int nameIdx) {
    ObjString* fName = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

    // Check native modules
    BtlValue nativeModule;
    if (btl_table_get(&vm->nativeModules, OBJ_VAL(fName), &nativeModule)) {
        btl_push(vm, nativeModule);
        return true;
    }

    // Check already-loaded modules
    BtlValue mVal;
    if (btl_table_get(&vm->modules, OBJ_VAL(fName), &mVal)) {
        btl_push(vm, mVal);
        return true;
    }

    // File-based import: read, compile, execute
    char* src = btl_compiled_readFile(vm, fName->chars);
    if (!src) {
        btl_runtime_error(vm, "Could not open file \"%s\".", fName->chars);
        return false;
    }

    size_t srcLen = strlen(src);
    ObjModule* m = btl_module_new(vm, fName);
    ObjFunction* f = btl_compile(vm, m, src);
    btl_realloc(vm, src, srcLen + 1, 0);

    if (!f) return false;

    ObjClosure* c = btl_closure_new(vm, f);
    btl_push(vm, OBJ_VAL(c));

    if (!btl_call_value(vm, OBJ_VAL(c), 0)) return false;

    // Set module on frame slots[0] and register BEFORE running (matches VM)
    vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
    btl_table_set(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));

    // Run the imported module via the interpreter with runFloor guard
    int savedFloor = vm->runFloor;
    vm->runFloor = vm->frameCount - 1;
    BtlInterpretResult result = btl_run(vm);
    vm->runFloor = savedFloor;
    if (result != BTL_INTERPRET_OK) return false;

    btl_push(vm, OBJ_VAL(m));
    return true;
}

bool btl_compiled_import_long(VM* vm, BtlCallFrame* frame, int nameIdx) {
    return btl_compiled_import(vm, frame, nameIdx);
}

// ----------------------------------------------------------------------------
// Actors
// ----------------------------------------------------------------------------

static inline BtlValue peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

// Async closure task - mirrors AsyncCallTask in vm.c
typedef struct {
    VM* vm;
    ObjClosure* closure;
    BtlValue* args;
    int argCount;
    ObjFuture* future;
} CompiledAsyncCallTask;

static void compiledAsyncCallRun(void* arg) {
    CompiledAsyncCallTask* task = (CompiledAsyncCallTask*) arg;
    VM* vm = task->vm;
    BTLRuntime* runtime = vm->runtime;
    int savedArgCount = task->argCount;

    btl_push(vm, OBJ_VAL(task->closure));
    for (int i = 0; i < task->argCount; i++) {
        btl_push(vm, task->args[i]);
    }

    BtlCallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = task->closure;
    frame->ip = task->closure->function->chunk.code;
    frame->slots = vm->stack;
    frame->openUpvalues = NULL;

    BtlInterpretResult result = btl_run(vm);

    if (result == BTL_INTERPRET_OK) {
        btl_future_resolve(task->future, vm->lastReturnValue);
    } else {
        ObjString* errMsg = btl_string_copy(vm, "Async call failed", 17);
        btl_future_reject(task->future, OBJ_VAL(errMsg));
    }

    if (task->args != NULL) {
        btl_realloc(vm, task->args, sizeof(BtlValue) * savedArgCount, 0);
    }

    btl_vm_free(vm, false);

    btl_runtime_alloc(runtime, vm, sizeof(VM), 0);
    btl_runtime_alloc(runtime, task, sizeof(CompiledAsyncCallTask), 0);
}

bool btl_compiled_do_new(VM* vm, int argCount) {
    BtlValue callee = peek(vm, argCount);

    if (IS_CLASS(callee)) {
        ObjClass* klass = AS_CLASS(callee);

        // Collect args from stack
        BtlValue* args = NULL;
        if (argCount > 0) {
            args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                args[i] = peek(vm, argCount - 1 - i);
            }
        }

        for (int i = 0; i <= argCount; i++) btl_pop(vm);

        ObjActor* actor = btl_actor_new(vm, klass, args, argCount);

        if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);

        btl_push(vm, OBJ_VAL(actor));
    } else if (IS_CLOSURE(callee)) {
        ObjClosure* closure = AS_CLOSURE(callee);
        ObjFuture* future = btl_future_new(vm);

        VM* asyncVM = btl_realloc(vm, NULL, 0, sizeof(VM));
        asyncVM->runtime = vm->runtime;
        btl_vm_init(asyncVM);

        asyncVM->stringClass = vm->stringClass;
        asyncVM->numberClass = vm->numberClass;
        asyncVM->listClass = vm->listClass;
        asyncVM->tableClass = vm->tableClass;
        asyncVM->rootModule = closure->function->module;

        CompiledAsyncCallTask* task = btl_realloc(vm, NULL, 0, sizeof(CompiledAsyncCallTask));
        task->vm = asyncVM;
        task->closure = closure;
        task->argCount = argCount;
        task->future = future;

        if (argCount > 0) {
            task->args = (BtlValue*)btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                task->args[i] = btl_deep_copy_value(asyncVM, vm, peek(vm, argCount - 1 - i));
            }
        } else {
            task->args = NULL;
        }

        for (int i = 0; i <= argCount; i++) btl_pop(vm);

        btl_threadpool_submit(vm->runtime->pool, compiledAsyncCallRun, task);

        btl_push(vm, OBJ_VAL(future));
    } else {
        btl_runtime_error(vm, "Can only use 'do' with classes or functions.");
        return false;
    }
    return true;
}

bool btl_compiled_do_invoke(VM* vm, BtlCallFrame* frame, int nameConst, int argCount) {
    ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameConst]);
    BtlValue actorVal = peek(vm, argCount);

    if (IS_NULL(actorVal)) {
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, BTL_NULL_VAL);
        return true;
    }

    if (!IS_ACTOR(actorVal)) {
        btl_runtime_error(vm, "Expected actor for 'do' method call.");
        return false;
    }

    ObjActor* actor = AS_ACTOR(actorVal);

    if (!actor->alive) {
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, BTL_NULL_VAL);
        return true;
    }

    ObjFuture* future = btl_future_new(vm);

    BtlValue* args = NULL;
    if (argCount > 0) {
        args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
        for (int i = 0; i < argCount; i++) {
            args[i] = peek(vm, argCount - 1 - i);
        }
    }

    btl_actor_send(actor, methodName, args, argCount, future);

    if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);

    for (int i = 0; i <= argCount; i++) btl_pop(vm);
    btl_push(vm, OBJ_VAL(future));
    return true;
}