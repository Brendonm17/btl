#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"
#include "runtime.h"

// ============================================================================
// CORE ALLOCATION - Runtime Level (before VM exists)
// ============================================================================

void* btl_runtime_alloc(BTLRuntime* runtime, void* pointer, size_t oldSize, size_t newSize) {
    // Free case
    if (newSize == 0) {
        if (pointer != NULL) {
            if (runtime != NULL && runtime->config.free != NULL) {
                runtime->config.free(pointer, oldSize, runtime->config.user_data);
            } else {
                free(pointer);
            }
        }
        return NULL;
    }

    void* result = NULL;

    if (runtime != NULL) {
        BTLConfig* cfg = &runtime->config;

        if (pointer == NULL) {
            // Fresh allocation
            if (cfg->alloc != NULL) {
                result = cfg->alloc(newSize, cfg->user_data);
            } else {
                result = malloc(newSize);
            }
        } else {
            // Reallocation
            if (cfg->realloc != NULL) {
                result = cfg->realloc(pointer, oldSize, newSize, cfg->user_data);
            } else if (cfg->alloc != NULL && cfg->free != NULL) {
                // Simulate realloc
                result = cfg->alloc(newSize, cfg->user_data);
                if (result != NULL) {
                    size_t copySize = oldSize < newSize ? oldSize : newSize;
                    memcpy(result, pointer, copySize);
                    cfg->free(pointer, oldSize, cfg->user_data);
                }
            } else {
                result = realloc(pointer, newSize);
            }
        }
    } else {
        // No runtime, use system allocator
        result = realloc(pointer, newSize);
    }

    if (result == NULL && newSize > 0) {
        fprintf(stderr, "btl: out of memory\n");
        exit(1);
    }

    return result;
}

// ============================================================================
// CORE ALLOCATION - VM Level (most common)
// ============================================================================

void* btl_realloc(VM* vm, void* pointer, size_t oldSize, size_t newSize) {
    // Update VM allocation tracking
    if (vm != NULL) {
        vm->bytesAllocated += newSize - oldSize;

        // Check max heap size (if configured and this is a growth)
        if (newSize > oldSize && vm->runtime != NULL) {
            size_t maxHeap = vm->runtime->config.max_heap_size;
            if (maxHeap > 0 && vm->bytesAllocated > maxHeap) {
                // Try GC first
                if (vm->gcInhibit == 0 && !vm->inMinorGC) {
                    majorGC(vm);
                    // Check again after GC
                    if (vm->bytesAllocated > maxHeap) {
                        btl_error(vm, "btl: max heap size exceeded\n");
                        exit(1);
                    }
                }
            }
        }
    }

#ifdef DEBUG_STRESS_GC
    static int stressCounter = 0;
    if (vm != NULL && newSize > oldSize && vm->gcInhibit == 0 && !vm->inMinorGC) {
        stressCounter++;
        if (stressCounter >= 10) {
            stressCounter = 0;
            if (vm->nurseryAllocated > 0 && vm->nursery.fromSpace != NULL) {
                minorGC(vm);
            }
        }
    }
#endif

    // Trigger major GC based on old gen growth
    if (vm != NULL && newSize > oldSize && !vm->inMinorGC && vm->gcInhibit == 0) {
        if (vm->bytesAllocated > vm->nextGC) {
            majorGC(vm);
        }
    }

    // Route through runtime allocator
    BTLRuntime* runtime = (vm != NULL) ? vm->runtime : NULL;
    return btl_runtime_alloc(runtime, pointer, oldSize, newSize);
}

// ============================================================================
// I/O FUNCTIONS - Runtime Level
// ============================================================================

void btl_runtime_print(BTLRuntime* runtime, const char* text) {
    if (runtime != NULL && runtime->config.print != NULL) {
        runtime->config.print(text, runtime->config.user_data);
    } else {
        printf("%s", text);
        fflush(stdout);
    }
}

void btl_runtime_error(BTLRuntime* runtime, const char* text) {
    if (runtime != NULL && runtime->config.error != NULL) {
        runtime->config.error(text, runtime->config.user_data);
    } else {
        fprintf(stderr, "%s", text);
        fflush(stderr);
    }
}

// ============================================================================
// I/O FUNCTIONS - VM Level
// ============================================================================

void btl_print(VM* vm, const char* text) {
    BTLRuntime* runtime = (vm != NULL) ? vm->runtime : NULL;
    btl_runtime_print(runtime, text);
}

void btl_println(VM* vm, const char* text) {
    btl_print(vm, text);
    btl_print(vm, "\n");
}

void btl_vprintf(VM* vm, const char* format, va_list args) {
    char buffer[4096];
    vsnprintf(buffer, sizeof(buffer), format, args);
    btl_print(vm, buffer);
}

void btl_printf(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    btl_vprintf(vm, format, args);
    va_end(args);
}

void btl_error(VM* vm, const char* text) {
    BTLRuntime* runtime = (vm != NULL) ? vm->runtime : NULL;
    btl_runtime_error(runtime, text);
}

void btl_errorln(VM* vm, const char* text) {
    btl_error(vm, text);
    btl_error(vm, "\n");
}

void btl_verrorf(VM* vm, const char* format, va_list args) {
    char buffer[4096];
    vsnprintf(buffer, sizeof(buffer), format, args);
    btl_error(vm, buffer);
}

void btl_errorf(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    btl_verrorf(vm, format, args);
    va_end(args);
}

// ============================================================================
// VALUE PRINTING
// ============================================================================

void btl_print_value(VM* vm, Value value) {
    if (IS_BOOL(value)) {
        btl_print(vm, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        btl_print(vm, "null");
    } else if (IS_NUMBER(value)) {
        btl_printf(vm, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            btl_print(vm, AS_CSTRING(value));
            break;
        case OBJ_FUNCTION: {
            ObjFunction* func = AS_FUNCTION(value);
            if (func->name == NULL) {
                btl_print(vm, "<script>");
            } else {
                btl_printf(vm, "<func %s>", func->name->chars);
            }
            break;
        }
        case OBJ_CLOSURE: {
            ObjFunction* func = AS_CLOSURE(value)->function;
            if (func->name == NULL) {
                btl_print(vm, "<script>");
            } else {
                btl_printf(vm, "<func %s>", func->name->chars);
            }
            break;
        }
        case OBJ_CLASS:
            btl_printf(vm, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            btl_printf(vm, "<%s instance>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_BOUND_METHOD: {
            ObjFunction* func = AS_BOUND_METHOD(value)->method->function;
            if (func->name == NULL) {
                btl_print(vm, "<bound method>");
            } else {
                btl_printf(vm, "<bound method %s>", func->name->chars);
            }
            break;
        }
        case OBJ_LIST:
            btl_print(vm, "<list>");
            break;
        case OBJ_TABLE:
            btl_print(vm, "<table>");
            break;
        case OBJ_MODULE:
            btl_printf(vm, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case OBJ_NATIVE:
            btl_print(vm, "<native func>");
            break;
        case OBJ_NATIVE_CLASS:
            btl_printf(vm, "<native class %s>", AS_NATIVE_CLASS(value)->name->chars);
            break;
        case OBJ_NATIVE_MODULE:
            btl_printf(vm, "<native module %s>", AS_NATIVE_MODULE(value)->name->chars);
            break;
        case OBJ_NATIVE_METHOD:
            btl_printf(vm, "<native method %s>", AS_NATIVE_METHOD(value)->name->chars);
            break;
        case OBJ_FUTURE:
            btl_print(vm, "<future>");
            break;
        case OBJ_ACTOR: {
            ObjActor* actor = AS_ACTOR(value);
            if (actor->alive) {
                btl_printf(vm, "<actor:%s>", actor->klass->name->chars);
            } else {
                btl_print(vm, "<actor:dead>");
            }
            break;
        }
        case OBJ_UPVALUE:
            btl_print(vm, "<upvalue>");
            break;
        default:
            btl_print(vm, "<object>");
            break;
        }
    }
}

void btl_error_value(VM* vm, Value value) {
    if (IS_BOOL(value)) {
        btl_error(vm, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        btl_error(vm, "null");
    } else if (IS_NUMBER(value)) {
        btl_errorf(vm, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            btl_error(vm, AS_CSTRING(value));
            break;
        default:
            btl_error(vm, "<object>");
            break;
        }
    }
}

// ============================================================================
// NURSERY MANAGEMENT
// ============================================================================

void initNursery(VM* vm, Nursery* nursery) {
    // Get nursery size from config, or use default
    size_t size = DEFAULT_NURSERY_SIZE;
    if (vm->runtime != NULL && vm->runtime->config.nursery_size > 0) {
        size = vm->runtime->config.nursery_size;
    }

    nursery->size = size;
    // Use runtime allocator for nursery spaces
    nursery->fromSpace = (uint8_t*) btl_realloc(vm, NULL, 0, size);
    nursery->toSpace = (uint8_t*) btl_realloc(vm, NULL, 0, size);
    nursery->allocPtr = nursery->fromSpace;
    nursery->limit = nursery->fromSpace + size;

    if (nursery->fromSpace == NULL || nursery->toSpace == NULL) {
        btl_error(vm, "Failed to allocate nursery\n");
        exit(1);
    }
}

void freeNursery(VM* vm, Nursery* nursery) {
    if (nursery->fromSpace != NULL) {
        btl_realloc(vm, nursery->fromSpace, nursery->size, 0);
    }
    if (nursery->toSpace != NULL) {
        btl_realloc(vm, nursery->toSpace, nursery->size, 0);
    }
    nursery->fromSpace = NULL;
    nursery->toSpace = NULL;
    nursery->allocPtr = NULL;
    nursery->limit = NULL;
    nursery->size = 0;
}

static void* nurseryAlloc(VM* vm, size_t size) {
    size = (size + 7) & ~7;

    if (vm->nursery.allocPtr + size > vm->nursery.limit) {
        return NULL;
    }

    void* ptr = vm->nursery.allocPtr;
    vm->nursery.allocPtr += size;
    vm->nurseryAllocated += size;
    return ptr;
}

// ============================================================================
// REMEMBERED SET
// ============================================================================

void initRememberedSet(RememberedSet* set) {
    set->entries = NULL;
    set->count = 0;
    set->capacity = 0;
}

void freeRememberedSet(VM* vm, RememberedSet* set) {
    if (set->entries != NULL) {
        FREE_ARRAY(vm, RememberedEntry, set->entries, set->capacity);
    }
    set->entries = NULL;
    set->count = 0;
    set->capacity = 0;
}

void rememberObject(VM* vm, Obj* object) {
    for (int i = 0; i < vm->rememberedSet.count; i++) {
        if (vm->rememberedSet.entries[i].object == object) {
            return;
        }
    }

    if (vm->rememberedSet.count >= vm->rememberedSet.capacity) {
        int oldCapacity = vm->rememberedSet.capacity;
        vm->rememberedSet.capacity = GROW_CAPACITY(oldCapacity);
        vm->rememberedSet.entries = GROW_ARRAY(vm, RememberedEntry,
            vm->rememberedSet.entries, oldCapacity, vm->rememberedSet.capacity);
    }

    vm->rememberedSet.entries[vm->rememberedSet.count].object = object;
    vm->rememberedSet.entries[vm->rememberedSet.count].next = NULL;
    vm->rememberedSet.count++;
}

// ============================================================================
// GC INHIBIT
// ============================================================================

void gcInhibitStart(VM* vm) {
    vm->gcInhibit++;
}

void gcInhibitEnd(VM* vm) {
    vm->gcInhibit--;
}

// ============================================================================
// WRITE BARRIER
// ============================================================================

void writeBarrier(VM* vm, Obj* container, Value value) {
    if (!IS_OBJ(value)) return;
    Obj* child = AS_OBJ(value);
    if (container->generation == GEN_OLD && child->generation == GEN_NURSERY) {
        rememberObject(vm, container);
    }
}

// ============================================================================
// OBJECT ALLOCATION
// ============================================================================

static Obj* allocateInOldGen(VM* vm, size_t size, ObjType type) {
    Obj* object = (Obj*) btl_realloc(vm, NULL, 0, size);
    object->type = type;
    object->isMarked = false;
    object->generation = GEN_OLD;
    object->next = vm->objects;
    object->forwarding = NULL;
    vm->objects = object;
    return object;
}

void* allocateObject(VM* vm, size_t size, ObjType type) {
    if (vm->inMinorGC) {
        return allocateInOldGen(vm, size, type);
    }

    // Calculate large object threshold based on actual nursery size
    size_t largeObjSize = LARGE_OBJECT_SIZE_FOR(vm->nursery.size);
    if (size >= largeObjSize) {
        return allocateInOldGen(vm, size, type);
    }

    if (type == OBJ_STRING) {
        return allocateInOldGen(vm, size, type);
    }

    if (type == OBJ_MODULE || type == OBJ_CLASS || type == OBJ_FUNCTION ||
        type == OBJ_CLOSURE || type == OBJ_NATIVE || type == OBJ_NATIVE_CLASS ||
        type == OBJ_NATIVE_MODULE || type == OBJ_NATIVE_METHOD ||
        type == OBJ_ACTOR || type == OBJ_FUTURE) {
        return allocateInOldGen(vm, size, type);
    }

    Obj* object = (Obj*) nurseryAlloc(vm, size);

    if (object == NULL) {
        if (vm->gcInhibit == 0) {
            minorGC(vm);
            object = (Obj*) nurseryAlloc(vm, size);
        }

        if (object == NULL) {
            return allocateInOldGen(vm, size, type);
        }
    }

    object->type = type;
    object->isMarked = false;
    object->generation = GEN_NURSERY;
    object->next = NULL;
    object->forwarding = NULL;

    return object;
}

// ============================================================================
// MARKING
// ============================================================================

void markObject(VM* vm, Obj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;

    object->isMarked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        // Gray stack uses runtime allocator
        vm->grayStack = (Obj**) btl_realloc(vm, vm->grayStack,
            sizeof(Obj*) * (vm->grayCapacity / 2),
            sizeof(Obj*) * vm->grayCapacity);
        if (vm->grayStack == NULL) exit(1);
    }

    vm->grayStack[vm->grayCount++] = object;
}

void markValue(VM* vm, Value value) {
    if (IS_OBJ(value)) markObject(vm, AS_OBJ(value));
}

static void markClass(VM* vm, ObjClass* klass) {
    markObject(vm, (Obj*) klass->name);
    markTable(vm, &klass->methodIndices);
    markTable(vm, &klass->fieldIndices);

    for (int i = 0; i < klass->methodCount; i++) {
        if (klass->methods[i].closure != NULL) {
            markObject(vm, (Obj*) klass->methods[i].closure);
            markObject(vm, (Obj*) klass->methods[i].name);
        }
    }
}

static void markArray(VM* vm, ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(vm, array->values[i]);
    }
}

static void blackenObject(VM* vm, Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD: {
        ObjBoundMethod* b = (ObjBoundMethod*) object;
        markValue(vm, b->receiver);
        markObject(vm, (Obj*) b->method);
        break;
    }
    case OBJ_CLASS:
        markClass(vm, (ObjClass*) object);
        break;
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        markObject(vm, (Obj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) {
            RuntimeUpvalue* uv = &closure->upvalues[i];
            if (!uv->isOpen) {
                if (uv->isMutable) {
                    markObject(vm, (Obj*) uv->loc.box);
                } else {
                    markValue(vm, uv->loc.immValue);
                }
            }
        }
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        markObject(vm, (Obj*) f->name);
        markArray(vm, &f->chunk.constants);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        markObject(vm, (Obj*) instance->klass);
        for (int i = 0; i < instance->klass->fieldCount; i++) {
            markValue(vm, instance->fields[i]);
        }
        break;
    }
    case OBJ_LIST: {
        ObjList* list = (ObjList*) object;
        for (int i = 0; i < list->items.count; i++) {
            markValue(vm, list->items.values[i]);
        }
        break;
    }
    case OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        markObject(vm, (Obj*) m->name);
        markTable(vm, &m->globalNames);
        markArray(vm, &m->globalValues);
        break;
    }
    case OBJ_UPVALUE:
        markValue(vm, ((ObjUpvalue*) object)->closed);
        break;
    case OBJ_TABLE:
        markTable(vm, &((ObjTable*) object)->table);
        break;
    case OBJ_NATIVE_METHOD:
        markObject(vm, (Obj*) ((ObjNativeMethod*) object)->name);
        break;
    case OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        markObject(vm, (Obj*) klass->name);
        markTable(vm, &klass->methods);
        break;
    }
    case OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        markObject(vm, (Obj*) module->name);
        markTable(vm, &module->globals);
        break;
    }
    case OBJ_FUTURE:
        markValue(vm, ((ObjFuture*) object)->result);
        break;
    case OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        markValue(vm, actor->instance);
        break;
    }
    case OBJ_NATIVE:
    case OBJ_STRING:
        break;
    }
}

// ============================================================================
// MINOR GC
// ============================================================================

static Obj* promoteObject(VM* vm, Obj* object) {
    if (object == NULL) return NULL;
    if (object->generation != GEN_NURSERY) return object;
    if (object->forwarding != NULL) return object->forwarding;

    size_t size;
    switch (object->type) {
    case OBJ_BOUND_METHOD: size = sizeof(ObjBoundMethod); break;
    case OBJ_CLASS: size = sizeof(ObjClass); break;
    case OBJ_CLOSURE: {
        ObjClosure* c = (ObjClosure*) object;
        size = sizeof(ObjClosure) + sizeof(RuntimeUpvalue) * c->upvalueCount;
        break;
    }
    case OBJ_FUNCTION: size = sizeof(ObjFunction); break;
    case OBJ_INSTANCE: size = sizeof(ObjInstance); break;
    case OBJ_LIST: size = sizeof(ObjList); break;
    case OBJ_MODULE: size = sizeof(ObjModule); break;
    case OBJ_NATIVE: size = sizeof(ObjNative); break;
    case OBJ_STRING: size = sizeof(ObjString); break;
    case OBJ_UPVALUE: size = sizeof(ObjUpvalue); break;
    case OBJ_TABLE: size = sizeof(ObjTable); break;
    case OBJ_NATIVE_METHOD: size = sizeof(ObjNativeMethod); break;
    case OBJ_NATIVE_CLASS: size = sizeof(ObjNativeClass); break;
    case OBJ_NATIVE_MODULE: size = sizeof(ObjNativeModule); break;
    case OBJ_FUTURE: size = sizeof(ObjFuture); break;
    case OBJ_ACTOR: size = sizeof(ObjActor); break;
    default: size = sizeof(Obj); break;
    }

    // Use runtime allocator for promoted objects
    Obj* copy = (Obj*) btl_realloc(vm, NULL, 0, size);
    if (copy == NULL) exit(1);

    memcpy(copy, object, size);

    copy->generation = GEN_OLD;
    copy->next = vm->objects;
    copy->forwarding = NULL;
    copy->isMarked = false;
    vm->objects = copy;

    object->forwarding = copy;
    vm->promotedBytes += size;

    // Duplicate external allocations
    switch (object->type) {
    case OBJ_CLOSURE: {
        ObjClosure* oldClosure = (ObjClosure*) object;
        ObjClosure* newClosure = (ObjClosure*) copy;

        if (oldClosure->fieldICs != NULL && oldClosure->function->fieldICCount > 0) {
            size_t icSize = sizeof(FieldIC) * oldClosure->function->fieldICCount;
            newClosure->fieldICs = btl_realloc(vm, NULL, 0, icSize);
            memcpy(newClosure->fieldICs, oldClosure->fieldICs, icSize);
        }

        if (oldClosure->methodICs != NULL && oldClosure->function->methodICCount > 0) {
            size_t icSize = sizeof(MethodIC) * oldClosure->function->methodICCount;
            newClosure->methodICs = btl_realloc(vm, NULL, 0, icSize);
            memcpy(newClosure->methodICs, oldClosure->methodICs, icSize);
        }
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* oldFunc = (ObjFunction*) object;
        ObjFunction* newFunc = (ObjFunction*) copy;

        if (oldFunc->chunk.code != NULL && oldFunc->chunk.capacity > 0) {
            newFunc->chunk.code = btl_realloc(vm, NULL, 0, oldFunc->chunk.capacity);
            memcpy(newFunc->chunk.code, oldFunc->chunk.code, oldFunc->chunk.capacity);
        }

        if (oldFunc->chunk.lines != NULL && oldFunc->chunk.capacity > 0) {
            size_t lineSize = sizeof(int) * oldFunc->chunk.capacity;
            newFunc->chunk.lines = btl_realloc(vm, NULL, 0, lineSize);
            memcpy(newFunc->chunk.lines, oldFunc->chunk.lines, lineSize);
        }

        if (oldFunc->chunk.constants.values != NULL && oldFunc->chunk.constants.capacity > 0) {
            size_t constSize = sizeof(Value) * oldFunc->chunk.constants.capacity;
            newFunc->chunk.constants.values = btl_realloc(vm, NULL, 0, constSize);
            memcpy(newFunc->chunk.constants.values, oldFunc->chunk.constants.values, constSize);
        }
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* oldInst = (ObjInstance*) object;
        ObjInstance* newInst = (ObjInstance*) copy;

        if (oldInst->fields != NULL && oldInst->klass->fieldCount > 0) {
            size_t fieldSize = sizeof(Value) * oldInst->klass->fieldCount;
            newInst->fields = btl_realloc(vm, NULL, 0, fieldSize);
            memcpy(newInst->fields, oldInst->fields, fieldSize);
        }
        break;
    }
    case OBJ_LIST: {
        ObjList* oldList = (ObjList*) object;
        ObjList* newList = (ObjList*) copy;

        if (oldList->items.values != NULL && oldList->items.capacity > 0) {
            size_t itemSize = sizeof(Value) * oldList->items.capacity;
            newList->items.values = btl_realloc(vm, NULL, 0, itemSize);
            memcpy(newList->items.values, oldList->items.values, itemSize);
        }
        break;
    }
    case OBJ_CLASS: {
        ObjClass* oldClass = (ObjClass*) object;
        ObjClass* newClass = (ObjClass*) copy;

        if (oldClass->methods != NULL && oldClass->methodCapacity > 0) {
            size_t methodSize = sizeof(MethodEntry) * oldClass->methodCapacity;
            newClass->methods = btl_realloc(vm, NULL, 0, methodSize);
            memcpy(newClass->methods, oldClass->methods, methodSize);
        }

        if (oldClass->methodIndices.entries != NULL && oldClass->methodIndices.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldClass->methodIndices.capacity;
            newClass->methodIndices.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newClass->methodIndices.entries, oldClass->methodIndices.entries, entrySize);
        }

        if (oldClass->fieldIndices.entries != NULL && oldClass->fieldIndices.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldClass->fieldIndices.capacity;
            newClass->fieldIndices.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newClass->fieldIndices.entries, oldClass->fieldIndices.entries, entrySize);
        }
        break;
    }
    case OBJ_MODULE: {
        ObjModule* oldMod = (ObjModule*) object;
        ObjModule* newMod = (ObjModule*) copy;

        if (oldMod->globalNames.entries != NULL && oldMod->globalNames.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldMod->globalNames.capacity;
            newMod->globalNames.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newMod->globalNames.entries, oldMod->globalNames.entries, entrySize);
        }

        if (oldMod->globalValues.values != NULL && oldMod->globalValues.capacity > 0) {
            size_t valSize = sizeof(Value) * oldMod->globalValues.capacity;
            newMod->globalValues.values = btl_realloc(vm, NULL, 0, valSize);
            memcpy(newMod->globalValues.values, oldMod->globalValues.values, valSize);
        }

        if (oldMod->classInfo.entries != NULL && oldMod->classInfo.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldMod->classInfo.capacity;
            newMod->classInfo.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newMod->classInfo.entries, oldMod->classInfo.entries, entrySize);
        }
        break;
    }
    case OBJ_TABLE: {
        ObjTable* oldTable = (ObjTable*) object;
        ObjTable* newTable = (ObjTable*) copy;

        if (oldTable->table.entries != NULL && oldTable->table.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldTable->table.capacity;
            newTable->table.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newTable->table.entries, oldTable->table.entries, entrySize);
        }
        break;
    }
    case OBJ_NATIVE_CLASS: {
        ObjNativeClass* oldNC = (ObjNativeClass*) object;
        ObjNativeClass* newNC = (ObjNativeClass*) copy;

        if (oldNC->methods.entries != NULL && oldNC->methods.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldNC->methods.capacity;
            newNC->methods.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newNC->methods.entries, oldNC->methods.entries, entrySize);
        }
        break;
    }
    case OBJ_NATIVE_MODULE: {
        ObjNativeModule* oldNM = (ObjNativeModule*) object;
        ObjNativeModule* newNM = (ObjNativeModule*) copy;

        if (oldNM->globals.entries != NULL && oldNM->globals.capacity > 0) {
            size_t entrySize = sizeof(Entry) * oldNM->globals.capacity;
            newNM->globals.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newNM->globals.entries, oldNM->globals.entries, entrySize);
        }
        break;
    }
    default:
        break;
    }

    return copy;
}

static Value promoteValue(VM* vm, Value value) {
    if (!IS_OBJ(value)) return value;
    Obj* obj = AS_OBJ(value);
    Obj* promoted = promoteObject(vm, obj);
    return OBJ_VAL(promoted);
}

static void scanObject(VM* vm, Obj* object) {
    if (object == NULL) return;

    switch (object->type) {
    case OBJ_BOUND_METHOD: {
        ObjBoundMethod* b = (ObjBoundMethod*) object;
        b->receiver = promoteValue(vm, b->receiver);
        b->method = (ObjClosure*) promoteObject(vm, (Obj*) b->method);
        break;
    }
    case OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        klass->name = (ObjString*) promoteObject(vm, (Obj*) klass->name);
        for (int i = 0; i < klass->methodCount; i++) {
            if (klass->methods[i].closure != NULL) {
                klass->methods[i].closure = (ObjClosure*) promoteObject(vm, (Obj*) klass->methods[i].closure);
                klass->methods[i].name = (ObjString*) promoteObject(vm, (Obj*) klass->methods[i].name);
            }
        }
        if (klass->methodIndices.entries != NULL) {
            for (int i = 0; i < klass->methodIndices.capacity; i++) {
                Entry* entry = &klass->methodIndices.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        if (klass->fieldIndices.entries != NULL) {
            for (int i = 0; i < klass->fieldIndices.capacity; i++) {
                Entry* entry = &klass->fieldIndices.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        closure->function = (ObjFunction*) promoteObject(vm, (Obj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) {
            RuntimeUpvalue* uv = &closure->upvalues[i];
            if (!uv->isOpen) {
                if (uv->isMutable && uv->loc.box != NULL) {
                    uv->loc.box = (ObjUpvalue*) promoteObject(vm, (Obj*) uv->loc.box);
                } else if (!uv->isMutable) {
                    uv->loc.immValue = promoteValue(vm, uv->loc.immValue);
                }
            }
        }
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        f->name = (ObjString*) promoteObject(vm, (Obj*) f->name);
        f->module = (ObjModule*) promoteObject(vm, (Obj*) f->module);
        if (f->chunk.constants.values != NULL) {
            for (int i = 0; i < f->chunk.constants.count; i++) {
                f->chunk.constants.values[i] = promoteValue(vm, f->chunk.constants.values[i]);
            }
        }
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        instance->klass = (ObjClass*) promoteObject(vm, (Obj*) instance->klass);
        if (instance->fields != NULL && instance->klass != NULL) {
            for (int i = 0; i < instance->klass->fieldCount; i++) {
                instance->fields[i] = promoteValue(vm, instance->fields[i]);
            }
        }
        break;
    }
    case OBJ_LIST: {
        ObjList* list = (ObjList*) object;
        if (list->items.values != NULL) {
            for (int i = 0; i < list->items.count; i++) {
                list->items.values[i] = promoteValue(vm, list->items.values[i]);
            }
        }
        break;
    }
    case OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        m->name = (ObjString*) promoteObject(vm, (Obj*) m->name);
        if (m->globalValues.values != NULL) {
            for (int i = 0; i < m->globalValues.count; i++) {
                m->globalValues.values[i] = promoteValue(vm, m->globalValues.values[i]);
            }
        }
        if (m->globalNames.entries != NULL) {
            for (int i = 0; i < m->globalNames.capacity; i++) {
                Entry* entry = &m->globalNames.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        break;
    }
    case OBJ_UPVALUE: {
        ObjUpvalue* uv = (ObjUpvalue*) object;
        uv->closed = promoteValue(vm, uv->closed);
        break;
    }
    case OBJ_TABLE: {
        ObjTable* table = (ObjTable*) object;
        if (table->table.entries != NULL) {
            for (int i = 0; i < table->table.capacity; i++) {
                Entry* entry = &table->table.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case OBJ_FUTURE: {
        ObjFuture* future = (ObjFuture*) object;
        future->result = promoteValue(vm, future->result);
        break;
    }
    case OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        actor->instance = promoteValue(vm, actor->instance);
        actor->klass = (ObjClass*) promoteObject(vm, (Obj*) actor->klass);
        break;
    }
    case OBJ_NATIVE_METHOD: {
        ObjNativeMethod* method = (ObjNativeMethod*) object;
        method->name = (ObjString*) promoteObject(vm, (Obj*) method->name);
        break;
    }
    case OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        klass->name = (ObjString*) promoteObject(vm, (Obj*) klass->name);
        if (klass->methods.entries != NULL) {
            for (int i = 0; i < klass->methods.capacity; i++) {
                Entry* entry = &klass->methods.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        module->name = (ObjString*) promoteObject(vm, (Obj*) module->name);
        if (module->globals.entries != NULL) {
            for (int i = 0; i < module->globals.capacity; i++) {
                Entry* entry = &module->globals.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case OBJ_NATIVE:
    case OBJ_STRING:
        break;
    }
}

void minorGC(VM* vm) {
    if (vm->inMinorGC) return;
    if (vm->nurseryAllocated == 0 || vm->nursery.fromSpace == NULL) return;
    if (vm->gcInhibit > 0) return;
    vm->gcInhibit++;
    vm->inMinorGC = true;

#ifdef DEBUG_LOG_GC
    btl_errorf(vm, "-- minor gc begin (nursery: %zu bytes)\n", vm->nurseryAllocated);
    size_t promotedBefore = vm->promotedBytes;
#endif

    Obj* oldGenHead = vm->objects;

    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        *slot = promoteValue(vm, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++) {
        vm->frames[i].closure = (ObjClosure*) promoteObject(vm, (Obj*) vm->frames[i].closure);
    }

    if (vm->rootModule) {
        vm->rootModule = (ObjModule*) promoteObject(vm, (Obj*) vm->rootModule);
    }

    if (vm->stringClass) vm->stringClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->stringClass);
    if (vm->numberClass) vm->numberClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->numberClass);
    if (vm->listClass) vm->listClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->listClass);
    if (vm->tableClass) vm->tableClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->tableClass);

    if (vm->modules.entries != NULL) {
        for (int i = 0; i < vm->modules.capacity; i++) {
            Entry* entry = &vm->modules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    if (vm->nativeModules.entries != NULL) {
        for (int i = 0; i < vm->nativeModules.capacity; i++) {
            Entry* entry = &vm->nativeModules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    if (vm->initString) {
        vm->initString = (ObjString*) promoteObject(vm, (Obj*) vm->initString);
    }

    vm->lastReturnValue = promoteValue(vm, vm->lastReturnValue);

    for (int i = 0; i < vm->rememberedSet.count; i++) {
        scanObject(vm, vm->rememberedSet.entries[i].object);
    }

    bool madeProgress = true;
    while (madeProgress) {
        madeProgress = false;
        Obj* obj = vm->objects;
        while (obj != NULL && obj != oldGenHead) {
            if (!obj->isMarked) {
                obj->isMarked = true;
                scanObject(vm, obj);
                madeProgress = true;
            }
            obj = obj->next;
        }
    }

    for (Obj* obj = vm->objects; obj != NULL && obj != oldGenHead; obj = obj->next) {
        obj->isMarked = false;
    }

    vm->nursery.allocPtr = vm->nursery.fromSpace;
    vm->nurseryAllocated = 0;
    vm->rememberedSet.count = 0;

    vm->minorGCCount++;
    vm->inMinorGC = false;
    vm->gcInhibit--;

#ifdef DEBUG_LOG_GC
    btl_errorf(vm, "-- minor gc end (promoted %zu bytes)\n", vm->promotedBytes - promotedBefore);
#endif

    //if (vm->bytesAllocated > vm->nextGC) {
    //    majorGC(vm);
    //}
}

// ============================================================================
// MAJOR GC
// ============================================================================

static void freeObject(VM* vm, Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD:
        FREE(vm, ObjBoundMethod, object);
        break;
    case OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        if (klass->methods != NULL) {
            FREE_ARRAY(vm, MethodEntry, klass->methods, klass->methodCapacity);
        }
        freeTable(vm, &klass->methodIndices);
        freeTable(vm, &klass->fieldIndices);
        FREE(vm, ObjClass, object);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        if (closure->fieldICs != NULL) {
            FREE_ARRAY(vm, FieldIC, closure->fieldICs, closure->function->fieldICCount);
        }
        if (closure->methodICs != NULL) {
            FREE_ARRAY(vm, MethodIC, closure->methodICs, closure->function->methodICCount);
        }
        size_t size = sizeof(ObjClosure) + sizeof(RuntimeUpvalue) * closure->upvalueCount;
        btl_realloc(vm, object, size, 0);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        freeChunk(vm, &f->chunk);
        FREE(vm, ObjFunction, object);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        if (instance->fields != NULL) {
            FREE_ARRAY(vm, Value, instance->fields, instance->klass->fieldCount);
        }
        FREE(vm, ObjInstance, object);
        break;
    }
    case OBJ_LIST: {
        ObjList* l = (ObjList*) object;
        freeValueArray(vm, &l->items);
        FREE(vm, ObjList, object);
        break;
    }
    case OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        for (int i = 0; i < m->classInfo.capacity; i++) {
            Entry* entry = &m->classInfo.entries[i];
            if (!IS_EMPTY(entry->key)) {
                Table* savedTable = (Table*) (uintptr_t) AS_NUMBER(entry->value);
                freeTable(vm, savedTable);
                FREE(vm, Table, savedTable);
            }
        }
        freeTable(vm, &m->classInfo);
        freeTable(vm, &m->globalNames);
        freeValueArray(vm, &m->globalValues);
        FREE(vm, ObjModule, object);
        break;
    }
    case OBJ_NATIVE:
        FREE(vm, ObjNative, object);
        break;
    case OBJ_STRING: {
        ObjString* s = (ObjString*) object;
        FREE_ARRAY(vm, char, s->chars, s->length + 1);
        FREE(vm, ObjString, object);
        break;
    }
    case OBJ_UPVALUE:
        FREE(vm, ObjUpvalue, object);
        break;
    case OBJ_TABLE: {
        ObjTable* table = (ObjTable*) object;
        freeTable(vm, &table->table);
        FREE(vm, ObjTable, object);
        break;
    }
    case OBJ_NATIVE_METHOD:
        FREE(vm, ObjNativeMethod, object);
        break;
    case OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        freeTable(vm, &klass->methods);
        FREE(vm, ObjNativeClass, object);
        break;
    }
    case OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        freeTable(vm, &module->globals);
        FREE(vm, ObjNativeModule, object);
        break;
    }
    case OBJ_FUTURE: {
        ObjFuture* future = (ObjFuture*) object;
        freeFuture(future);
        FREE(vm, ObjFuture, object);
        break;
    }
    case OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        actorStop(actor);
        FREE(vm, ObjActor, object);
        break;
    }
    }
}

static void markRoots(VM* vm) {
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        markValue(vm, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++) {
        markObject(vm, (Obj*) vm->frames[i].closure);
    }

    markTable(vm, &vm->modules);

    if (vm->rootModule) markObject(vm, (Obj*) vm->rootModule);

    if (vm->stringClass != NULL) markObject(vm, (Obj*) vm->stringClass);
    if (vm->numberClass != NULL) markObject(vm, (Obj*) vm->numberClass);
    if (vm->listClass != NULL) markObject(vm, (Obj*) vm->listClass);
    if (vm->tableClass != NULL) markObject(vm, (Obj*) vm->tableClass);
    markTable(vm, &vm->nativeModules);

    markCompilerRoots(vm);
    markObject(vm, (Obj*) vm->initString);
    markValue(vm, vm->lastReturnValue);
}

void majorGC(VM* vm) {
    if (vm->gcInhibit > 0) return;

    vm->gcInhibit++;

#ifdef DEBUG_LOG_GC
    btl_error(vm, "-- major gc begin\n");
    size_t before = vm->bytesAllocated;
#endif

    markRoots(vm);

    while (vm->grayCount > 0) {
        Obj* object = vm->grayStack[--vm->grayCount];
        blackenObject(vm, object);
    }

    tableRemoveWhite(&vm->strings);

    Obj* previous = NULL;
    Obj* object = vm->objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm->objects = object;
            }
            freeObject(vm, unreached);
        }
    }

    // Use grow factor from config, or default
    float growFactor = DEFAULT_GC_HEAP_GROW_FACTOR;
    if (vm->runtime != NULL && vm->runtime->config.gc_grow_factor > 0) {
        growFactor = vm->runtime->config.gc_grow_factor;
    }
    vm->nextGC = (size_t) (vm->bytesAllocated * growFactor);

    vm->majorGCCount++;

#ifdef DEBUG_LOG_GC
    btl_errorf(vm, "-- major gc end (collected %zu bytes, %zu -> %zu)\n",
        before - vm->bytesAllocated, before, vm->bytesAllocated);
#endif

    vm->gcInhibit--;
}

void collectGarbage(VM* vm) {
    size_t threshold = NURSERY_THRESHOLD_FOR(vm->nursery.size);
    if (vm->nurseryAllocated > threshold) {
        minorGC(vm);
    } else {
        majorGC(vm);
    }
}

// ============================================================================
// CLEANUP
// ============================================================================

void freeObjects(VM* vm) {
    Obj* object = vm->objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(vm, object);
        object = next;
    }

    if (vm->grayStack != NULL) {
        btl_realloc(vm, vm->grayStack, sizeof(Obj*) * vm->grayCapacity, 0);
        vm->grayStack = NULL;
    }

    freeNursery(vm, &vm->nursery);
    freeRememberedSet(vm, &vm->rememberedSet);
}