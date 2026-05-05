#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"
#include "runtime.h"

// Runtime-level allocation. Used before the VM exists, or by the runtime
// itself. Routes through BTLConfig.platform.mem if available.
void* btl_runtime_alloc(BTLRuntime* runtime, void* pointer, size_t oldSize, size_t newSize) {
    if (runtime != NULL) {
        BtlMemoryHandles* mem = &runtime->config.platform.mem;

        // Free case
        if (newSize == 0) {
            if (pointer != NULL) {
                mem->free(pointer, oldSize, mem->user_data);
            }
            return NULL;
        }

        // Allocate/reallocate
        void* result = mem->realloc(pointer, oldSize, newSize, mem->user_data);

        if (result == NULL && newSize > 0) {
            // Use platform I/O for error message
            runtime->config.platform.io.error("btl: out of memory\n",
                runtime->config.platform.io.user_data);
            exit(1);
        }

        return result;
    }

    // No runtime - use system allocator directly
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL && newSize > 0) {
        fprintf(stderr, "btl: out of memory\n");
        exit(1);
    }

    return result;
}

// ============================================================================
// Core Allocation - VM Level
//
// Most common allocation path. Tracks allocation for GC and routes through
// runtime allocator.
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
                    btl_gc_major(vm);
                    // Check again after GC
                    if (vm->bytesAllocated > maxHeap) {
                        btl_error(vm, "btl: max heap size exceeded\n");
                        exit(1);
                    }
                }
            }
        }
    }

#ifdef BTL_DEBUG_STRESS_GC
    static int stressCounter = 0;
    if (vm != NULL && newSize > oldSize && vm->gcInhibit == 0 && !vm->inMinorGC) {
        stressCounter++;
        if (stressCounter >= 10) {
            stressCounter = 0;
            if (vm->nurseryAllocated > 0 && vm->nursery.fromSpace != NULL) {
                btl_gc_minor(vm);
            }
        }
    }
#endif

    // Trigger major GC based on old gen growth
    if (vm != NULL && newSize > oldSize && !vm->inMinorGC && vm->gcInhibit == 0) {
        if (vm->bytesAllocated > vm->nextGC) {
            btl_gc_major(vm);
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
    if (runtime != NULL) {
        BtlIOHandles* io = &runtime->config.platform.io;
        io->print(text, io->user_data);
    } else {
        printf("%s", text);
        fflush(stdout);
    }
}

void btl_runtime_err_print(BTLRuntime* runtime, const char* text) {
    if (runtime != NULL) {
        BtlIOHandles* io = &runtime->config.platform.io;
        io->error(text, io->user_data);
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
    btl_runtime_err_print(runtime, text);
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

void btl_print_value(VM* vm, BtlValue value) {
    if (IS_BOOL(value)) {
        btl_print(vm, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        btl_print(vm, "null");
    } else if (IS_INT(value)) {
        btl_printf(vm, "%" PRId64, AS_INT(value));
    } else if (IS_NUMBER(value)) {
        btl_printf(vm, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case BTL_OBJ_STRING:
            btl_print(vm, AS_CSTRING(value));
            break;
        case BTL_OBJ_FUNCTION: {
            ObjFunction* func = AS_FUNCTION(value);
            if (func->name == NULL) {
                btl_print(vm, "<script>");
            } else {
                btl_printf(vm, "<func %s>", func->name->chars);
            }
            break;
        }
        case BTL_OBJ_CLOSURE: {
            ObjFunction* func = AS_CLOSURE(value)->function;
            if (func->name == NULL) {
                btl_print(vm, "<script>");
            } else {
                btl_printf(vm, "<func %s>", func->name->chars);
            }
            break;
        }
        case BTL_OBJ_CLASS:
            btl_printf(vm, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case BTL_OBJ_INSTANCE:
            btl_printf(vm, "<%s instance>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case BTL_OBJ_BOUND_METHOD: {
            ObjFunction* func = AS_BOUND_METHOD(value)->method->function;
            if (func->name == NULL) {
                btl_print(vm, "<bound method>");
            } else {
                btl_printf(vm, "<bound method %s>", func->name->chars);
            }
            break;
        }
        case BTL_OBJ_LIST:
            btl_print(vm, "<list>");
            break;
        case BTL_OBJ_TABLE:
            btl_print(vm, "<table>");
            break;
        case BTL_OBJ_MODULE:
            btl_printf(vm, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case BTL_OBJ_NATIVE:
            btl_print(vm, "<native func>");
            break;
        case BTL_OBJ_NATIVE_CLASS:
            btl_printf(vm, "<native class %s>", AS_NATIVE_CLASS(value)->name->chars);
            break;
        case BTL_OBJ_NATIVE_MODULE:
            btl_printf(vm, "<native module %s>", AS_NATIVE_MODULE(value)->name->chars);
            break;
        case BTL_OBJ_NATIVE_METHOD:
            btl_printf(vm, "<native method %s>", AS_NATIVE_METHOD(value)->name->chars);
            break;
        case BTL_OBJ_FUTURE:
            btl_print(vm, "<future>");
            break;
        case BTL_OBJ_ACTOR: {
            ObjActor* actor = AS_ACTOR(value);
            if (actor->alive) {
                btl_printf(vm, "<actor:%s>", actor->klass->name->chars);
            } else {
                btl_print(vm, "<actor:dead>");
            }
            break;
        }
        case BTL_OBJ_ENTITY: {
            ObjEntity* entity = AS_ENTITY(value);
            btl_printf(vm, "<entity %" PRIu64 ">", entity->id);
            break;
        }
        case BTL_OBJ_UPVALUE:
            btl_print(vm, "<upvalue>");
            break;
        case BTL_OBJ_COROUTINE:
            btl_print(vm, "<coroutine>");
            break;
        default:
            btl_print(vm, "<object>");
            break;
        }
    }
}

void btl_error_value(VM* vm, BtlValue value) {
    if (IS_BOOL(value)) {
        btl_error(vm, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        btl_error(vm, "null");
    } else if (IS_NUMBER(value)) {
        btl_errorf(vm, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case BTL_OBJ_STRING:
            btl_error(vm, AS_CSTRING(value));
            break;
        default:
            btl_error(vm, "<object>");
            break;
        }
    }
}

// ============================================================================
// Nursery Management
//
// The nursery is a semi-space for young object allocation. Uses bump-pointer
// allocation for fast allocation. When full, triggers minor GC which promotes
// live objects to old generation.
// ============================================================================

void btl_nursery_init(VM* vm, BtlNursery* nursery) {
    // Get nursery size from config, or use default
    size_t size = BTL_DEFAULT_NURSERY_SIZE;
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

void btl_nursery_free(VM* vm, BtlNursery* nursery) {
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
// Remembered Set
//
// Tracks old-generation objects that reference nursery objects. These serve
// as additional roots during minor GC.
// ============================================================================

void btl_remembered_set_init(BtlRememberedSet* set) {
    set->entries = NULL;
    set->count = 0;
    set->capacity = 0;
}

void btl_remembered_set_free(VM* vm, BtlRememberedSet* set) {
    if (set->entries != NULL) {
        BTL_FREE_ARRAY(vm, BtlRememberedEntry, set->entries, set->capacity);
    }
    set->entries = NULL;
    set->count = 0;
    set->capacity = 0;
}

void btl_remembered_set_add(VM* vm, BtlObj* object) {
    for (int i = 0; i < vm->rememberedSet.count; i++) {
        if (vm->rememberedSet.entries[i].object == object) {
            return;
        }
    }

    if (vm->rememberedSet.count >= vm->rememberedSet.capacity) {
        int oldCapacity = vm->rememberedSet.capacity;
        vm->rememberedSet.capacity = BTL_GROW_CAPACITY(oldCapacity);
        vm->rememberedSet.entries = BTL_GROW_ARRAY(vm, BtlRememberedEntry,
            vm->rememberedSet.entries, oldCapacity, vm->rememberedSet.capacity);
    }

    vm->rememberedSet.entries[vm->rememberedSet.count].object = object;
    vm->rememberedSet.entries[vm->rememberedSet.count].next = NULL;
    vm->rememberedSet.count++;
}

// ============================================================================
// GC Inhibit
//
// Temporarily prevent GC from running. Used during sensitive operations
// where GC would be problematic. Nest-able (uses counter).
// ============================================================================

void btl_gc_inhibit_start(VM* vm) {
    vm->gcInhibit++;
}

void btl_gc_inhibit_end(VM* vm) {
    vm->gcInhibit--;
}

// ============================================================================
// Write Barrier
//
// Called when storing a reference. If an old-gen object stores a reference
// to a nursery object, we must remember it as a root for minor GC.
// ============================================================================

void btl_gc_write_barrier(VM* vm, BtlObj* container, BtlValue value) {
    if (!IS_OBJ(value)) return;
    BtlObj* child = AS_OBJ(value);
    if (container->generation == BTL_GEN_OLD && child->generation == BTL_GEN_NURSERY) {
        btl_remembered_set_add(vm, container);
    }
}

// ============================================================================
// Object Allocation
//
// Allocates objects either in nursery or old generation based on size and
// type. Long-lived objects (modules, classes, functions) go directly to
// old gen. Large objects also go directly to old gen.
// ============================================================================

static BtlObj* allocateInOldGen(VM* vm, size_t size, BtlObjType type) {
    BtlObj* object = (BtlObj*) btl_realloc(vm, NULL, 0, size);
    object->type = type;
    object->isMarked = false;
    object->generation = BTL_GEN_OLD;
    object->next = vm->objects;
    object->forwarding = NULL;
    vm->objects = object;
    return object;
}

void* btl_object_allocate(VM* vm, size_t size, BtlObjType type) {
    if (vm->inMinorGC) {
        return allocateInOldGen(vm, size, type);
    }

    // Calculate large object threshold based on actual nursery size
    size_t largeObjSize = BTL_LARGE_OBJECT_SIZE_FOR(vm->nursery.size);
    if (size >= largeObjSize) {
        return allocateInOldGen(vm, size, type);
    }

    if (type == BTL_OBJ_STRING) {
        return allocateInOldGen(vm, size, type);
    }

    if (type == BTL_OBJ_MODULE || type == BTL_OBJ_CLASS || type == BTL_OBJ_FUNCTION ||
        type == BTL_OBJ_CLOSURE || type == BTL_OBJ_NATIVE || type == BTL_OBJ_NATIVE_CLASS ||
        type == BTL_OBJ_NATIVE_MODULE || type == BTL_OBJ_NATIVE_METHOD ||
        type == BTL_OBJ_ACTOR || type == BTL_OBJ_FUTURE ||
        type == BTL_OBJ_ENTITY) {
        return allocateInOldGen(vm, size, type);
    }

    BtlObj* object = (BtlObj*) nurseryAlloc(vm, size);

    if (object == NULL) {
        if (vm->gcInhibit == 0) {
            btl_gc_minor(vm);
            object = (BtlObj*) nurseryAlloc(vm, size);
        }

        if (object == NULL) {
            return allocateInOldGen(vm, size, type);
        }
    }

    object->type = type;
    object->isMarked = false;
    object->generation = BTL_GEN_NURSERY;
    object->next = NULL;
    object->forwarding = NULL;

    return object;
}

// ============================================================================
// GC Marking
//
// Tri-color marking: objects start white, are marked gray when first seen,
// and become black after all their references are scanned.
// ============================================================================

void btl_gc_mark_object(VM* vm, BtlObj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;

    object->isMarked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = BTL_GROW_CAPACITY(vm->grayCapacity);
        // Gray stack uses runtime allocator
        vm->grayStack = (BtlObj**) btl_realloc(vm, vm->grayStack,
            sizeof(BtlObj*) * (vm->grayCapacity / 2),
            sizeof(BtlObj*) * vm->grayCapacity);
        if (vm->grayStack == NULL) exit(1);
    }

    vm->grayStack[vm->grayCount++] = object;
}

void btl_gc_mark_value(VM* vm, BtlValue value) {
    if (IS_OBJ(value)) btl_gc_mark_object(vm, AS_OBJ(value));
}

static void markClass(VM* vm, ObjClass* klass) {
    btl_gc_mark_object(vm, (BtlObj*) klass->name);
    btl_table_mark(vm, &klass->methodIndices);
    btl_table_mark(vm, &klass->fieldIndices);
    btl_table_mark(vm, &klass->nativeMethods);

    for (int i = 0; i < klass->methodCount; i++) {
        if (klass->methods[i].closure != NULL) {
            btl_gc_mark_object(vm, (BtlObj*) klass->methods[i].closure);
            btl_gc_mark_object(vm, (BtlObj*) klass->methods[i].name);
        }
    }
}

static void markArray(VM* vm, BtlValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        btl_gc_mark_value(vm, array->values[i]);
    }
}

static void blackenObject(VM* vm, BtlObj* object) {
    switch (object->type) {
    case BTL_OBJ_BOUND_METHOD: {
        ObjBoundMethod* b = (ObjBoundMethod*) object;
        btl_gc_mark_value(vm, b->receiver);
        btl_gc_mark_object(vm, (BtlObj*) b->method);
        break;
    }
    case BTL_OBJ_CLASS:
        markClass(vm, (ObjClass*) object);
        break;
    case BTL_OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        btl_gc_mark_object(vm, (BtlObj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) {
            BtlRuntimeUpvalue* uv = &closure->upvalues[i];
            if (!uv->isOpen) {
                if (uv->isMutable) {
                    btl_gc_mark_object(vm, (BtlObj*) uv->loc.box);
                } else {
                    btl_gc_mark_value(vm, uv->loc.immValue);
                }
            }
        }
        break;
    }
    case BTL_OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        btl_gc_mark_object(vm, (BtlObj*) f->name);
        markArray(vm, &f->chunk.constants);
        break;
    }
    case BTL_OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        btl_gc_mark_object(vm, (BtlObj*) instance->klass);
        for (int i = 0; i < instance->klass->fieldCount; i++) {
            btl_gc_mark_value(vm, instance->fields[i]);
        }
        break;
    }
    case BTL_OBJ_LIST: {
        ObjList* list = (ObjList*) object;
        for (int i = 0; i < list->items.count; i++) {
            btl_gc_mark_value(vm, list->items.values[i]);
        }
        break;
    }
    case BTL_OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        btl_gc_mark_object(vm, (BtlObj*) m->name);
        btl_table_mark(vm, &m->globalNames);
        markArray(vm, &m->globalValues);
        // Mark classInfo keys and saved table contents
        btl_table_mark(vm, &m->classInfo);
        for (int i = 0; i < m->classInfo.capacity; i++) {
            BtlEntry* entry = &m->classInfo.entries[i];
            if (!IS_EMPTY(entry->key)) {
                BtlSavedClassInfo* savedInfo = (BtlSavedClassInfo*) (uintptr_t) AS_NUMBER(entry->value);
                btl_table_mark(vm, &savedInfo->methodIndices);
                btl_table_mark(vm, &savedInfo->fieldIndices);
            }
        }
        break;
    }
    case BTL_OBJ_UPVALUE:
        btl_gc_mark_value(vm, ((ObjUpvalue*) object)->closed);
        break;
    case BTL_OBJ_TABLE:
        btl_table_mark(vm, &((ObjTable*) object)->table);
        break;
    case BTL_OBJ_NATIVE_METHOD:
        btl_gc_mark_object(vm, (BtlObj*) ((ObjNativeMethod*) object)->name);
        break;
    case BTL_OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        btl_gc_mark_object(vm, (BtlObj*) klass->name);
        btl_table_mark(vm, &klass->methods);
        break;
    }
    case BTL_OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        btl_gc_mark_object(vm, (BtlObj*) module->name);
        btl_table_mark(vm, &module->globals);
        break;
    }
    case BTL_OBJ_FUTURE:
        btl_gc_mark_value(vm, ((ObjFuture*) object)->result);
        break;
    case BTL_OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        btl_gc_mark_value(vm, actor->instance);
        break;
    }
    case BTL_OBJ_ENTITY: {
        ObjEntity* entity = (ObjEntity*) object;
        btl_table_mark(vm, &entity->scriptComponents);
        break;
    }
    case BTL_OBJ_COROUTINE: {
        ObjCoroutine* co = (ObjCoroutine*) object;
        btl_gc_mark_object(vm, (BtlObj*) co->body);
        // Skip stack/frame marking for RUNNING coroutines: their stack is
        // swapped into the VM during execution, so the VM's own root marking
        // covers it. co->stack/stackTop may be stale if the stack was
        // reallocated during execution (dangling pointer).
        if (co->state == COROUTINE_RUNNING) break;
        // Mark everything on the coroutine's own stack
        for (BtlValue* slot = co->stack; slot < co->stackTop; slot++) {
            btl_gc_mark_value(vm, *slot);
        }
        // Mark closures in the coroutine's own frames
        for (int i = 0; i < co->frameCount; i++) {
            btl_gc_mark_object(vm, (BtlObj*) co->frames[i].closure);
            // Walk open upvalues linked list
            BtlRuntimeUpvalue* uv = co->frames[i].openUpvalues;
            while (uv) {
                if (!uv->isOpen && uv->isMutable && uv->loc.box) {
                    btl_gc_mark_object(vm, (BtlObj*) uv->loc.box);
                } else if (!uv->isOpen && !uv->isMutable) {
                    btl_gc_mark_value(vm, uv->loc.immValue);
                }
                uv = uv->next;
            }
        }
        break;
    }
    case BTL_OBJ_NATIVE:
    case BTL_OBJ_STRING:
        break;
    }
}

// ============================================================================
// Minor GC
//
// Collects the nursery by promoting all live objects to old generation.
// Scans stack, frames, remembered set, and other roots.
// ============================================================================

static BtlObj* promoteObject(VM* vm, BtlObj* object) {
    if (object == NULL) return NULL;
    if (object->generation != BTL_GEN_NURSERY) return object;
    if (object->forwarding != NULL) return object->forwarding;

    size_t size;
    switch (object->type) {
    case BTL_OBJ_BOUND_METHOD: size = sizeof(ObjBoundMethod); break;
    case BTL_OBJ_CLASS: size = sizeof(ObjClass); break;
    case BTL_OBJ_CLOSURE: {
        ObjClosure* c = (ObjClosure*) object;
        size = sizeof(ObjClosure) + sizeof(BtlRuntimeUpvalue) * c->upvalueCount;
        break;
    }
    case BTL_OBJ_FUNCTION: size = sizeof(ObjFunction); break;
    case BTL_OBJ_INSTANCE: size = sizeof(ObjInstance); break;
    case BTL_OBJ_LIST: size = sizeof(ObjList); break;
    case BTL_OBJ_MODULE: size = sizeof(ObjModule); break;
    case BTL_OBJ_NATIVE: size = sizeof(ObjNative); break;
    case BTL_OBJ_STRING: size = sizeof(ObjString); break;
    case BTL_OBJ_UPVALUE: size = sizeof(ObjUpvalue); break;
    case BTL_OBJ_TABLE: size = sizeof(ObjTable); break;
    case BTL_OBJ_NATIVE_METHOD: size = sizeof(ObjNativeMethod); break;
    case BTL_OBJ_NATIVE_CLASS: size = sizeof(ObjNativeClass); break;
    case BTL_OBJ_NATIVE_MODULE: size = sizeof(ObjNativeModule); break;
    case BTL_OBJ_FUTURE: size = sizeof(ObjFuture); break;
    case BTL_OBJ_ACTOR: size = sizeof(ObjActor); break;
    case BTL_OBJ_ENTITY: size = sizeof(ObjEntity); break;
    case BTL_OBJ_COROUTINE: size = sizeof(ObjCoroutine); break;
    default: size = sizeof(BtlObj); break;
    }

    // Use runtime allocator for promoted objects
    BtlObj* copy = (BtlObj*) btl_realloc(vm, NULL, 0, size);
    if (copy == NULL) exit(1);

    memcpy(copy, object, size);

    copy->generation = BTL_GEN_OLD;
    copy->next = vm->objects;
    copy->forwarding = NULL;
    copy->isMarked = false;
    vm->objects = copy;

    object->forwarding = copy;
    vm->promotedBytes += size;

    // Duplicate external allocations
    switch (object->type) {
    case BTL_OBJ_CLOSURE: {
        ObjClosure* oldClosure = (ObjClosure*) object;
        ObjClosure* newClosure = (ObjClosure*) copy;

        if (oldClosure->fieldICs != NULL && oldClosure->function->fieldICCount > 0) {
            size_t icSize = sizeof(BtlFieldIC) * oldClosure->function->fieldICCount;
            newClosure->fieldICs = btl_realloc(vm, NULL, 0, icSize);
            memcpy(newClosure->fieldICs, oldClosure->fieldICs, icSize);
        }

        if (oldClosure->methodICs != NULL && oldClosure->function->methodICCount > 0) {
            size_t icSize = sizeof(BtlMethodIC) * oldClosure->function->methodICCount;
            newClosure->methodICs = btl_realloc(vm, NULL, 0, icSize);
            memcpy(newClosure->methodICs, oldClosure->methodICs, icSize);
        }
        break;
    }
    case BTL_OBJ_FUNCTION: {
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
            size_t constSize = sizeof(BtlValue) * oldFunc->chunk.constants.capacity;
            newFunc->chunk.constants.values = btl_realloc(vm, NULL, 0, constSize);
            memcpy(newFunc->chunk.constants.values, oldFunc->chunk.constants.values, constSize);
        }
        break;
    }
    case BTL_OBJ_INSTANCE: {
        ObjInstance* oldInst = (ObjInstance*) object;
        ObjInstance* newInst = (ObjInstance*) copy;

        if (oldInst->fields != NULL && oldInst->klass->fieldCount > 0) {
            size_t fieldSize = sizeof(BtlValue) * oldInst->klass->fieldCount;
            newInst->fields = btl_realloc(vm, NULL, 0, fieldSize);
            memcpy(newInst->fields, oldInst->fields, fieldSize);
        }
        break;
    }
    case BTL_OBJ_LIST: {
        ObjList* oldList = (ObjList*) object;
        ObjList* newList = (ObjList*) copy;

        if (oldList->items.values != NULL && oldList->items.capacity > 0) {
            size_t itemSize = sizeof(BtlValue) * oldList->items.capacity;
            newList->items.values = btl_realloc(vm, NULL, 0, itemSize);
            memcpy(newList->items.values, oldList->items.values, itemSize);
        }
        break;
    }
    case BTL_OBJ_CLASS: {
        ObjClass* oldClass = (ObjClass*) object;
        ObjClass* newClass = (ObjClass*) copy;

        if (oldClass->methods != NULL && oldClass->methodCapacity > 0) {
            size_t methodSize = sizeof(BtlMethodEntry) * oldClass->methodCapacity;
            newClass->methods = btl_realloc(vm, NULL, 0, methodSize);
            memcpy(newClass->methods, oldClass->methods, methodSize);
        }

        if (oldClass->methodIndices.entries != NULL && oldClass->methodIndices.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldClass->methodIndices.capacity;
            newClass->methodIndices.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newClass->methodIndices.entries, oldClass->methodIndices.entries, entrySize);
        }

        if (oldClass->fieldIndices.entries != NULL && oldClass->fieldIndices.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldClass->fieldIndices.capacity;
            newClass->fieldIndices.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newClass->fieldIndices.entries, oldClass->fieldIndices.entries, entrySize);
        }

        if (oldClass->nativeGetters != NULL && oldClass->fieldCount > 0) {
            size_t getterSize = sizeof(BtlFieldGetterFn) * oldClass->fieldCount;
            newClass->nativeGetters = btl_realloc(vm, NULL, 0, getterSize);
            memcpy(newClass->nativeGetters, oldClass->nativeGetters, getterSize);
        }

        if (oldClass->nativeSetters != NULL && oldClass->fieldCount > 0) {
            size_t setterSize = sizeof(BtlFieldSetterFn) * oldClass->fieldCount;
            newClass->nativeSetters = btl_realloc(vm, NULL, 0, setterSize);
            memcpy(newClass->nativeSetters, oldClass->nativeSetters, setterSize);
        }

        if (oldClass->nativeMethods.entries != NULL && oldClass->nativeMethods.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldClass->nativeMethods.capacity;
            newClass->nativeMethods.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newClass->nativeMethods.entries, oldClass->nativeMethods.entries, entrySize);
        }
        break;
    }
    case BTL_OBJ_MODULE: {
        ObjModule* oldMod = (ObjModule*) object;
        ObjModule* newMod = (ObjModule*) copy;

        if (oldMod->globalNames.entries != NULL && oldMod->globalNames.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldMod->globalNames.capacity;
            newMod->globalNames.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newMod->globalNames.entries, oldMod->globalNames.entries, entrySize);
        }

        if (oldMod->globalValues.values != NULL && oldMod->globalValues.capacity > 0) {
            size_t valSize = sizeof(BtlValue) * oldMod->globalValues.capacity;
            newMod->globalValues.values = btl_realloc(vm, NULL, 0, valSize);
            memcpy(newMod->globalValues.values, oldMod->globalValues.values, valSize);
        }

        if (oldMod->classInfo.entries != NULL && oldMod->classInfo.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldMod->classInfo.capacity;
            newMod->classInfo.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newMod->classInfo.entries, oldMod->classInfo.entries, entrySize);
        }
        break;
    }
    case BTL_OBJ_TABLE: {
        ObjTable* oldTable = (ObjTable*) object;
        ObjTable* newTable = (ObjTable*) copy;

        if (oldTable->table.entries != NULL && oldTable->table.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldTable->table.capacity;
            newTable->table.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newTable->table.entries, oldTable->table.entries, entrySize);
        }
        break;
    }
    case BTL_OBJ_NATIVE_CLASS: {
        ObjNativeClass* oldNC = (ObjNativeClass*) object;
        ObjNativeClass* newNC = (ObjNativeClass*) copy;

        if (oldNC->methods.entries != NULL && oldNC->methods.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldNC->methods.capacity;
            newNC->methods.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newNC->methods.entries, oldNC->methods.entries, entrySize);
        }
        break;
    }
    case BTL_OBJ_NATIVE_MODULE: {
        ObjNativeModule* oldNM = (ObjNativeModule*) object;
        ObjNativeModule* newNM = (ObjNativeModule*) copy;

        if (oldNM->globals.entries != NULL && oldNM->globals.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldNM->globals.capacity;
            newNM->globals.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newNM->globals.entries, oldNM->globals.entries, entrySize);
        }
        break;
    }
    case BTL_OBJ_ENTITY: {
        ObjEntity* oldEntity = (ObjEntity*) object;
        ObjEntity* newEntity = (ObjEntity*) copy;

        if (oldEntity->scriptComponents.entries != NULL && oldEntity->scriptComponents.capacity > 0) {
            size_t entrySize = sizeof(BtlEntry) * oldEntity->scriptComponents.capacity;
            newEntity->scriptComponents.entries = btl_realloc(vm, NULL, 0, entrySize);
            memcpy(newEntity->scriptComponents.entries, oldEntity->scriptComponents.entries, entrySize);
        }
        break;
    }
    default:
        break;
    }

    return copy;
}

static BtlValue promoteValue(VM* vm, BtlValue value) {
    if (!IS_OBJ(value)) return value;
    BtlObj* obj = AS_OBJ(value);
    BtlObj* promoted = promoteObject(vm, obj);
    return OBJ_VAL(promoted);
}

static void scanObject(VM* vm, BtlObj* object) {
    if (object == NULL) return;

    switch (object->type) {
    case BTL_OBJ_BOUND_METHOD: {
        ObjBoundMethod* b = (ObjBoundMethod*) object;
        b->receiver = promoteValue(vm, b->receiver);
        b->method = (ObjClosure*) promoteObject(vm, (BtlObj*) b->method);
        break;
    }
    case BTL_OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        klass->name = (ObjString*) promoteObject(vm, (BtlObj*) klass->name);
        for (int i = 0; i < klass->methodCount; i++) {
            if (klass->methods[i].closure != NULL) {
                klass->methods[i].closure = (ObjClosure*) promoteObject(vm, (BtlObj*) klass->methods[i].closure);
                klass->methods[i].name = (ObjString*) promoteObject(vm, (BtlObj*) klass->methods[i].name);
            }
        }
        if (klass->methodIndices.entries != NULL) {
            for (int i = 0; i < klass->methodIndices.capacity; i++) {
                BtlEntry* entry = &klass->methodIndices.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        if (klass->fieldIndices.entries != NULL) {
            for (int i = 0; i < klass->fieldIndices.capacity; i++) {
                BtlEntry* entry = &klass->fieldIndices.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        if (klass->nativeMethods.entries != NULL) {
            for (int i = 0; i < klass->nativeMethods.capacity; i++) {
                BtlEntry* entry = &klass->nativeMethods.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case BTL_OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        closure->function = (ObjFunction*) promoteObject(vm, (BtlObj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) {
            BtlRuntimeUpvalue* uv = &closure->upvalues[i];
            if (!uv->isOpen) {
                if (uv->isMutable && uv->loc.box != NULL) {
                    uv->loc.box = (ObjUpvalue*) promoteObject(vm, (BtlObj*) uv->loc.box);
                } else if (!uv->isMutable) {
                    uv->loc.immValue = promoteValue(vm, uv->loc.immValue);
                }
            }
        }
        break;
    }
    case BTL_OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        f->name = (ObjString*) promoteObject(vm, (BtlObj*) f->name);
        f->module = (ObjModule*) promoteObject(vm, (BtlObj*) f->module);
        if (f->chunk.constants.values != NULL) {
            for (int i = 0; i < f->chunk.constants.count; i++) {
                f->chunk.constants.values[i] = promoteValue(vm, f->chunk.constants.values[i]);
            }
        }
        break;
    }
    case BTL_OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        instance->klass = (ObjClass*) promoteObject(vm, (BtlObj*) instance->klass);
        if (instance->fields != NULL && instance->klass != NULL) {
            for (int i = 0; i < instance->klass->fieldCount; i++) {
                instance->fields[i] = promoteValue(vm, instance->fields[i]);
            }
        }
        break;
    }
    case BTL_OBJ_LIST: {
        ObjList* list = (ObjList*) object;
        if (list->items.values != NULL) {
            for (int i = 0; i < list->items.count; i++) {
                list->items.values[i] = promoteValue(vm, list->items.values[i]);
            }
        }
        break;
    }
    case BTL_OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        m->name = (ObjString*) promoteObject(vm, (BtlObj*) m->name);
        if (m->globalValues.values != NULL) {
            for (int i = 0; i < m->globalValues.count; i++) {
                m->globalValues.values[i] = promoteValue(vm, m->globalValues.values[i]);
            }
        }
        if (m->globalNames.entries != NULL) {
            for (int i = 0; i < m->globalNames.capacity; i++) {
                BtlEntry* entry = &m->globalNames.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                }
            }
        }
        break;
    }
    case BTL_OBJ_UPVALUE: {
        ObjUpvalue* uv = (ObjUpvalue*) object;
        uv->closed = promoteValue(vm, uv->closed);
        break;
    }
    case BTL_OBJ_TABLE: {
        ObjTable* table = (ObjTable*) object;
        if (table->table.entries != NULL) {
            for (int i = 0; i < table->table.capacity; i++) {
                BtlEntry* entry = &table->table.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case BTL_OBJ_FUTURE: {
        ObjFuture* future = (ObjFuture*) object;
        future->result = promoteValue(vm, future->result);
        break;
    }
    case BTL_OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        actor->instance = promoteValue(vm, actor->instance);
        actor->klass = (ObjClass*) promoteObject(vm, (BtlObj*) actor->klass);
        break;
    }
    case BTL_OBJ_ENTITY: {
        ObjEntity* entity = (ObjEntity*) object;
        if (entity->scriptComponents.entries != NULL) {
            for (int i = 0; i < entity->scriptComponents.capacity; i++) {
                BtlEntry* entry = &entity->scriptComponents.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case BTL_OBJ_NATIVE_METHOD: {
        ObjNativeMethod* method = (ObjNativeMethod*) object;
        method->name = (ObjString*) promoteObject(vm, (BtlObj*) method->name);
        break;
    }
    case BTL_OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        klass->name = (ObjString*) promoteObject(vm, (BtlObj*) klass->name);
        if (klass->methods.entries != NULL) {
            for (int i = 0; i < klass->methods.capacity; i++) {
                BtlEntry* entry = &klass->methods.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case BTL_OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        module->name = (ObjString*) promoteObject(vm, (BtlObj*) module->name);
        if (module->globals.entries != NULL) {
            for (int i = 0; i < module->globals.capacity; i++) {
                BtlEntry* entry = &module->globals.entries[i];
                if (!IS_EMPTY(entry->key)) {
                    entry->key = promoteValue(vm, entry->key);
                    entry->value = promoteValue(vm, entry->value);
                }
            }
        }
        break;
    }
    case BTL_OBJ_COROUTINE: {
        ObjCoroutine* co = (ObjCoroutine*) object;
        co->body = (ObjClosure*) promoteObject(vm, (BtlObj*) co->body);
        // Skip stack/frame promotion for RUNNING coroutines: their stack is
        // swapped into the VM during execution, so vm->stack has the live data.
        // co->stack/stackTop may be stale if reallocated during execution.
        // The scheduler saves vm->stack back into co->stack after execution.
        if (co->state == COROUTINE_RUNNING) break;
        for (BtlValue* slot = co->stack; slot < co->stackTop; slot++) {
            *slot = promoteValue(vm, *slot);
        }
        for (int i = 0; i < co->frameCount; i++) {
            co->frames[i].closure = (ObjClosure*) promoteObject(vm, (BtlObj*) co->frames[i].closure);
        }
        break;
    }
    case BTL_OBJ_NATIVE:
    case BTL_OBJ_STRING:
        break;
    }
}

// Free external (heap-allocated) data owned by nursery objects.
// Called before resetting the nursery bump pointer so that external arrays
// (list items, table entries, instance fields) are not leaked.
// The object structs themselves live inside the nursery slab and are
// reclaimed when the bump pointer resets, so only their external data needs
// explicit freeing.
static void freeNurseryExternals(VM* vm) {
    uint8_t* ptr = vm->nursery.fromSpace;
    uint8_t* end = vm->nursery.allocPtr;

    while (ptr < end) {
        BtlObj* obj = (BtlObj*) ptr;
        size_t size;
        bool promoted = (obj->forwarding != NULL);

        switch (obj->type) {
        case BTL_OBJ_LIST: {
            ObjList* list = (ObjList*) obj;
            // promoteObject always duplicates items when capacity > 0,
            // so free the old data for both promoted and dead objects.
            if (list->items.values != NULL && list->items.capacity > 0) {
                btl_realloc(vm, list->items.values,
                            sizeof(BtlValue) * list->items.capacity, 0);
            }
            size = sizeof(ObjList);
            break;
        }
        case BTL_OBJ_TABLE: {
            ObjTable* table = (ObjTable*) obj;
            if (table->table.entries != NULL && table->table.capacity > 0) {
                btl_realloc(vm, table->table.entries,
                            sizeof(BtlEntry) * table->table.capacity, 0);
            }
            size = sizeof(ObjTable);
            break;
        }
        case BTL_OBJ_INSTANCE: {
            ObjInstance* inst = (ObjInstance*) obj;
            if (inst->fields != NULL) {
                if (promoted) {
                    // promoteObject only duplicates fields when fieldCount > 0.
                    // When fieldCount == 0, the promoted copy shares this pointer
                    // and will free it during freeObject. Don't double-free.
                    if (inst->klass != NULL && inst->klass->fieldCount > 0) {
                        btl_realloc(vm, inst->fields,
                                    sizeof(BtlValue) * inst->klass->fieldCount, 0);
                    }
                } else {
                    // Dead object. We own the only reference. Use the actual
                    // allocated size: max(fieldCount, 1) per btl_instance_new.
                    int count = (inst->klass != NULL && inst->klass->fieldCount > 0)
                                ? inst->klass->fieldCount : 1;
                    btl_realloc(vm, inst->fields,
                                sizeof(BtlValue) * count, 0);
                }
            }
            size = sizeof(ObjInstance);
            break;
        }
        case BTL_OBJ_ENTITY: {
            ObjEntity* entity = (ObjEntity*) obj;
            if (entity->scriptComponents.entries != NULL && entity->scriptComponents.capacity > 0) {
                btl_realloc(vm, entity->scriptComponents.entries,
                            sizeof(BtlEntry) * entity->scriptComponents.capacity, 0);
            }
            size = sizeof(ObjEntity);
            break;
        }
        case BTL_OBJ_COROUTINE: {
            ObjCoroutine* co = (ObjCoroutine*) obj;
            if (co->stack != NULL) {
                free(co->stack);
            }
            if (co->frames != NULL) {
                free(co->frames);
            }
            size = sizeof(ObjCoroutine);
            break;
        }
        case BTL_OBJ_BOUND_METHOD:
            size = sizeof(ObjBoundMethod);
            break;
        case BTL_OBJ_UPVALUE:
            size = sizeof(ObjUpvalue);
            break;
        default:
            // Should not happen. Only the above types go into the nursery.
            // But if it does, we can't determine the size, so bail out.
            return;
        }

        // Advance by aligned size (same alignment as nurseryAlloc)
        size = (size + 7) & ~7;
        ptr += size;
    }
}

void btl_gc_minor(VM* vm) {
    if (vm->inMinorGC) return;
    if (vm->nurseryAllocated == 0 || vm->nursery.fromSpace == NULL) return;
    if (vm->gcInhibit > 0) return;
    vm->gcInhibit++;
    vm->inMinorGC = true;

#ifdef BTL_DEBUG_LOG_GC
    btl_errorf(vm, "-- minor gc begin (nursery: %zu bytes)\n", vm->nurseryAllocated);
    size_t promotedBefore = vm->promotedBytes;
#endif

    BtlObj* oldGenHead = vm->objects;

    for (BtlValue* slot = vm->stack; slot < vm->stackTop; slot++) {
        *slot = promoteValue(vm, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++) {
        vm->frames[i].closure = (ObjClosure*) promoteObject(vm, (BtlObj*) vm->frames[i].closure);
    }

    if (vm->rootModule) {
        vm->rootModule = (ObjModule*) promoteObject(vm, (BtlObj*) vm->rootModule);
    }

    if (vm->stringClass) vm->stringClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->stringClass);
    if (vm->numberClass) vm->numberClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->numberClass);
    if (vm->intClass) vm->intClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->intClass);
    if (vm->listClass) vm->listClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->listClass);
    if (vm->tableClass) vm->tableClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->tableClass);
    if (vm->entityClass) vm->entityClass = (ObjNativeClass*) promoteObject(vm, (BtlObj*) vm->entityClass);

    if (vm->modules.entries != NULL) {
        for (int i = 0; i < vm->modules.capacity; i++) {
            BtlEntry* entry = &vm->modules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    if (vm->nativeModules.entries != NULL) {
        for (int i = 0; i < vm->nativeModules.capacity; i++) {
            BtlEntry* entry = &vm->nativeModules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    // Promote nativeClassInfo string keys (field names stored inside are
    // NUMBER_VAL pointers to malloc'd structs, so only keys need promotion)
    if (vm->nativeClassInfo.entries != NULL) {
        for (int i = 0; i < vm->nativeClassInfo.capacity; i++) {
            BtlEntry* entry = &vm->nativeClassInfo.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                // Also promote strings inside the fieldIndices/methodIndices tables
                BtlSavedClassInfo* info = (BtlSavedClassInfo*)(uintptr_t)AS_NUMBER(entry->value);
                for (int j = 0; j < info->fieldIndices.capacity; j++) {
                    BtlEntry* fe = &info->fieldIndices.entries[j];
                    if (!IS_EMPTY(fe->key)) fe->key = promoteValue(vm, fe->key);
                }
                for (int j = 0; j < info->methodIndices.capacity; j++) {
                    BtlEntry* me = &info->methodIndices.entries[j];
                    if (!IS_EMPTY(me->key)) me->key = promoteValue(vm, me->key);
                }
            }
        }
    }

    if (vm->initString) {
        vm->initString = (ObjString*) promoteObject(vm, (BtlObj*) vm->initString);
    }

    vm->lastReturnValue = promoteValue(vm, vm->lastReturnValue);

    for (int i = 0; i < vm->rememberedSet.count; i++) {
        scanObject(vm, vm->rememberedSet.entries[i].object);
    }

    bool madeProgress = true;
    while (madeProgress) {
        madeProgress = false;
        BtlObj* obj = vm->objects;
        while (obj != NULL && obj != oldGenHead) {
            if (!obj->isMarked) {
                obj->isMarked = true;
                scanObject(vm, obj);
                madeProgress = true;
            }
            obj = obj->next;
        }
    }

    for (BtlObj* obj = vm->objects; obj != NULL && obj != oldGenHead; obj = obj->next) {
        obj->isMarked = false;
    }

    // Free external allocations owned by nursery objects before resetting.
    // Promoted objects had their data duplicated into new heap allocations;
    // dead objects' data also needs freeing. The object structs themselves
    // live in the nursery slab and are reclaimed by the bump-pointer reset.
    freeNurseryExternals(vm);

    vm->nursery.allocPtr = vm->nursery.fromSpace;
    vm->nurseryAllocated = 0;
    vm->rememberedSet.count = 0;

    vm->minorGCCount++;
    vm->inMinorGC = false;
    vm->gcInhibit--;

#ifdef BTL_DEBUG_LOG_GC
    btl_errorf(vm, "-- minor gc end (promoted %zu bytes)\n", vm->promotedBytes - promotedBefore);
#endif

    //if (vm->bytesAllocated > vm->nextGC) {
    //    btl_gc_major(vm);
    //}
}

// ============================================================================
// Major GC
//
// Full mark-sweep collection of the old generation. Marks all reachable
// objects starting from roots, then sweeps unmarked objects.
// ============================================================================

static void freeObject(VM* vm, BtlObj* object) {
    switch (object->type) {
    case BTL_OBJ_BOUND_METHOD:
        BTL_FREE(vm, ObjBoundMethod, object);
        break;
    case BTL_OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        if (klass->methods != NULL) {
            BTL_FREE_ARRAY(vm, BtlMethodEntry, klass->methods, klass->methodCapacity);
        }
        btl_table_free(vm, &klass->methodIndices);
        btl_table_free(vm, &klass->fieldIndices);
        btl_table_free(vm, &klass->nativeMethods);
        if (klass->nativeGetters != NULL) {
            BTL_FREE_ARRAY(vm, BtlFieldGetterFn, klass->nativeGetters, klass->fieldCount);
        }
        if (klass->nativeSetters != NULL) {
            BTL_FREE_ARRAY(vm, BtlFieldSetterFn, klass->nativeSetters, klass->fieldCount);
        }
        BTL_FREE(vm, ObjClass, object);
        break;
    }
    case BTL_OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        if (closure->fieldICs != NULL) {
            BTL_FREE_ARRAY(vm, BtlFieldIC, closure->fieldICs, closure->function->fieldICCount);
        }
        if (closure->methodICs != NULL) {
            BTL_FREE_ARRAY(vm, BtlMethodIC, closure->methodICs, closure->function->methodICCount);
        }
        size_t size = sizeof(ObjClosure) + sizeof(BtlRuntimeUpvalue) * closure->upvalueCount;
        btl_realloc(vm, object, size, 0);
        break;
    }
    case BTL_OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        btl_chunk_free(vm, &f->chunk);
        BTL_FREE(vm, ObjFunction, object);
        break;
    }
    case BTL_OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        if (instance->nativeData && instance->klass->nativeDataFree) {
            instance->klass->nativeDataFree(instance->nativeData);
        }
        if (instance->fields != NULL) {
            BTL_FREE_ARRAY(vm, BtlValue, instance->fields, instance->klass->fieldCount);
        }
        BTL_FREE(vm, ObjInstance, object);
        break;
    }
    case BTL_OBJ_LIST: {
        ObjList* l = (ObjList*) object;
        btl_value_array_free(vm, &l->items);
        BTL_FREE(vm, ObjList, object);
        break;
    }
    case BTL_OBJ_MODULE: {
        ObjModule* m = (ObjModule*) object;
        for (int i = 0; i < m->classInfo.capacity; i++) {
            BtlEntry* entry = &m->classInfo.entries[i];
            if (!IS_EMPTY(entry->key)) {
                BtlSavedClassInfo* savedInfo = (BtlSavedClassInfo*) (uintptr_t) AS_NUMBER(entry->value);
                btl_table_free(vm, &savedInfo->methodIndices);
                btl_table_free(vm, &savedInfo->fieldIndices);
                BTL_FREE(vm, BtlSavedClassInfo, savedInfo);
            }
        }
        btl_table_free(vm, &m->classInfo);
        btl_table_free(vm, &m->globalNames);
        btl_value_array_free(vm, &m->globalValues);
        BTL_FREE(vm, ObjModule, object);
        break;
    }
    case BTL_OBJ_NATIVE:
        BTL_FREE(vm, ObjNative, object);
        break;
    case BTL_OBJ_STRING: {
        ObjString* s = (ObjString*) object;
        BTL_FREE_ARRAY(vm, char, s->chars, s->length + 1);
        BTL_FREE(vm, ObjString, object);
        break;
    }
    case BTL_OBJ_UPVALUE:
        BTL_FREE(vm, ObjUpvalue, object);
        break;
    case BTL_OBJ_TABLE: {
        ObjTable* table = (ObjTable*) object;
        btl_table_free(vm, &table->table);
        BTL_FREE(vm, ObjTable, object);
        break;
    }
    case BTL_OBJ_NATIVE_METHOD:
        BTL_FREE(vm, ObjNativeMethod, object);
        break;
    case BTL_OBJ_NATIVE_CLASS: {
        ObjNativeClass* klass = (ObjNativeClass*) object;
        btl_table_free(vm, &klass->methods);
        BTL_FREE(vm, ObjNativeClass, object);
        break;
    }
    case BTL_OBJ_NATIVE_MODULE: {
        ObjNativeModule* module = (ObjNativeModule*) object;
        btl_table_free(vm, &module->globals);
        BTL_FREE(vm, ObjNativeModule, object);
        break;
    }
    case BTL_OBJ_FUTURE: {
        ObjFuture* future = (ObjFuture*) object;
        btl_future_free(future);
        BTL_FREE(vm, ObjFuture, object);
        break;
    }
    case BTL_OBJ_ACTOR: {
        ObjActor* actor = (ObjActor*) object;
        btl_actor_stop(actor);
        BTL_FREE(vm, ObjActor, object);
        break;
    }
    case BTL_OBJ_ENTITY: {
        ObjEntity* entity = (ObjEntity*) object;
        btl_table_free(vm, &entity->scriptComponents);
        BTL_FREE(vm, ObjEntity, object);
        break;
    }
    case BTL_OBJ_COROUTINE: {
        ObjCoroutine* co = (ObjCoroutine*) object;
        if (co->stack != NULL) free(co->stack);
        if (co->frames != NULL) free(co->frames);
        BTL_FREE(vm, ObjCoroutine, object);
        break;
    }
    }
}

static void markRoots(VM* vm) {
    for (BtlValue* slot = vm->stack; slot < vm->stackTop; slot++) {
        btl_gc_mark_value(vm, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++) {
        btl_gc_mark_object(vm, (BtlObj*) vm->frames[i].closure);
    }

    btl_table_mark(vm, &vm->modules);

    if (vm->rootModule) btl_gc_mark_object(vm, (BtlObj*) vm->rootModule);

    if (vm->stringClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->stringClass);
    if (vm->numberClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->numberClass);
    if (vm->intClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->intClass);
    if (vm->listClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->listClass);
    if (vm->tableClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->tableClass);
    if (vm->entityClass != NULL) btl_gc_mark_object(vm, (BtlObj*) vm->entityClass);
    btl_table_mark(vm, &vm->nativeModules);

    // Mark native class info keys and field name strings (not GC-managed structs,
    // but the ObjString keys could be collected if not marked)
    for (int i = 0; i < vm->nativeClassInfo.capacity; i++) {
        BtlEntry* entry = &vm->nativeClassInfo.entries[i];
        if (!IS_EMPTY(entry->key)) {
            btl_gc_mark_value(vm, entry->key);
            BtlSavedClassInfo* info = (BtlSavedClassInfo*)(uintptr_t)AS_NUMBER(entry->value);
            btl_table_mark(vm, &info->fieldIndices);
            btl_table_mark(vm, &info->methodIndices);
        }
    }

    btl_compiler_mark_roots(vm);
    btl_gc_mark_object(vm, (BtlObj*) vm->initString);
    btl_gc_mark_value(vm, vm->lastReturnValue);

    // Mark native GC roots (registered by engine via btl_gc_add_root)
    for (int i = 0; i < vm->nativeRootCount; i++) {
        btl_gc_mark_value(vm, vm->nativeRoots[i]);
    }

    // Custom engine mark callback (e.g., UI element callbacks stored in C structs)
    if (vm->nativeMarkFn) {
        vm->nativeMarkFn(vm, vm->nativeMarkUserData);
    }
}

void btl_gc_major(VM* vm) {
    if (vm->gcInhibit > 0) return;

    vm->gcInhibit++;

#ifdef BTL_DEBUG_LOG_GC
    btl_error(vm, "-- major gc begin\n");
    size_t before = vm->bytesAllocated;
#endif

    markRoots(vm);

    while (vm->grayCount > 0) {
        BtlObj* object = vm->grayStack[--vm->grayCount];
        blackenObject(vm, object);
    }

    btl_table_remove_white(&vm->strings);

    BtlObj* previous = NULL;
    BtlObj* object = vm->objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            BtlObj* unreached = object;
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
    float growFactor = BTL_DEFAULT_GC_HEAP_GROW_FACTOR;
    if (vm->runtime != NULL && vm->runtime->config.gc_grow_factor > 0) {
        growFactor = vm->runtime->config.gc_grow_factor;
    }
    vm->nextGC = (size_t) (vm->bytesAllocated * growFactor);

    vm->majorGCCount++;

#ifdef BTL_DEBUG_LOG_GC
    btl_errorf(vm, "-- major gc end (collected %zu bytes, %zu -> %zu)\n",
        before - vm->bytesAllocated, before, vm->bytesAllocated);
#endif

    vm->gcInhibit--;
}

void btl_gc_collect(VM* vm) {
    size_t threshold = BTL_NURSERY_THRESHOLD_FOR(vm->nursery.size);
    if (vm->nurseryAllocated > threshold) {
        btl_gc_minor(vm);
    } else {
        btl_gc_major(vm);
    }
}

// ============================================================================
// Cleanup
//
// Free all objects and GC infrastructure during VM shutdown.
// ============================================================================

void btl_gc_free_all(VM* vm) {
    // Free external allocations of any objects still in the nursery BEFORE
    // freeing old-gen objects, because nursery instances may reference
    // old-gen classes (inst->klass) to determine field count.
    if (vm->nursery.fromSpace != NULL && vm->nursery.allocPtr > vm->nursery.fromSpace) {
        freeNurseryExternals(vm);
    }

    BtlObj* object = vm->objects;
    while (object != NULL) {
        BtlObj* next = object->next;
        freeObject(vm, object);
        object = next;
    }

    if (vm->grayStack != NULL) {
        btl_realloc(vm, vm->grayStack, sizeof(BtlObj*) * vm->grayCapacity, 0);
        vm->grayStack = NULL;
    }

    btl_nursery_free(vm, &vm->nursery);
    btl_remembered_set_free(vm, &vm->rememberedSet);
}