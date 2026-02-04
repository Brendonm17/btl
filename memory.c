#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

// ============================================
// NURSERY MANAGEMENT
// ============================================

void initNursery(Nursery* nursery) {
    nursery->size = NURSERY_SIZE;
    nursery->fromSpace = (uint8_t*) malloc(NURSERY_SIZE);
    nursery->toSpace = (uint8_t*) malloc(NURSERY_SIZE);
    nursery->allocPtr = nursery->fromSpace;
    nursery->limit = nursery->fromSpace + NURSERY_SIZE;

    if (nursery->fromSpace == NULL || nursery->toSpace == NULL) {
        fprintf(stderr, "Failed to allocate nursery\n");
        exit(1);
    }
}

void freeNursery(Nursery* nursery) {
    free(nursery->fromSpace);
    free(nursery->toSpace);
    nursery->fromSpace = NULL;
    nursery->toSpace = NULL;
    nursery->allocPtr = NULL;
    nursery->limit = NULL;
}

// Fast bump allocation in nursery
static void* nurseryAlloc(VM* vm, size_t size) {
    // Align to 8 bytes
    size = (size + 7) & ~7;

    if (vm->nursery.allocPtr + size > vm->nursery.limit) {
        return NULL;  // Nursery full
    }

    void* ptr = vm->nursery.allocPtr;
    vm->nursery.allocPtr += size;
    vm->nurseryAllocated += size;
    return ptr;
}

// ============================================
// REMEMBERED SET
// ============================================

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
    // Check if already in remembered set
    for (int i = 0; i < vm->rememberedSet.count; i++) {
        if (vm->rememberedSet.entries[i].object == object) {
            return;
        }
    }

    // Grow if needed
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

// ============================================
// GC INHIBIT HELPERS
// ============================================

void gcInhibitStart(VM* vm) {
    vm->gcInhibit++;
}

void gcInhibitEnd(VM* vm) {
    vm->gcInhibit--;
}

// ============================================
// WRITE BARRIER
// ============================================

void writeBarrier(VM* vm, Obj* container, Value value) {
    if (!IS_OBJ(value)) return;

    Obj* child = AS_OBJ(value);

    // If old object points to nursery object, remember it
    if (container->generation == GEN_OLD && child->generation == GEN_NURSERY) {
        rememberObject(vm, container);
    }
}

// ============================================
// OBJECT ALLOCATION
// ============================================

// Allocate directly in old generation (bypass nursery)
static Obj* allocateInOldGen(VM* vm, size_t size, ObjType type) {
    Obj* object = (Obj*) reallocate(vm, NULL, 0, size);
    object->type = type;
    object->isMarked = false;
    object->generation = GEN_OLD;
    object->next = vm->objects;
    object->forwarding = NULL;
    vm->objects = object;
    return object;
}

void* allocateObject(VM* vm, size_t size, ObjType type) {
    // During GC, allocate directly to old gen to avoid complications
    if (vm->inMinorGC) {
        return allocateInOldGen(vm, size, type);
    }

    // Large objects go directly to old generation
    if (size >= LARGE_OBJECT_SIZE) {
        return allocateInOldGen(vm, size, type);
    }

    // Strings go directly to old generation because they're interned
    // and moving them would corrupt the intern table's hash positions
    if (type == OBJ_STRING) {
        return allocateInOldGen(vm, size, type);
    }

    // These types are often shared across actor VMs or are long-lived,
    // so allocate them directly to old gen to avoid promotion issues
    if (type == OBJ_MODULE || type == OBJ_CLASS || type == OBJ_FUNCTION ||
        type == OBJ_CLOSURE || type == OBJ_NATIVE || type == OBJ_NATIVE_CLASS ||
        type == OBJ_NATIVE_MODULE || type == OBJ_NATIVE_METHOD ||
        type == OBJ_ACTOR || type == OBJ_FUTURE) {
        return allocateInOldGen(vm, size, type);
    }

    // Try nursery first
    Obj* object = (Obj*) nurseryAlloc(vm, size);

    if (object == NULL) {
        // Nursery full - run minor GC only if not inhibited
        if (vm->gcInhibit == 0) {
            minorGC(vm);
            // Try again
            object = (Obj*) nurseryAlloc(vm, size);
        }

        if (object == NULL) {
            // Still no room (or GC was inhibited) - fall back to old generation
            return allocateInOldGen(vm, size, type);
        }
    }

    // Initialize object header
    object->type = type;
    object->isMarked = false;
    object->generation = GEN_NURSERY;
    object->next = NULL;
    object->forwarding = NULL;

    return object;
}

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize) {
    vm->bytesAllocated += newSize - oldSize;

#ifdef DEBUG_STRESS_GC
    // Stress test: GC periodically, but only if not inhibited
    // Using a counter to avoid GC on literally every allocation which is too aggressive
    static int stressCounter = 0;
    if (newSize > oldSize && vm->gcInhibit == 0 && !vm->inMinorGC) {
        stressCounter++;
        if (stressCounter >= 10) {  // GC every 10 allocations
            stressCounter = 0;
            if (vm->nurseryAllocated > 0 && vm->nursery.fromSpace != NULL) {
                minorGC(vm);
            }
        }
    }
#endif

    // Trigger major GC based on old gen growth
    if (newSize > oldSize && !vm->inMinorGC && vm->gcInhibit == 0) {
        if (vm->bytesAllocated > vm->nextGC) {
            majorGC(vm);
        }
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

// ============================================
// MARKING FUNCTIONS
// ============================================

void markObject(VM* vm, Obj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;

    object->isMarked = true;

    // Add to gray stack
    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack = (Obj**) realloc(vm->grayStack, sizeof(Obj*) * vm->grayCapacity);
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
    case OBJ_CLASS: {
        markClass(vm, (ObjClass*) object);
        break;
    }
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
    case OBJ_NATIVE_METHOD: {
        ObjNativeMethod* method = (ObjNativeMethod*) object;
        markObject(vm, (Obj*) method->name);
        break;
    }
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
    case OBJ_FUTURE: {
        ObjFuture* future = (ObjFuture*) object;
        markValue(vm, future->result);
        break;
    }
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

// ============================================
// MINOR GC - Simplified approach
// ============================================

// Promote a single nursery object to old generation
static Obj* promoteObject(VM* vm, Obj* object) {
    if (object == NULL) return NULL;
    if (object->generation != GEN_NURSERY) return object;
    if (object->forwarding != NULL) return object->forwarding;

    // Calculate size
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

    // Allocate copy in old gen
    Obj* copy = (Obj*) malloc(size);
    if (copy == NULL) exit(1);

    // Copy object header and data
    memcpy(copy, object, size);

    // Update metadata
    copy->generation = GEN_OLD;
    copy->next = vm->objects;
    copy->forwarding = NULL;
    copy->isMarked = false;
    vm->objects = copy;

    // Set forwarding pointer in original
    object->forwarding = copy;

    vm->bytesAllocated += size;
    vm->promotedBytes += size;

    // CRITICAL: Duplicate external allocations so the promoted copy owns its own memory
    // The nursery copy's external pointers will become invalid when nursery resets,
    // but that's okay since we won't access them again.
    switch (object->type) {
    case OBJ_CLOSURE: {
        ObjClosure* oldClosure = (ObjClosure*) object;
        ObjClosure* newClosure = (ObjClosure*) copy;

        // Duplicate field ICs
        if (oldClosure->fieldICs != NULL && oldClosure->function->fieldICCount > 0) {
            newClosure->fieldICs = malloc(sizeof(FieldIC) * oldClosure->function->fieldICCount);
            memcpy(newClosure->fieldICs, oldClosure->fieldICs,
                sizeof(FieldIC) * oldClosure->function->fieldICCount);
            vm->bytesAllocated += sizeof(FieldIC) * oldClosure->function->fieldICCount;
        }

        // Duplicate method ICs
        if (oldClosure->methodICs != NULL && oldClosure->function->methodICCount > 0) {
            newClosure->methodICs = malloc(sizeof(MethodIC) * oldClosure->function->methodICCount);
            memcpy(newClosure->methodICs, oldClosure->methodICs,
                sizeof(MethodIC) * oldClosure->function->methodICCount);
            vm->bytesAllocated += sizeof(MethodIC) * oldClosure->function->methodICCount;
        }
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* oldFunc = (ObjFunction*) object;
        ObjFunction* newFunc = (ObjFunction*) copy;

        // Duplicate chunk's code array
        if (oldFunc->chunk.code != NULL && oldFunc->chunk.capacity > 0) {
            newFunc->chunk.code = malloc(oldFunc->chunk.capacity);
            memcpy(newFunc->chunk.code, oldFunc->chunk.code, oldFunc->chunk.capacity);
            vm->bytesAllocated += oldFunc->chunk.capacity;
        }

        // Duplicate chunk's lines array (same capacity as code)
        if (oldFunc->chunk.lines != NULL && oldFunc->chunk.capacity > 0) {
            newFunc->chunk.lines = malloc(sizeof(int) * oldFunc->chunk.capacity);
            memcpy(newFunc->chunk.lines, oldFunc->chunk.lines,
                sizeof(int) * oldFunc->chunk.capacity);
            vm->bytesAllocated += sizeof(int) * oldFunc->chunk.capacity;
        }

        // Duplicate chunk's constants array
        if (oldFunc->chunk.constants.values != NULL && oldFunc->chunk.constants.capacity > 0) {
            newFunc->chunk.constants.values = malloc(sizeof(Value) * oldFunc->chunk.constants.capacity);
            memcpy(newFunc->chunk.constants.values, oldFunc->chunk.constants.values,
                sizeof(Value) * oldFunc->chunk.constants.capacity);
            vm->bytesAllocated += sizeof(Value) * oldFunc->chunk.constants.capacity;
        }
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* oldInst = (ObjInstance*) object;
        ObjInstance* newInst = (ObjInstance*) copy;

        // Duplicate fields array
        if (oldInst->fields != NULL && oldInst->klass->fieldCount > 0) {
            int fieldSize = oldInst->klass->fieldCount > 0 ? oldInst->klass->fieldCount : 1;
            newInst->fields = malloc(sizeof(Value) * fieldSize);
            memcpy(newInst->fields, oldInst->fields, sizeof(Value) * fieldSize);
            vm->bytesAllocated += sizeof(Value) * fieldSize;
        }
        break;
    }
    case OBJ_LIST: {
        ObjList* oldList = (ObjList*) object;
        ObjList* newList = (ObjList*) copy;

        // Duplicate items array
        if (oldList->items.values != NULL && oldList->items.capacity > 0) {
            newList->items.values = malloc(sizeof(Value) * oldList->items.capacity);
            memcpy(newList->items.values, oldList->items.values,
                sizeof(Value) * oldList->items.capacity);
            vm->bytesAllocated += sizeof(Value) * oldList->items.capacity;
        }
        break;
    }
    case OBJ_CLASS: {
        ObjClass* oldClass = (ObjClass*) object;
        ObjClass* newClass = (ObjClass*) copy;

        // Duplicate methods array
        if (oldClass->methods != NULL && oldClass->methodCapacity > 0) {
            newClass->methods = malloc(sizeof(MethodEntry) * oldClass->methodCapacity);
            memcpy(newClass->methods, oldClass->methods,
                sizeof(MethodEntry) * oldClass->methodCapacity);
            vm->bytesAllocated += sizeof(MethodEntry) * oldClass->methodCapacity;
        }

        // Duplicate methodIndices table
        if (oldClass->methodIndices.entries != NULL && oldClass->methodIndices.capacity > 0) {
            newClass->methodIndices.entries = malloc(sizeof(Entry) * oldClass->methodIndices.capacity);
            memcpy(newClass->methodIndices.entries, oldClass->methodIndices.entries,
                sizeof(Entry) * oldClass->methodIndices.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldClass->methodIndices.capacity;
        }

        // Duplicate fieldIndices table
        if (oldClass->fieldIndices.entries != NULL && oldClass->fieldIndices.capacity > 0) {
            newClass->fieldIndices.entries = malloc(sizeof(Entry) * oldClass->fieldIndices.capacity);
            memcpy(newClass->fieldIndices.entries, oldClass->fieldIndices.entries,
                sizeof(Entry) * oldClass->fieldIndices.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldClass->fieldIndices.capacity;
        }
        break;
    }
    case OBJ_MODULE: {
        ObjModule* oldMod = (ObjModule*) object;
        ObjModule* newMod = (ObjModule*) copy;

        // Duplicate globalNames table
        if (oldMod->globalNames.entries != NULL && oldMod->globalNames.capacity > 0) {
            newMod->globalNames.entries = malloc(sizeof(Entry) * oldMod->globalNames.capacity);
            memcpy(newMod->globalNames.entries, oldMod->globalNames.entries,
                sizeof(Entry) * oldMod->globalNames.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldMod->globalNames.capacity;
        }

        // Duplicate globalValues array
        if (oldMod->globalValues.values != NULL && oldMod->globalValues.capacity > 0) {
            newMod->globalValues.values = malloc(sizeof(Value) * oldMod->globalValues.capacity);
            memcpy(newMod->globalValues.values, oldMod->globalValues.values,
                sizeof(Value) * oldMod->globalValues.capacity);
            vm->bytesAllocated += sizeof(Value) * oldMod->globalValues.capacity;
        }

        // Duplicate classInfo table
        if (oldMod->classInfo.entries != NULL && oldMod->classInfo.capacity > 0) {
            newMod->classInfo.entries = malloc(sizeof(Entry) * oldMod->classInfo.capacity);
            memcpy(newMod->classInfo.entries, oldMod->classInfo.entries,
                sizeof(Entry) * oldMod->classInfo.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldMod->classInfo.capacity;
        }
        break;
    }
    case OBJ_TABLE: {
        ObjTable* oldTable = (ObjTable*) object;
        ObjTable* newTable = (ObjTable*) copy;

        // Duplicate table entries
        if (oldTable->table.entries != NULL && oldTable->table.capacity > 0) {
            newTable->table.entries = malloc(sizeof(Entry) * oldTable->table.capacity);
            memcpy(newTable->table.entries, oldTable->table.entries,
                sizeof(Entry) * oldTable->table.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldTable->table.capacity;
        }
        break;
    }
    case OBJ_NATIVE_CLASS: {
        ObjNativeClass* oldNC = (ObjNativeClass*) object;
        ObjNativeClass* newNC = (ObjNativeClass*) copy;

        // Duplicate methods table
        if (oldNC->methods.entries != NULL && oldNC->methods.capacity > 0) {
            newNC->methods.entries = malloc(sizeof(Entry) * oldNC->methods.capacity);
            memcpy(newNC->methods.entries, oldNC->methods.entries,
                sizeof(Entry) * oldNC->methods.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldNC->methods.capacity;
        }
        break;
    }
    case OBJ_NATIVE_MODULE: {
        ObjNativeModule* oldNM = (ObjNativeModule*) object;
        ObjNativeModule* newNM = (ObjNativeModule*) copy;

        // Duplicate globals table
        if (oldNM->globals.entries != NULL && oldNM->globals.capacity > 0) {
            newNM->globals.entries = malloc(sizeof(Entry) * oldNM->globals.capacity);
            memcpy(newNM->globals.entries, oldNM->globals.entries,
                sizeof(Entry) * oldNM->globals.capacity);
            vm->bytesAllocated += sizeof(Entry) * oldNM->globals.capacity;
        }
        break;
    }
    default:
        // Other types don't have external allocations
        break;
    }

    return copy;
}

// Update a value, promoting nursery objects
static Value promoteValue(VM* vm, Value value) {
    if (!IS_OBJ(value)) return value;
    Obj* obj = AS_OBJ(value);
    Obj* promoted = promoteObject(vm, obj);
    return OBJ_VAL(promoted);
}

// Scan and update references in an object
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
        // Update table keys
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
        // No references to update
        break;
    }
}

void minorGC(VM* vm) {
    if (vm->inMinorGC) return;

    // Don't GC if nursery is empty or not initialized
    if (vm->nurseryAllocated == 0 || vm->nursery.fromSpace == NULL) {
        return;
    }

    // Don't GC if inhibited (during critical object construction)
    if (vm->gcInhibit > 0) {
        return;
}

    vm->inMinorGC = true;

#ifdef DEBUG_LOG_GC
    fprintf(stderr, "-- minor gc begin (nursery: %zu bytes)\n", vm->nurseryAllocated);
    fflush(stderr);
    size_t promotedBefore = vm->promotedBytes;
#endif

    // Remember where old gen list started - we'll only scan newly promoted objects
    Obj* oldGenHead = vm->objects;

    // Phase 1: Promote all live objects from roots

    // Stack roots
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        *slot = promoteValue(vm, *slot);
    }

    // Frame roots
    for (int i = 0; i < vm->frameCount; i++) {
        vm->frames[i].closure = (ObjClosure*) promoteObject(vm, (Obj*) vm->frames[i].closure);
    }

    // Module roots
    if (vm->rootModule) {
        vm->rootModule = (ObjModule*) promoteObject(vm, (Obj*) vm->rootModule);
    }

    // Native classes
    if (vm->stringClass) {
        vm->stringClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->stringClass);
    }
    if (vm->numberClass) {
        vm->numberClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->numberClass);
    }
    if (vm->listClass) {
        vm->listClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->listClass);
    }
    if (vm->tableClass) {
        vm->tableClass = (ObjNativeClass*) promoteObject(vm, (Obj*) vm->tableClass);
    }

    // String table - strings are always in old gen, so no promotion needed
    // But we still need to check in case any values reference nursery objects
    // (though string table values are always NULL_VAL for interning)

    // Modules table
    if (vm->modules.entries != NULL) {
        for (int i = 0; i < vm->modules.capacity; i++) {
            Entry* entry = &vm->modules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    // Native modules
    if (vm->nativeModules.entries != NULL) {
        for (int i = 0; i < vm->nativeModules.capacity; i++) {
            Entry* entry = &vm->nativeModules.entries[i];
            if (!IS_EMPTY(entry->key)) {
                entry->key = promoteValue(vm, entry->key);
                entry->value = promoteValue(vm, entry->value);
            }
        }
    }

    // Init string
    if (vm->initString) {
        vm->initString = (ObjString*) promoteObject(vm, (Obj*) vm->initString);
    }

    // Last return value
    vm->lastReturnValue = promoteValue(vm, vm->lastReturnValue);

    // Remembered set - scan old gen objects that point to nursery
    for (int i = 0; i < vm->rememberedSet.count; i++) {
        scanObject(vm, vm->rememberedSet.entries[i].object);
    }

    // Phase 2: Scan promoted objects iteratively
    // Use isMarked temporarily to track which objects we've scanned
    // (We'll clear it before returning)
    bool madeProgress = true;
    while (madeProgress) {
        madeProgress = false;
        Obj* obj = vm->objects;
        while (obj != NULL && obj != oldGenHead) {
            if (!obj->isMarked) {
                obj->isMarked = true;  // Mark as scanned
                scanObject(vm, obj);   // May promote more objects
                madeProgress = true;   // We did work, check for new objects
            }
            obj = obj->next;
        }
    }

    // Clear the isMarked flags we set
    for (Obj* obj = vm->objects; obj != NULL && obj != oldGenHead; obj = obj->next) {
        obj->isMarked = false;
    }

    // Phase 3: Reset nursery
    vm->nursery.allocPtr = vm->nursery.fromSpace;
    vm->nurseryAllocated = 0;

    // Clear remembered set
    vm->rememberedSet.count = 0;

    vm->minorGCCount++;
    vm->inMinorGC = false;

#ifdef DEBUG_LOG_GC
    fprintf(stderr, "-- minor gc end (promoted %zu bytes)\n", vm->promotedBytes - promotedBefore);
    fflush(stderr);
#endif

    // If old gen is getting big, trigger major GC
    if (vm->bytesAllocated > vm->nextGC) {
        majorGC(vm);
    }
}

// ============================================
// MAJOR GC (Old Generation Collection)
// ============================================

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
            FREE_ARRAY(vm, FieldIC, closure->fieldICs,
                closure->function->fieldICCount);
        }
        if (closure->methodICs != NULL) {
            FREE_ARRAY(vm, MethodIC, closure->methodICs,
                closure->function->methodICCount);
        }
        size_t size = sizeof(ObjClosure) + sizeof(RuntimeUpvalue) * closure->upvalueCount;
        reallocate(vm, object, size, 0);
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
    case OBJ_NATIVE_METHOD: {
        FREE(vm, ObjNativeMethod, object);
        break;
    }
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
    // Stack roots
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        markValue(vm, *slot);
    }

    // Frame roots
    for (int i = 0; i < vm->frameCount; i++) {
        markObject(vm, (Obj*) vm->frames[i].closure);
    }

    // Module registry
    markTable(vm, &vm->modules);

    // Root module
    if (vm->rootModule) markObject(vm, (Obj*) vm->rootModule);

    // Native classes and modules
    if (vm->stringClass != NULL) markObject(vm, (Obj*) vm->stringClass);
    if (vm->numberClass != NULL) markObject(vm, (Obj*) vm->numberClass);
    if (vm->listClass != NULL) markObject(vm, (Obj*) vm->listClass);
    if (vm->tableClass != NULL) markObject(vm, (Obj*) vm->tableClass);
    markTable(vm, &vm->nativeModules);

    // Compiler roots
    markCompilerRoots(vm);

    // Init string
    markObject(vm, (Obj*) vm->initString);

    // Last return value
    markValue(vm, vm->lastReturnValue);
}

void majorGC(VM* vm) {
    // Don't GC if inhibited
    if (vm->gcInhibit > 0) {
        return;
    }

#ifdef DEBUG_LOG_GC
    fprintf(stderr, "-- major gc begin\n");
    fflush(stderr);
    size_t before = vm->bytesAllocated;
#endif

    // Mark phase
    markRoots(vm);

    // Trace references
    while (vm->grayCount > 0) {
        Obj* object = vm->grayStack[--vm->grayCount];
        blackenObject(vm, object);
    }

    // Sweep weak references
    tableRemoveWhite(&vm->strings);

    // Sweep phase
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

    vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;
    vm->majorGCCount++;

#ifdef DEBUG_LOG_GC
    fprintf(stderr, "-- major gc end (collected %zu bytes, %zu -> %zu)\n",
        before - vm->bytesAllocated, before, vm->bytesAllocated);
    fflush(stderr);
#endif
        }

void collectGarbage(VM* vm) {
    if (vm->nurseryAllocated > NURSERY_THRESHOLD) {
        minorGC(vm);
    } else {
        majorGC(vm);
    }
}

// ============================================
// CLEANUP
// ============================================

void freeObjects(VM* vm) {
    Obj* object = vm->objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(vm, object);
        object = next;
    }

    free(vm->grayStack);
    freeNursery(&vm->nursery);
    freeRememberedSet(vm, &vm->rememberedSet);
}