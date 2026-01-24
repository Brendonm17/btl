#include <stdlib.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

void* reallocate(struct VM* vm, void* pointer, size_t oldSize, size_t newSize) {
    vm->bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
        if (vm->bytesAllocated > vm->nextGC) collectGarbage(vm);
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

void markObject(struct VM* vm, struct Obj* object) {
    if (object == NULL || object->isMarked) return;
    object->isMarked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack = (struct Obj**) realloc(vm->grayStack, sizeof(struct Obj*) * vm->grayCapacity);
        if (vm->grayStack == NULL) exit(1);
    }

    vm->grayStack[vm->grayCount++] = object;
}

void markValue(struct VM* vm, Value value) {
    if (IS_OBJ(value)) markObject(vm, AS_OBJ(value));
}

static void markArray(struct VM* vm, ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(vm, array->values[i]);
    }
}

static void blackenObject(struct VM* vm, struct Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD: {
        ObjBoundMethod* b = (ObjBoundMethod*) object;
        markValue(vm, b->receiver);
        markObject(vm, (struct Obj*) b->method);
        break;
    }
    case OBJ_CLASS: {
        ObjClass* k = (ObjClass*) object;
        markObject(vm, (struct Obj*) k->name);
        markTable(vm, &k->methods);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        markObject(vm, (Obj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) {
            RuntimeUpvalue* uv = &closure->upvalues[i];
            if (!uv->isOpen) {
                markObject(vm, (Obj*) uv->loc.box);
            }
        }
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        markObject(vm, (struct Obj*) f->name);
        markArray(vm, &f->chunk.constants);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* i = (ObjInstance*) object;
        markObject(vm, (struct Obj*) i->klass);
        markTable(vm, &i->fields);
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
        markObject(vm, (struct Obj*) m->name);
        markTable(vm, &m->globalNames);
        markArray(vm, &m->globalValues);
        break;
    }
    case OBJ_UPVALUE:
        markValue(vm, ((ObjUpvalue*) object)->closed);
        break;
    case OBJ_NATIVE:
    case OBJ_STRING:
        break;
    }
}

static void freeObject(struct VM* vm, struct Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD:
        FREE(vm, ObjBoundMethod, object);
        break;
    case OBJ_CLASS: {
        ObjClass* k = (ObjClass*) object;
        freeTable(vm, &k->methods);
        FREE(vm, ObjClass, object);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
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
        ObjInstance* i = (ObjInstance*) object;
        freeTable(vm, &i->fields);
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
    }
}

void collectGarbage(struct VM* vm) {
    // Mark stack roots
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        markValue(vm, *slot);
    }

    // Mark frame roots
    for (int i = 0; i < vm->frameCount; i++) {
        markObject(vm, (struct Obj*) vm->frames[i].closure);
    }

    // Mark module registry
    markTable(vm, &vm->modules);

    // Mark root module
    if (vm->rootModule) markObject(vm, (struct Obj*) vm->rootModule);

    // Mark compiler roots (linked chain)
    markCompilerRoots(vm);

    // Mark strings
    markObject(vm, (struct Obj*) vm->initString);

    // Trace references
    while (vm->grayCount > 0) {
        struct Obj* object = vm->grayStack[--vm->grayCount];
        blackenObject(vm, object);
    }

    // Sweep
    tableRemoveWhite(&vm->strings);

    struct Obj* previous = NULL;
    struct Obj* object = vm->objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            struct Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm->objects = object;
            }

            freeObject(vm, unreached);
        }
    }

    vm->nextGC = vm->bytesAllocated * 2;
}

void freeObjects(struct VM* vm) {
    struct Obj* object = vm->objects;
    while (object != NULL) {
        struct Obj* next = object->next;
        freeObject(vm, object);
        object = next;
    }
    free(vm->grayStack);
}