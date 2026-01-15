#include <stdlib.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize) {
    vm->bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage(vm);
#endif
        if (vm->bytesAllocated > vm->nextGC) collectGarbage(vm);
    }
    if (newSize == 0) {
        free(pointer); return NULL;
    }
    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
    }

void markObject(VM* vm, Obj* object) {
    if (object == NULL || object->isMarked) return;
    object->isMarked = true;
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

static void markArray(VM* vm, ValueArray* array) {
    for (int i = 0; i < array->count; i++) markValue(vm, array->values[i]);
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
        ObjClass* k = (ObjClass*) object;
        markObject(vm, (Obj*) k->name);
        markTable(vm, &k->methods);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* c = (ObjClosure*) object;
        markObject(vm, (Obj*) c->function);
        for (int i = 0; i < c->upvalueCount; i++) markObject(vm, (Obj*) c->upvalues[i]);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* f = (ObjFunction*) object;
        markObject(vm, (Obj*) f->name);
        markArray(vm, &f->chunk.constants);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* i = (ObjInstance*) object;
        markObject(vm, (Obj*) i->klass);
        markTable(vm, &i->fields);
        break;
    }
    case OBJ_UPVALUE: markValue(vm, ((ObjUpvalue*) object)->closed); break;
    case OBJ_NATIVE: case OBJ_STRING: break;
    }
    }

static void freeObject(VM* vm, Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD: FREE(vm, ObjBoundMethod, object); break;
    case OBJ_CLASS: {
        ObjClass* k = (ObjClass*) object;
        freeTable(vm, &k->methods);
        FREE(vm, ObjClass, object);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* c = (ObjClosure*) object;
        FREE_ARRAY(vm, ObjUpvalue*, c->upvalues, c->upvalueCount);
        FREE(vm, ObjClosure, object);
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
    case OBJ_NATIVE: FREE(vm, ObjNative, object); break;
    case OBJ_STRING: {
        ObjString* s = (ObjString*) object;
        FREE_ARRAY(vm, char, s->chars, s->length + 1);
        FREE(vm, ObjString, object);
        break;
    }
    case OBJ_UPVALUE: FREE(vm, ObjUpvalue, object); break;
    }
    }

void collectGarbage(VM* vm) {
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) markValue(vm, *slot);
    for (int i = 0; i < vm->frameCount; i++) markObject(vm, (Obj*) vm->frames[i].closure);
    for (ObjUpvalue* u = vm->openUpvalues; u != NULL; u = u->next) markObject(vm, (Obj*) u);
    markTable(vm, &vm->globals);
    markCompilerRoots(vm);
    markObject(vm, (Obj*) vm->initString);

    while (vm->grayCount > 0) blackenObject(vm, vm->grayStack[--vm->grayCount]);
    tableRemoveWhite(&vm->strings);

    Obj* prev = NULL;
    Obj* obj = vm->objects;
    while (obj != NULL) {
        if (obj->isMarked) {
            obj->isMarked = false;
            prev = obj;
            obj = obj->next;
        } else {
            Obj* unreached = obj;
            obj = obj->next;
            if (prev != NULL) prev->next = obj; else vm->objects = obj;
            freeObject(vm, unreached);
        }
    }
    vm->nextGC = vm->bytesAllocated * 2;
}

void freeObjects(VM* vm) {
    Obj* obj = vm->objects;
    while (obj != NULL) {
        Obj* next = obj->next;
        freeObject(vm, obj);
        obj = next;
    }
    free(vm->grayStack);
}