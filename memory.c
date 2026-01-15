#include <stdlib.h>
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(VM* vm, void* pointer, size_t oldSize, size_t newSize) {
    vm->bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
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
        ObjBoundMethod* bound = (ObjBoundMethod*) object;
        markValue(vm, bound->receiver);
        markObject(vm, (Obj*) bound->method);
        break;
    }
    case OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        markObject(vm, (Obj*) klass->name);
        markTable(vm, &klass->methods);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        markObject(vm, (Obj*) closure->function);
        for (int i = 0; i < closure->upvalueCount; i++) markObject(vm, (Obj*) closure->upvalues[i]);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* function = (ObjFunction*) object;
        markObject(vm, (Obj*) function->name);
        markArray(vm, &function->chunk.constants);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        markObject(vm, (Obj*) instance->klass);
        markTable(vm, &instance->fields);
        break;
    }
    case OBJ_UPVALUE: markValue(vm, ((ObjUpvalue*) object)->closed); break;
    case OBJ_NATIVE:
    case OBJ_STRING: break;
    }
}

static void freeObject(VM* vm, Obj* object) {
    switch (object->type) {
    case OBJ_BOUND_METHOD: FREE(vm, ObjBoundMethod, object); break;
    case OBJ_CLASS: {
        ObjClass* klass = (ObjClass*) object;
        freeTable(vm, &klass->methods);
        FREE(vm, ObjClass, object);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*) object;
        FREE_ARRAY(vm, ObjUpvalue*, closure->upvalues, closure->upvalueCount);
        FREE(vm, ObjClosure, object);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* function = (ObjFunction*) object;
        freeChunk(vm, &function->chunk);
        FREE(vm, ObjFunction, object);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*) object;
        freeTable(vm, &instance->fields);
        FREE(vm, ObjInstance, object);
        break;
    }
    case OBJ_NATIVE: FREE(vm, ObjNative, object); break;
    case OBJ_STRING: {
        ObjString* string = (ObjString*) object;
        FREE_ARRAY(vm, char, string->chars, string->length + 1);
        FREE(vm, ObjString, object);
        break;
    }
    case OBJ_UPVALUE: FREE(vm, ObjUpvalue, object); break;
    }
}

void collectGarbage(VM* vm) {
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) markValue(vm, *slot);
    for (int i = 0; i < vm->frameCount; i++) markObject(vm, (Obj*) vm->frames[i].closure);
    for (ObjUpvalue* upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next) markObject(vm, (Obj*) upvalue);

    // Mark Fast Globals
    markTable(vm, &vm->globalNames);
    markArray(vm, &vm->globalValues);

    markCompilerRoots(vm);
    markObject(vm, (Obj*) vm->initString);

    while (vm->grayCount > 0) blackenObject(vm, vm->grayStack[--vm->grayCount]);
    tableRemoveWhite(&vm->strings);

    Obj* previous = NULL;
    Obj* object = vm->objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false; previous = object; object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) previous->next = object; else vm->objects = object;
            freeObject(vm, unreached);
        }
    }
    vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;
}

void freeObjects(VM* vm) {
    Obj* object = vm->objects;
    while (object != NULL) {
        Obj* next = object->next; freeObject(vm, object); object = next;
    }
    free(vm->grayStack);
}