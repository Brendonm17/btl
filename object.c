#include <stdio.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(vm, type, ot) (type*)allocateObject(vm, sizeof(type), ot)

static struct Obj* allocateObject(struct VM* vm, size_t size, ObjType type) {
    struct Obj* object = (struct Obj*) reallocate(vm, NULL, 0, size);
    object->type = type; object->isMarked = false; object->next = vm->objects; vm->objects = object;
    return object;
}

ObjBoundMethod* newBoundMethod(struct VM* vm, Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(vm, ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver; bound->method = method; return bound;
}

ObjClass* newClass(struct VM* vm, struct ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(vm, ObjClass, OBJ_CLASS);
    klass->name = name; initTable(&klass->methods); return klass;
}

ObjClosure* newClosure(struct VM* vm, ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(vm, ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) upvalues[i] = NULL;
    ObjClosure* closure = ALLOCATE_OBJ(vm, ObjClosure, OBJ_CLOSURE);
    closure->function = function; closure->upvalues = upvalues; closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjFunction* newFunction(struct VM* vm, ObjModule* module) {
    ObjFunction* function = ALLOCATE_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    function->arity = 0; function->upvalueCount = 0; function->name = NULL;
    function->module = module; initChunk(&function->chunk); return function;
}

ObjInstance* newInstance(struct VM* vm, ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(vm, ObjInstance, OBJ_INSTANCE);
    instance->klass = klass; initTable(&instance->fields); return instance;
}

ObjList* newList(struct VM* vm) {
    ObjList* list = ALLOCATE_OBJ(vm, ObjList, OBJ_LIST);
    initValueArray(&list->items); return list;
}

ObjModule* newModule(struct VM* vm, struct ObjString* name) {
    ObjModule* module = ALLOCATE_OBJ(vm, ObjModule, OBJ_MODULE);
    module->name = name; initTable(&module->globalNames); initValueArray(&module->globalValues);
    return module;
}

ObjNative* newNative(struct VM* vm, NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
    native->function = function; return native;
}

static struct ObjString* allocateString(struct VM* vm, char* chars, int length, uint32_t hash) {
    struct ObjString* string = (struct ObjString*) allocateObject(vm, sizeof(struct ObjString), OBJ_STRING);
    string->length = length; string->chars = chars; string->hash = hash;
    push(vm, OBJ_VAL(string)); tableSet(vm, &vm->strings, OBJ_VAL(string), NIL_VAL); pop(vm);
    return string;
}

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t) key[i]; hash *= 16777619;
    }
    return hash;
}

struct ObjString* takeString(struct VM* vm, char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(vm, char, chars, length + 1); return interned;
    }
    return allocateString(vm, chars, length, hash);
}

struct ObjString* copyString(struct VM* vm, const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) return interned;
    char* heapChars = ALLOCATE(vm, char, length + 1);
    memcpy(heapChars, chars, length); heapChars[length] = '\0';
    return allocateString(vm, heapChars, length, hash);
}

ObjUpvalue* newUpvalue(struct VM* vm, Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(vm, ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL; upvalue->location = slot; upvalue->next = NULL;
    return upvalue;
}

void printObjectStderr(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_BOUND_METHOD: fprintf(stderr, "<fn %s>", AS_BOUND_METHOD(value)->method->function->name->chars); break;
    case OBJ_CLASS: fprintf(stderr, "%s", AS_CLASS(value)->name->chars); break;
    case OBJ_CLOSURE: fprintf(stderr, "<fn %s>", AS_CLOSURE(value)->function->name->chars); break;
    case OBJ_FUNCTION: fprintf(stderr, "<fn %s>", AS_FUNCTION(value)->name->chars); break;
    case OBJ_INSTANCE: fprintf(stderr, "%s instance", AS_INSTANCE(value)->klass->name->chars); break;
    case OBJ_LIST: {
        ObjList* list = AS_LIST(value); fprintf(stderr, "[");
        for (int i = 0; i < list->items.count; i++) {
            printValueStderr(list->items.values[i]); if (i < list->items.count - 1) fprintf(stderr, ", ");
        }
        fprintf(stderr, "]"); break;
    }
    case OBJ_MODULE: fprintf(stderr, "<module %s>", AS_MODULE(value)->name->chars); break;
    case OBJ_NATIVE: fprintf(stderr, "<native fn>"); break;
    case OBJ_STRING: fprintf(stderr, "%s", AS_CSTRING(value)); break;
    case OBJ_UPVALUE: fprintf(stderr, "upvalue"); break;
    }
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_BOUND_METHOD: printf("<fn %s>", AS_BOUND_METHOD(value)->method->function->name->chars); break;
    case OBJ_CLASS: printf("%s", AS_CLASS(value)->name->chars); break;
    case OBJ_CLOSURE: printf("<fn %s>", AS_CLOSURE(value)->function->name->chars); break;
    case OBJ_FUNCTION: printf("<fn %s>", AS_FUNCTION(value)->name->chars); break;
    case OBJ_INSTANCE: printf("%s instance", AS_INSTANCE(value)->klass->name->chars); break;
    case OBJ_LIST: {
        ObjList* list = AS_LIST(value); printf("[");
        for (int i = 0; i < list->items.count; i++) {
            printValue(list->items.values[i]); if (i < list->items.count - 1) printf(", ");
        }
        printf("]"); break;
    }
    case OBJ_MODULE: printf("<module %s>", AS_MODULE(value)->name->chars); break;
    case OBJ_NATIVE: printf("<native fn>"); break;
    case OBJ_STRING: printf("%s", AS_CSTRING(value)); break;
    case OBJ_UPVALUE: printf("upvalue"); break;
    }
}