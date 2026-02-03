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
    object->type = type;
    object->isMarked = false;
    object->next = vm->objects;
    vm->objects = object;
    return object;
}

ObjNativeMethod* newNativeMethod(VM* vm, NativeMethodFn fn, const char* name, int arity) {
    ObjNativeMethod* method = ALLOCATE_OBJ(vm, ObjNativeMethod, OBJ_NATIVE_METHOD);
    method->function = fn;
    method->name = copyString(vm, name, (int) strlen(name));
    method->arity = arity;
    return method;
}

ObjNativeClass* newNativeClass(VM* vm, const char* name) {
    ObjNativeClass* klass = ALLOCATE_OBJ(vm, ObjNativeClass, OBJ_NATIVE_CLASS);
    klass->name = copyString(vm, name, (int) strlen(name));
    initTable(&klass->methods);
    return klass;
}

ObjNativeModule* newNativeModule(VM* vm, const char* name) {
    ObjNativeModule* module = ALLOCATE_OBJ(vm, ObjNativeModule, OBJ_NATIVE_MODULE);
    module->name = copyString(vm, name, (int) strlen(name));
    initTable(&module->globals);
    return module;
}

void defineNativeMethod(VM* vm, ObjNativeClass* klass, const char* name, NativeMethodFn fn, int arity) {
    ObjNativeMethod* method = newNativeMethod(vm, fn, name, arity);
    push(vm, OBJ_VAL(method));
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &klass->methods, OBJ_VAL(nameStr), OBJ_VAL(method));
    pop(vm);
    pop(vm);
}

void defineNativeModuleValue(VM* vm, ObjNativeModule* module, const char* name, Value value) {
    push(vm, value);
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &module->globals, OBJ_VAL(nameStr), value);
    pop(vm);
    pop(vm);
}

void defineNativeModuleFn(VM* vm, ObjNativeModule* module, const char* name, NativeFn fn, int arity) {
    (void) arity;
    ObjNative* native = newNative(vm, fn);
    push(vm, OBJ_VAL(native));
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &module->globals, OBJ_VAL(nameStr), OBJ_VAL(native));
    pop(vm);
    pop(vm);
}

ObjTable* newTable(struct VM* vm) {
    ObjTable* table = ALLOCATE_OBJ(vm, ObjTable, OBJ_TABLE);
    initTable(&table->table);
    return table;
}

ObjBoundMethod* newBoundMethod(struct VM* vm, Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(vm, ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass* newClass(struct VM* vm, struct ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(vm, ObjClass, OBJ_CLASS);
    klass->name = name;

    // OPTIMIZED: Initialize vtable
    klass->methods = NULL;
    klass->methodCount = 0;
    klass->methodCapacity = 0;

    // Method indices for compile-time lookup
    initTable(&klass->methodIndices);

    // Field system (unchanged)
    klass->fieldCount = 0;
    initTable(&klass->fieldIndices);

    return klass;
}

ObjClosure* newClosure(VM* vm, ObjFunction* function) {
    size_t size = sizeof(ObjClosure) + sizeof(RuntimeUpvalue) * function->upvalueCount;
    ObjClosure* closure = (ObjClosure*) allocateObject(vm, size, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalueCount = function->upvalueCount;

    for (int i = 0; i < function->upvalueCount; i++) {
        closure->upvalues[i].isOpen = true;
        closure->upvalues[i].next = NULL;
    }
    // Allocate and initialize IC arrays
    if (function->fieldICCount > 0) {
        closure->fieldICs = ALLOCATE(vm, FieldIC, function->fieldICCount);
        initFieldICs(closure->fieldICs, function->fieldICCount);
    } else {
        closure->fieldICs = NULL;
    }

    if (function->methodICCount > 0) {
        closure->methodICs = ALLOCATE(vm, MethodIC, function->methodICCount);
        initMethodICs(closure->methodICs, function->methodICCount);
    } else {
        closure->methodICs = NULL;
    }

    return closure;
}

ObjFunction* newFunction(struct VM* vm, ObjModule* module) {
    ObjFunction* function = ALLOCATE_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    function->module = module;
    function->fieldICCount = 0;
    function->methodICCount = 0;
    initChunk(&function->chunk);
    return function;
}

ObjInstance* newInstance(struct VM* vm, ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(vm, ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;

    // Safety: always allocate at least 1 byte if count is 0 
    // to prevent malloc(0) implementation-defined behavior
    int size = klass->fieldCount > 0 ? klass->fieldCount : 1;
    instance->fields = ALLOCATE(vm, Value, size);

    for (int i = 0; i < klass->fieldCount; i++) {
        instance->fields[i] = NULL_VAL;
    }
    return instance;
}

ObjList* newList(struct VM* vm) {
    ObjList* list = ALLOCATE_OBJ(vm, ObjList, OBJ_LIST);
    initValueArray(&list->items);
    return list;
}

ObjModule* newModule(struct VM* vm, struct ObjString* name) {
    ObjModule* module = ALLOCATE_OBJ(vm, ObjModule, OBJ_MODULE);
    module->name = name;
    initTable(&module->globalNames);
    initValueArray(&module->globalValues);
    initTable(&module->classInfo);
    return module;
}

ObjNative* newNative(struct VM* vm, NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

static struct ObjString* allocateString(struct VM* vm, char* chars, int length, uint32_t hash) {
    struct ObjString* string = (struct ObjString*) allocateObject(vm, sizeof(struct ObjString), OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    push(vm, OBJ_VAL(string));
    tableSet(vm, &vm->strings, OBJ_VAL(string), NULL_VAL);
    pop(vm);
    return string;
}

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t) key[i];
        hash *= 16777619;
    }
    return hash;
}

struct ObjString* takeString(struct VM* vm, char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(vm, char, chars, length + 1);
        return interned;
    }
    return allocateString(vm, chars, length, hash);
}

struct ObjString* copyString(struct VM* vm, const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) return interned;
    char* heapChars = ALLOCATE(vm, char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(vm, heapChars, length, hash);
}

ObjUpvalue* newUpvalueBox(VM* vm, Value value) {
    ObjUpvalue* box = ALLOCATE_OBJ(vm, ObjUpvalue, OBJ_UPVALUE);
    box->closed = value;
    return box;
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_BOUND_METHOD:
        printf("<func %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
        break;
    case OBJ_CLASS:
        printf("<class %s>", AS_CLASS(value)->name->chars);
        break;
    case OBJ_CLOSURE:
        printf("<func %s>", AS_CLOSURE(value)->function->name->chars);
        break;
    case OBJ_FUNCTION:
        printf("<func %s>", AS_FUNCTION(value)->name->chars);
        break;
    case OBJ_INSTANCE:
        printf("<%s instance>", AS_INSTANCE(value)->klass->name->chars);
        break;
    case OBJ_LIST: {
        ObjList* list = AS_LIST(value);
        printf("<list>[");
        for (int i = 0; i < list->items.count; i++) {
            printValue(list->items.values[i]);
            if (i < list->items.count - 1) printf(", ");
        }
        printf("]");
        break;
    }
    case OBJ_TABLE:
        printf("<table>");
        break;
    case OBJ_MODULE:
        printf("<module %s>", AS_MODULE(value)->name->chars);
        break;
    case OBJ_NATIVE:
        printf("<native func>");
        break;
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    case OBJ_UPVALUE:
        printf("<upvalue>");
        break;
    case OBJ_NATIVE_METHOD:
        printf("<native method %s>", AS_NATIVE_METHOD(value)->name->chars);
        break;
    case OBJ_NATIVE_CLASS:
        printf("<native class %s>", AS_NATIVE_CLASS(value)->name->chars);
        break;
    case OBJ_NATIVE_MODULE:
        printf("<native module %s>", AS_NATIVE_MODULE(value)->name->chars);
        break;
    default:
        printf("<obj>");
        break;
    }
}