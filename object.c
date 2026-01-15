#include <stdio.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "vm.h"

#define ALLOCATE_OBJ(vm, type, ot) (type*)allocateObject(vm, sizeof(type), ot)
static Obj* allocateObject(VM* vm, size_t size, ObjType type) {
    Obj* obj = (Obj*) reallocate(vm, NULL, 0, size);
    obj->type = type; obj->isMarked = false; obj->next = vm->objects; vm->objects = obj;
    return obj;
}
ObjBoundMethod* newBoundMethod(VM* vm, Value r, ObjClosure* m) {
    ObjBoundMethod* b = ALLOCATE_OBJ(vm, ObjBoundMethod, OBJ_BOUND_METHOD);
    b->receiver = r; b->method = m; return b;
}
ObjClass* newClass(VM* vm, ObjString* name) {
    ObjClass* k = ALLOCATE_OBJ(vm, ObjClass, OBJ_CLASS);
    k->name = name; initTable(&k->methods); return k;
}
ObjClosure* newClosure(VM* vm, ObjFunction* f) {
    ObjUpvalue** u = ALLOCATE(vm, ObjUpvalue*, f->upvalueCount);
    for (int i = 0; i < f->upvalueCount; i++) u[i] = NULL;
    ObjClosure* c = ALLOCATE_OBJ(vm, ObjClosure, OBJ_CLOSURE);
    c->function = f; c->upvalues = u; c->upvalueCount = f->upvalueCount; return c;
}
ObjFunction* newFunction(VM* vm) {
    ObjFunction* f = ALLOCATE_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    f->arity = 0; f->upvalueCount = 0; f->name = NULL; initChunk(&f->chunk); return f;
}
ObjInstance* newInstance(VM* vm, ObjClass* k) {
    ObjInstance* i = ALLOCATE_OBJ(vm, ObjInstance, OBJ_INSTANCE);
    i->klass = k; initTable(&i->fields); return i;
}
ObjNative* newNative(VM* vm, NativeFn f) {
    ObjNative* n = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
    n->function = f; return n;
}
static ObjString* allocateString(VM* vm, char* c, int len, uint32_t h) {
    ObjString* s = ALLOCATE_OBJ(vm, ObjString, OBJ_STRING);
    s->length = len; s->chars = c; s->hash = h;
    push(vm, OBJ_VAL(s)); tableSet(vm, &vm->strings, s, NIL_VAL); pop(vm);
    return s;
}
static uint32_t hashString(const char* key, int len) {
    uint32_t h = 2166136261u;
    for (int i = 0; i < len; i++) {
        h ^= (uint8_t) key[i]; h *= 16777619;
    }
    return h;
}
ObjString* takeString(VM* vm, char* c, int len) {
    uint32_t h = hashString(c, len);
    ObjString* i = tableFindString(&vm->strings, c, len, h);
    if (i != NULL) {
        FREE_ARRAY(vm, char, c, len + 1); return i;
    }
    return allocateString(vm, c, len, h);
}
ObjString* copyString(VM* vm, const char* c, int len) {
    uint32_t h = hashString(c, len);
    ObjString* i = tableFindString(&vm->strings, c, len, h);
    if (i != NULL) return i;
    char* hc = ALLOCATE(vm, char, len + 1); memcpy(hc, c, len); hc[len] = '\0';
    return allocateString(vm, hc, len, h);
}
ObjUpvalue* newUpvalue(VM* vm, Value* slot) {
    ObjUpvalue* u = ALLOCATE_OBJ(vm, ObjUpvalue, OBJ_UPVALUE);
    u->closed = NIL_VAL; u->location = slot; u->next = NULL; return u;
}
void printObject(Value v) {
    switch (OBJ_TYPE(v)) {
    case OBJ_CLASS: printf("%s", AS_CLASS(v)->name->chars); break;
    case OBJ_BOUND_METHOD: case OBJ_CLOSURE: case OBJ_FUNCTION: {
        ObjFunction* f = (OBJ_TYPE(v) == OBJ_FUNCTION) ? AS_FUNCTION(v) : (OBJ_TYPE(v) == OBJ_CLOSURE ? AS_CLOSURE(v)->function : AS_BOUND_METHOD(v)->method->function);
        if (f->name == NULL) printf("<script>"); else printf("<fn %s>", f->name->chars);
        break;
    }
    case OBJ_INSTANCE: printf("%s instance", AS_INSTANCE(v)->klass->name->chars); break;
    case OBJ_NATIVE: printf("<native fn>"); break;
    case OBJ_STRING: printf("%s", AS_CSTRING(v)); break;
    case OBJ_UPVALUE: printf("upvalue"); break;
    }
}