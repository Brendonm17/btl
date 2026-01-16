#ifndef btl_object_h
#define btl_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

struct VM;

#define OBJ_TYPE(v) (AS_OBJ(v)->type)
#define IS_BOUND_METHOD(v) isObjType(v, OBJ_BOUND_METHOD)
#define IS_CLASS(v)        isObjType(v, OBJ_CLASS)
#define IS_CLOSURE(v)      isObjType(v, OBJ_CLOSURE)
#define IS_FUNCTION(v)     isObjType(v, OBJ_FUNCTION)
#define IS_INSTANCE(v)     isObjType(v, OBJ_INSTANCE)
#define IS_LIST(v)         isObjType(v, OBJ_LIST)
#define IS_MODULE(v)       isObjType(v, OBJ_MODULE)
#define IS_NATIVE(v)       isObjType(v, OBJ_NATIVE)
#define IS_STRING(v)       isObjType(v, OBJ_STRING)

#define AS_BOUND_METHOD(v) ((ObjBoundMethod*)AS_OBJ(v))
#define AS_CLASS(v)        ((ObjClass*)AS_OBJ(v))
#define AS_CLOSURE(v)      ((ObjClosure*)AS_OBJ(v))
#define AS_FUNCTION(v)     ((ObjFunction*)AS_OBJ(v))
#define AS_INSTANCE(v)     ((ObjInstance*)AS_OBJ(v))
#define AS_LIST(v)         ((ObjList*)AS_OBJ(v))
#define AS_MODULE(v)       ((ObjModule*)AS_OBJ(v))
#define AS_NATIVE(v)       (((ObjNative*)AS_OBJ(v))->function)
#define AS_STRING(v)       ((ObjString*)AS_OBJ(v))
#define AS_CSTRING(v)      (((ObjString*)AS_OBJ(v))->chars)

typedef enum {
    OBJ_BOUND_METHOD, OBJ_CLASS, OBJ_CLOSURE, OBJ_FUNCTION,
    OBJ_INSTANCE, OBJ_LIST, OBJ_MODULE, OBJ_NATIVE, OBJ_STRING, OBJ_UPVALUE
} ObjType;

typedef struct Obj {
    ObjType type; bool isMarked; struct Obj* next;
} Obj;

typedef struct ObjModule {
    Obj obj; struct ObjString* name; Table globalNames; ValueArray globalValues;
} ObjModule;

typedef struct {
    Obj obj; int arity; int upvalueCount; Chunk chunk; struct ObjString* name; ObjModule* module;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct {
    Obj obj; NativeFn function;
} ObjNative;
typedef struct ObjString {
    Obj obj; int length; char* chars; uint32_t hash;
} ObjString;
typedef struct ObjUpvalue {
    Obj obj; Value* location; Value closed; struct ObjUpvalue* next;
} ObjUpvalue;
typedef struct ObjClosure {
    Obj obj; ObjFunction* function; ObjUpvalue** upvalues; int upvalueCount;
} ObjClosure;
typedef struct {
    Obj obj; struct ObjString* name; Table methods;
} ObjClass;
typedef struct {
    Obj obj; ObjClass* klass; Table fields;
} ObjInstance;
typedef struct {
    Obj obj; ValueArray items;
} ObjList;
typedef struct {
    Obj obj; Value receiver; ObjClosure* method;
} ObjBoundMethod;

ObjBoundMethod* newBoundMethod(struct VM* vm, Value receiver, ObjClosure* method);
ObjClass* newClass(struct VM* vm, struct ObjString* name);
ObjClosure* newClosure(struct VM* vm, ObjFunction* function);
ObjFunction* newFunction(struct VM* vm, ObjModule* module);
ObjInstance* newInstance(struct VM* vm, ObjClass* klass);
ObjList* newList(struct VM* vm);
ObjModule* newModule(struct VM* vm, struct ObjString* name);
ObjNative* newNative(struct VM* vm, NativeFn function);
struct ObjString* takeString(struct VM* vm, char* chars, int length);
struct ObjString* copyString(struct VM* vm, const char* chars, int length);
ObjUpvalue* newUpvalue(struct VM* vm, Value* slot);
void printObject(Value value);
void printObjectStderr(Value value);
static inline bool isObjType(Value v, ObjType t) {
    return IS_OBJ(v) && AS_OBJ(v)->type == t;
}
#endif