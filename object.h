#ifndef btl_object_h
#define btl_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

// --- Forward Declarations ---
// Necessary to allow structs to reference each other
typedef struct ObjUpvalue ObjUpvalue;       // The "Box"
typedef struct RuntimeUpvalue RuntimeUpvalue; // The "Inline" struct
typedef struct ObjClosure ObjClosure;
typedef struct ObjModule ObjModule;

// --- Macros ---
#define OBJ_TYPE(v)        (AS_OBJ(v)->type)
#define IS_BOUND_METHOD(v) isObjType(v, OBJ_BOUND_METHOD)
#define IS_CLASS(v)        isObjType(v, OBJ_CLASS)
#define IS_CLOSURE(v)      isObjType(v, OBJ_CLOSURE)
#define IS_FUNCTION(v)     isObjType(v, OBJ_FUNCTION)
#define IS_INSTANCE(v)     isObjType(v, OBJ_INSTANCE)
#define IS_LIST(v)         isObjType(v, OBJ_LIST)
#define IS_MODULE(v)       isObjType(v, OBJ_MODULE)
#define IS_NATIVE(v)       isObjType(v, OBJ_NATIVE)
#define IS_STRING(v)       isObjType(v, OBJ_STRING)
#define IS_UPVALUE_BOX(v)  isObjType(v, OBJ_UPVALUE)

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
    OBJ_INSTANCE, OBJ_LIST, OBJ_MODULE, OBJ_NATIVE, OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

struct Obj {
    ObjType type;
    bool isMarked;
    struct Obj* next;
};

// --- Upvalue System ---

// 1. The Box (Heap Object)
// Stores the value when closed.
struct ObjUpvalue {
    Obj obj;
    Value closed;
};

// 2. The Inline Struct (RuntimeUpvalue)
// Lives inside ObjClosure. Renamed to avoid compiler.h conflict.
struct RuntimeUpvalue {
    bool isOpen;
    union {
        Value* stack;       // Open
        ObjUpvalue* box;    // Closed
    } loc;
    struct RuntimeUpvalue* next;
};

// 3. The Closure
typedef struct ObjClosure {
    Obj obj;
    struct ObjFunction* function;
    int upvalueCount;
    RuntimeUpvalue upvalues []; // Flexible Array Member
} ObjClosure;

// --- Other Objects (Restored) ---

typedef struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;
} ObjString;

typedef struct ObjModule {
    Obj obj;
    ObjString* name;
    Table globalNames;
    ValueArray globalValues;
} ObjModule;

typedef struct ObjFunction {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
    ObjModule* module;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct ObjNative {
    Obj obj;
    NativeFn function;
} ObjNative;

typedef struct ObjClass {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

typedef struct ObjInstance {
    Obj obj;
    ObjClass* klass;
    Table fields;
} ObjInstance;

typedef struct ObjList {
    Obj obj;
    ValueArray items;
} ObjList;

typedef struct ObjBoundMethod {
    Obj obj;
    Value receiver;
    ObjClosure* method;
} ObjBoundMethod;

// --- Prototypes ---

ObjClosure* newClosure(struct VM* vm, ObjFunction* function);
ObjUpvalue* newUpvalueBox(struct VM* vm, Value value);
ObjFunction* newFunction(struct VM* vm, ObjModule* module);
ObjClass* newClass(struct VM* vm, ObjString* name);
ObjInstance* newInstance(struct VM* vm, ObjClass* klass);
ObjBoundMethod* newBoundMethod(struct VM* vm, Value receiver, ObjClosure* method);
ObjList* newList(struct VM* vm);
ObjModule* newModule(struct VM* vm, ObjString* name);
ObjNative* newNative(struct VM* vm, NativeFn function);
ObjString* takeString(struct VM* vm, char* chars, int length);
ObjString* copyString(struct VM* vm, const char* chars, int length);

void printObject(Value value);
void printObjectStderr(Value value);

static inline bool isObjType(Value v, ObjType t) {
    return IS_OBJ(v) && AS_OBJ(v)->type == t;
}

#endif