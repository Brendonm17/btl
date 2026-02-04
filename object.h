#define _POSIX_C_SOURCE 199309L
#ifndef btl_object_h
#define btl_object_h

#include <pthread.h>
#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"
#include "ic.h"

// --- Forward Declarations ---
typedef struct VM VM;
typedef struct ObjUpvalue ObjUpvalue;
typedef struct RuntimeUpvalue RuntimeUpvalue;
typedef struct ObjClosure ObjClosure;
typedef struct ObjModule ObjModule;
typedef struct ObjString ObjString;
typedef struct ObjFuture ObjFuture;
typedef struct ObjActor ObjActor;

// --- Object Types ---
typedef enum {
    OBJ_BOUND_METHOD, OBJ_CLASS, OBJ_CLOSURE, OBJ_FUNCTION,
    OBJ_INSTANCE, OBJ_LIST, OBJ_MODULE, OBJ_NATIVE, OBJ_STRING,
    OBJ_UPVALUE, OBJ_TABLE,
    OBJ_NATIVE_METHOD, OBJ_NATIVE_CLASS, OBJ_NATIVE_MODULE,
    OBJ_FUTURE, OBJ_ACTOR
} ObjType;

struct Obj {
    ObjType type;
    bool isMarked;
    struct Obj* next;
};

// Future states
typedef enum {
    FUTURE_PENDING,
    FUTURE_READY,
    FUTURE_ERROR
} FutureState;

typedef struct ObjFuture {
    Obj obj;
    volatile FutureState state;
    Value result;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
} ObjFuture;

typedef struct {
    struct ObjString* method;
    Value* args;
    int argCount;
    ObjFuture* future;
} ActorMessage;

typedef struct {
    ActorMessage* messages;
    int capacity;
    int count;
    int head;
    int tail;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
} MessageQueue;

typedef struct ObjActor {
    Obj obj;
    struct VM* vm;
    Value instance;
    struct ObjClass* klass;
    MessageQueue inbox;
    pthread_t thread;
    volatile bool alive;
    volatile bool shouldStop;
} ObjActor;

// --- Native Method/Class/Module Types ---
typedef Value (*NativeFn)(int argCount, Value* args);
typedef Value (*NativeMethodFn)(VM* vm, Value receiver, int argCount, Value* args);

typedef struct {
    Obj obj;
    NativeMethodFn function;
    ObjString* name;
    int arity;
} ObjNativeMethod;

typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjNativeClass;

typedef struct {
    Obj obj;
    ObjString* name;
    Table globals;
} ObjNativeModule;

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
#define IS_TABLE(v)        isObjType(v, OBJ_TABLE)
#define IS_NATIVE_METHOD(v)  isObjType(v, OBJ_NATIVE_METHOD)
#define IS_NATIVE_CLASS(v)   isObjType(v, OBJ_NATIVE_CLASS)
#define IS_NATIVE_MODULE(v)  isObjType(v, OBJ_NATIVE_MODULE)
#define IS_FUTURE(value)    isObjType(value, OBJ_FUTURE)
#define IS_ACTOR(value)     isObjType(value, OBJ_ACTOR)

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
#define AS_TABLE(v)        ((ObjTable*)AS_OBJ(v))
#define AS_NATIVE_METHOD(v)  ((ObjNativeMethod*)AS_OBJ(v))
#define AS_NATIVE_CLASS(v)   ((ObjNativeClass*)AS_OBJ(v))
#define AS_NATIVE_MODULE(v)  ((ObjNativeModule*)AS_OBJ(v))
#define AS_FUTURE(value)    ((ObjFuture*)AS_OBJ(value))
#define AS_ACTOR(value)     ((ObjActor*)AS_OBJ(value))

// --- Existing Object Definitions ---
struct ObjUpvalue {
    Obj obj;
    Value closed;
};

struct RuntimeUpvalue {
    bool isOpen;
    bool isMutable;
    union {
        Value* stack;
        ObjUpvalue* box;
        Value immValue;
    } loc;
    struct RuntimeUpvalue* next;
};

struct ObjClosure {
    Obj obj;
    struct ObjFunction* function;
    int upvalueCount;
    FieldIC* fieldICs;
    MethodIC* methodICs;
    RuntimeUpvalue upvalues [];
};

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;
};

struct ObjModule {
    Obj obj;
    ObjString* name;
    Table globalNames;
    ValueArray globalValues;
    Table classInfo;
};

typedef struct ObjFunction {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
    ObjModule* module;
    int fieldICCount;
    int methodICCount;
} ObjFunction;

typedef struct ObjNative {
    Obj obj;
    NativeFn function;
} ObjNative;

typedef struct {
    ObjClosure* closure;
    uint8_t arity;
    ObjString* name;
} MethodEntry;

typedef struct ObjClass {
    Obj obj;
    ObjString* name;
    MethodEntry* methods;
    int methodCount;
    int methodCapacity;
    Table methodIndices;
    Table fieldIndices;
    int fieldCount;
} ObjClass;

typedef struct ObjInstance {
    Obj obj;
    ObjClass* klass;
    Value* fields;
} ObjInstance;

typedef struct ObjTable {
    Obj obj;
    Table table;
} ObjTable;

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
ObjClosure* newClosure(VM* vm, ObjFunction* function);
ObjUpvalue* newUpvalueBox(VM* vm, Value value);
ObjFunction* newFunction(VM* vm, ObjModule* module);
ObjClass* newClass(VM* vm, ObjString* name);
ObjInstance* newInstance(VM* vm, ObjClass* klass);
ObjBoundMethod* newBoundMethod(VM* vm, Value receiver, ObjClosure* method);
ObjList* newList(VM* vm);
ObjModule* newModule(VM* vm, ObjString* name);
ObjNative* newNative(VM* vm, NativeFn function);
ObjString* takeString(VM* vm, char* chars, int length);
ObjString* copyString(VM* vm, const char* chars, int length);
ObjTable* newTable(VM* vm);

ObjNativeMethod* newNativeMethod(VM* vm, NativeMethodFn fn, const char* name, int arity);
ObjNativeClass* newNativeClass(VM* vm, const char* name);
ObjNativeModule* newNativeModule(VM* vm, const char* name);
void defineNativeMethod(VM* vm, ObjNativeClass* klass, const char* name, NativeMethodFn fn, int arity);
void defineNativeModuleValue(VM* vm, ObjNativeModule* module, const char* name, Value value);
void defineNativeModuleFn(VM* vm, ObjNativeModule* module, const char* name, NativeFn fn, int arity);

ObjFuture* newFuture(struct VM* vm);
void futureResolve(ObjFuture* future, Value result);
void futureReject(ObjFuture* future, Value error);
Value futureGet(ObjFuture* future);
FutureState futureGetState(ObjFuture* future);
void freeFuture(ObjFuture* future);

void messageQueueInit(MessageQueue* queue);
void messageQueueFree(MessageQueue* queue);
void messageQueuePush(MessageQueue* queue, ActorMessage message);
bool messageQueuePop(MessageQueue* queue, ActorMessage* out, int timeoutMs);

ObjActor* newActor(struct VM* parentVM, ObjClass* klass, Value* args, int argCount);
void actorSend(ObjActor* actor, ObjString* method, Value* args, int argCount, ObjFuture* future);
void actorStop(ObjActor* actor);
// Deep copy for passing data to actors
Value deepCopyValue(struct VM* destVM, struct VM* srcVM, Value value);

void printObject(Value value);

static inline bool isObjType(Value v, ObjType t) {
    return IS_OBJ(v) && AS_OBJ(v)->type == t;
}

#endif