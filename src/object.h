// Heap-allocated object types: strings, functions, classes, instances, lists,
// tables, modules, etc. All objects begin with a BtlObj header (type tag,
// GC marking flag, generation, intrusive list pointer, forwarding pointer
// during minor GC promotion).

#ifndef btl_object_h
#define btl_object_h

#include "common.h"
#include "platform.h"
#include "chunk.h"
#include "table.h"
#include "value.h"
#include "ic.h"

// ----------------------------------------------------------------------------
// Forward Declarations
// ----------------------------------------------------------------------------
typedef struct VM VM;
typedef struct BtlObj BtlObj;
typedef struct ObjUpvalue ObjUpvalue;
typedef struct BtlRuntimeUpvalue BtlRuntimeUpvalue;
typedef struct ObjClosure ObjClosure;
typedef struct ObjModule ObjModule;
typedef struct ObjString ObjString;
typedef struct ObjFuture ObjFuture;
typedef struct ObjActor ObjActor;
typedef struct BtlCallFrame BtlCallFrame;

// ============================================================================
// Object Type Enumeration
//
// Every heap object has a type tag identifying its concrete type.
// ============================================================================
typedef enum BtlObjType {
    BTL_OBJ_BOUND_METHOD,   // Method bound to a receiver
    BTL_OBJ_CLASS,          // Class object
    BTL_OBJ_CLOSURE,        // Function closure (function + captured upvalues)
    BTL_OBJ_FUNCTION,       // Compiled function (bytecode)
    BTL_OBJ_INSTANCE,       // Class instance
    BTL_OBJ_LIST,           // Dynamic array
    BTL_OBJ_MODULE,         // Module (compilation unit)
    BTL_OBJ_NATIVE,         // Native C function
    BTL_OBJ_STRING,         // Interned string
    BTL_OBJ_UPVALUE,        // Closed-over variable box
    BTL_OBJ_TABLE,          // Hash table (dictionary)
    BTL_OBJ_NATIVE_METHOD,  // Native method (takes receiver)
    BTL_OBJ_NATIVE_CLASS,   // Native class wrapper
    BTL_OBJ_NATIVE_MODULE,  // Native module wrapper
    BTL_OBJ_FUTURE,         // Async result placeholder
    BTL_OBJ_ACTOR,          // Actor for concurrency
    BTL_OBJ_ENTITY,         // Entity wrapper for game engine integration
    BTL_OBJ_COROUTINE       // Stackful coroutine (own stack + frames)
} BtlObjType;

// ============================================================================
// Base Object Structure
//
// All objects begin with this header. The type field allows safe downcasting.
// ============================================================================
struct BtlObj {
    BtlObjType type;        // Object type tag
    bool isMarked;          // GC mark flag (false=WHITE, true=GRAY/BLACK)
    uint8_t generation;     // GEN_NURSERY or GEN_OLD
    struct BtlObj* next;    // Linked list for old generation objects
    struct BtlObj* forwarding; // Forwarding pointer during minor GC promotion
};

// ============================================================================
// Future States (for async operations)
// ============================================================================
typedef enum {
    BTL_FUTURE_PENDING,     // Not yet resolved
    BTL_FUTURE_READY,       // Successfully resolved with value
    BTL_FUTURE_ERROR        // Rejected with error
} BtlFutureState;

// ============================================================================
// Future Object
//
// Represents the result of an asynchronous operation. Can be awaited.
// ============================================================================
typedef struct ObjFuture {
    BtlObj obj;
    volatile BtlFutureState state;  // Current state
    BtlValue result;                   // Result value (when ready)
    BtlMutexHandle mutex;           // Synchronization
    BtlCondHandle cond;             // Condition for waiting
    BtlThreadHandles* threadHandles; // Platform thread functions
} ObjFuture;

// ============================================================================
// Actor Message
//
// A message sent to an actor, containing method name, arguments, and
// a future for the response.
// ============================================================================
typedef struct {
    struct ObjString* method;   // Method to invoke
    BtlValue* args;                // Arguments (deep copied)
    int argCount;               // Number of arguments
    ObjFuture* future;          // Future for result
} BtlActorMessage;

// ============================================================================
// Message Queue
//
// Thread-safe queue for actor messages.
// ============================================================================
typedef struct {
    BtlActorMessage* messages;  // Circular buffer
    int capacity;               // Buffer capacity
    int count;                  // Current message count
    int head;                   // Read position
    int tail;                   // Write position
    BtlMutexHandle mutex;       // Synchronization
    BtlCondHandle cond;         // Condition for waiting
    BtlThreadHandles* threadHandles; // Platform thread functions
} BtlMessageQueue;

// ============================================================================
// Actor Object
//
// An actor is an isolated execution context that processes messages
// sequentially on its own thread.
// ============================================================================
typedef struct ObjActor {
    BtlObj obj;
    struct VM* vm;              // Actor's private VM
    BtlValue instance;             // The instance this actor wraps
    struct ObjClass* klass;     // Class of the instance
    BtlMessageQueue inbox;      // Incoming message queue
    BtlThreadHandle thread;     // Actor's thread
    BtlThreadHandles* threadHandles; // Platform thread functions
    volatile bool alive;        // Is actor running?
    volatile bool shouldStop;   // Stop requested?
} ObjActor;

// ============================================================================
// Native Function Types
// ============================================================================

// Simple native function (no receiver, but has VM access for object creation/error reporting)
typedef BtlValue (*BtlNativeFn)(VM* vm, int argCount, BtlValue* args);

// Native method (has receiver)
typedef BtlValue (*BtlNativeMethodFn)(VM* vm, BtlValue receiver, int argCount, BtlValue* args);

// ============================================================================
// Native Method Object
// ============================================================================
typedef struct {
    BtlObj obj;
    BtlNativeMethodFn function;  // The C function
    ObjString* name;             // Method name
    int arity;                   // Expected argument count
} ObjNativeMethod;

// ============================================================================
// Native Class Object
//
// Wraps a native class with methods defined in C.
// ============================================================================
typedef struct {
    BtlObj obj;
    ObjString* name;         // Class name
    BtlTable methods;           // Method table (name -> ObjNativeMethod)
} ObjNativeClass;

// ============================================================================
// Native Module Object
//
// Wraps a native module (like "system", "math", etc).
// ============================================================================
typedef struct {
    BtlObj obj;
    ObjString* name;         // Module name
    BtlTable globals;           // Global table (name -> value)
} ObjNativeModule;

// ============================================================================
// Type Check Macros
//
// Safe type checking for values.
// ============================================================================
#define OBJ_TYPE(v)            (AS_OBJ(v)->type)
#define IS_BOUND_METHOD(v)     btl_is_obj_type(v, BTL_OBJ_BOUND_METHOD)
#define IS_CLASS(v)            btl_is_obj_type(v, BTL_OBJ_CLASS)
#define IS_CLOSURE(v)          btl_is_obj_type(v, BTL_OBJ_CLOSURE)
#define IS_FUNCTION(v)         btl_is_obj_type(v, BTL_OBJ_FUNCTION)
#define IS_INSTANCE(v)         btl_is_obj_type(v, BTL_OBJ_INSTANCE)
#define IS_LIST(v)             btl_is_obj_type(v, BTL_OBJ_LIST)
#define IS_MODULE(v)           btl_is_obj_type(v, BTL_OBJ_MODULE)
#define IS_NATIVE(v)           btl_is_obj_type(v, BTL_OBJ_NATIVE)
#define IS_STRING(v)           btl_is_obj_type(v, BTL_OBJ_STRING)
#define IS_UPVALUE_BOX(v)      btl_is_obj_type(v, BTL_OBJ_UPVALUE)
#define IS_TABLE(v)            btl_is_obj_type(v, BTL_OBJ_TABLE)
#define IS_NATIVE_METHOD(v)    btl_is_obj_type(v, BTL_OBJ_NATIVE_METHOD)
#define IS_NATIVE_CLASS(v)     btl_is_obj_type(v, BTL_OBJ_NATIVE_CLASS)
#define IS_NATIVE_MODULE(v)    btl_is_obj_type(v, BTL_OBJ_NATIVE_MODULE)
#define IS_FUTURE(value)       btl_is_obj_type(value, BTL_OBJ_FUTURE)
#define IS_ACTOR(value)        btl_is_obj_type(value, BTL_OBJ_ACTOR)
#define IS_ENTITY(value)       btl_is_obj_type(value, BTL_OBJ_ENTITY)
#define IS_COROUTINE(value)    btl_is_obj_type(value, BTL_OBJ_COROUTINE)

// ============================================================================
// Type Cast Macros
//
// Unsafe casts - caller must verify type first.
// ============================================================================
#define AS_BOUND_METHOD(v)     ((ObjBoundMethod*)AS_OBJ(v))
#define AS_CLASS(v)            ((ObjClass*)AS_OBJ(v))
#define AS_CLOSURE(v)          ((ObjClosure*)AS_OBJ(v))
#define AS_FUNCTION(v)         ((ObjFunction*)AS_OBJ(v))
#define AS_INSTANCE(v)         ((ObjInstance*)AS_OBJ(v))
#define AS_LIST(v)             ((ObjList*)AS_OBJ(v))
#define AS_MODULE(v)           ((ObjModule*)AS_OBJ(v))
#define AS_NATIVE(v)           (((ObjNative*)AS_OBJ(v))->function)
#define AS_STRING(v)           ((ObjString*)AS_OBJ(v))
#define AS_CSTRING(v)          (((ObjString*)AS_OBJ(v))->chars)
#define AS_TABLE(v)            ((ObjTable*)AS_OBJ(v))
#define AS_NATIVE_METHOD(v)    ((ObjNativeMethod*)AS_OBJ(v))
#define AS_NATIVE_CLASS(v)     ((ObjNativeClass*)AS_OBJ(v))
#define AS_NATIVE_MODULE(v)    ((ObjNativeModule*)AS_OBJ(v))
#define AS_FUTURE(value)       ((ObjFuture*)AS_OBJ(value))
#define AS_ACTOR(value)        ((ObjActor*)AS_OBJ(value))
#define AS_ENTITY(value)       ((ObjEntity*)AS_OBJ(value))
#define AS_COROUTINE(value)    ((ObjCoroutine*)AS_OBJ(value))

// ============================================================================
// Upvalue Box
//
// Holds a closed-over value after the original stack slot goes out of scope.
// ============================================================================
struct ObjUpvalue {
    BtlObj obj;
    BtlValue closed;    // The captured value
};

// ============================================================================
// Runtime Upvalue
//
// Represents an upvalue at runtime. Can be:
// - Open: points to a stack slot
// - Closed mutable: points to an ObjUpvalue box
// - Closed immutable: contains the value directly
// ============================================================================
struct BtlRuntimeUpvalue {
    bool isOpen;             // True if pointing to stack
    bool isMutable;          // True if variable can be modified
    union {
        BtlValue* stack;        // Open: pointer to stack slot
        ObjUpvalue* box;     // Closed mutable: box containing value
        BtlValue immValue;      // Closed immutable: the value itself
    } loc;
    struct BtlRuntimeUpvalue* next;  // For open upvalue chain
};

// ============================================================================
// Closure Object
//
// A function combined with its captured environment (upvalues).
// Also contains inline caches for property/method access.
// ============================================================================
struct ObjClosure {
    BtlObj obj;
    struct ObjFunction* function;   // The underlying function
    int upvalueCount;               // Number of captured upvalues
    BtlFieldIC* fieldICs;              // Field access inline caches
    BtlMethodIC* methodICs;            // Method call inline caches
    BtlRuntimeUpvalue upvalues[];      // Flexible array of upvalues
};

// ============================================================================
// String Object
//
// Immutable, interned string. All strings with the same content share
// the same ObjString instance.
// ============================================================================
struct ObjString {
    BtlObj obj;
    int length;          // String length (excluding null terminator)
    char* chars;         // Null-terminated character data
    uint32_t hash;       // Pre-computed hash for fast table lookup
};

// ============================================================================
// Saved Class Info
//
// Compile-time metadata saved for each class so child classes can inherit
// method and field indices from their parents.
// ============================================================================
typedef struct {
    BtlTable methodIndices;     // Method signature -> vtable index
    BtlTable fieldIndices;      // Field name -> field index
    int fieldCount;             // Number of fields
} BtlSavedClassInfo;

// ============================================================================
// Module Object
//
// A compilation unit containing global variables and class metadata.
// ============================================================================
struct ObjModule {
    BtlObj obj;
    ObjString* name;            // Module name/path
    BtlTable globalNames;          // Maps global names to indices
    BtlValueArray globalValues;    // Global variable values
    BtlTable classInfo;            // Class metadata for field indices
};

// ============================================================================
// Function Object
//
// Compiled bytecode for a function. Closures wrap functions.
// ============================================================================
typedef struct ObjFunction {
    BtlObj obj;
    int arity;               // Expected argument count
    int upvalueCount;        // Number of upvalues captured
    BtlChunk chunk;             // Compiled bytecode
    ObjString* name;         // Function name (NULL for scripts)
    ObjModule* module;       // Containing module
    int fieldICCount;        // Number of field inline cache slots
    int methodICCount;       // Number of method inline cache slots
    void* compiledHandler;   // Transpiler: direct C function pointer
    int transpilerId;        // Transpiler: function ID assigned during collection (-1 = unassigned)
} ObjFunction;

// ============================================================================
// Native Function Object
//
// Wraps a simple native C function.
// ============================================================================
typedef struct ObjNative {
    BtlObj obj;
    BtlNativeFn function;    // The C function
} ObjNative;

// ============================================================================
// Method Entry
//
// Entry in a class's method table.
// ============================================================================
typedef struct {
    ObjClosure* closure;     // The method closure
    uint8_t arity;           // Expected argument count
    ObjString* name;         // Method name
} BtlMethodEntry;

// ============================================================================
// Native Field Accessor Types (for engine-backed components)
//
// These allow ObjClass to have fields whose get/set operations are intercepted
// by native C functions instead of reading/writing the ObjInstance fields array.
// ============================================================================
typedef BtlValue (*BtlFieldGetterFn)(VM* vm, void* backingData, int fieldIndex);
typedef void     (*BtlFieldSetterFn)(VM* vm, void* backingData, int fieldIndex, BtlValue value);

// Native constructor callback. Replaces default btl_instance_new() + init() flow
typedef BtlValue (*BtlNativeConstructorFn)(VM* vm, int argCount, BtlValue* args);

// Native data destructor. Called when instance is GC'd to free nativeData
typedef void (*BtlNativeDataFreeFn)(void* data);

// ============================================================================
// Class Object
//
// Represents a class with methods and field layout.
// ============================================================================
typedef struct ObjClass {
    BtlObj obj;
    ObjString* name;             // Class name
    BtlMethodEntry* methods;     // Method array (vtable)
    int methodCount;             // Number of methods
    int methodCapacity;          // Allocated method slots
    BtlTable methodIndices;         // Maps method names to vtable indices
    BtlTable fieldIndices;          // Maps field names to field indices
    int fieldCount;              // Number of instance fields
    int initCache[9];            // Cached init method indices for arities 0-8 (-1 = not cached, -2 = no init)

    // Native field accessors (NULL = normal BTL fields)
    BtlFieldGetterFn* nativeGetters;   // Array[fieldCount], NULL entries = use fields[]
    BtlFieldSetterFn* nativeSetters;   // Array[fieldCount], NULL entries = use fields[]

    // Native constructor (NULL = normal instantiation)
    BtlNativeConstructorFn nativeConstructor;

    // Native data destructor (NULL = no-op). Called when instance is GC'd to free nativeData.
    BtlNativeDataFreeFn nativeDataFree;

    // Native C methods on ObjClass instances (name -> ObjNativeMethod*)
    // Used for component classes (e.g. Tilemap) that need methods without
    // converting to ObjNativeClass (which lacks nativeGetters/nativeSetters).
    BtlTable nativeMethods;
} ObjClass;

// ============================================================================
// Instance Object
//
// An instance of a class with field storage.
// ============================================================================
typedef struct ObjInstance {
    BtlObj obj;
    ObjClass* klass;         // The instance's class
    BtlValue* fields;           // Field values array
    void* nativeData;        // Optional native backing data (NULL for normal instances)
} ObjInstance;

// ============================================================================
// Table Object (Dictionary)
//
// User-level hash table.
// ============================================================================
typedef struct ObjTable {
    BtlObj obj;
    BtlTable table;             // The underlying hash table
} ObjTable;

// ============================================================================
// List Object
//
// Dynamic array.
// ============================================================================
typedef struct ObjList {
    BtlObj obj;
    BtlValueArray items;        // The list contents
} ObjList;

// ============================================================================
// Bound Method Object
//
// A method bound to a specific receiver.
// ============================================================================
typedef struct ObjBoundMethod {
    BtlObj obj;
    BtlValue receiver;          // The receiver (this)
    ObjClosure* method;      // The method closure
} ObjBoundMethod;

// ============================================================================
// Entity Object
//
// Wraps a game engine entity ID with a script-component table.
// Methods dispatched via ObjNativeClass (same pattern as String/List).
// ============================================================================
typedef struct ObjEntity {
    BtlObj obj;
    uint64_t id;                    // Engine entity ID
    void* world;                    // Pointer to engine ECS world (opaque)
    void* engine;                   // Pointer to engine context (opaque)
    BtlTable scriptComponents;      // ObjClass* -> ObjInstance* for user BTL components
} ObjEntity;

// ============================================================================
// Coroutine Object
//
// Stackful coroutine with its own value stack and call frame stack.
// Suspended state is preserved across yields; the scheduler swaps the VM's
// stack pointers to the coroutine's storage during execution.
// ============================================================================

typedef enum {
    COROUTINE_SUSPENDED,
    COROUTINE_RUNNING,
    COROUTINE_DEAD
} CoroutineState;

typedef struct ObjCoroutine {
    BtlObj obj;
    BtlValue* stack;            // Own value stack
    int stackCapacity;          // Allocated capacity
    BtlValue* stackTop;         // Saved stack top (when suspended)
    BtlCallFrame* frames;      // Own call frame stack
    int frameCapacity;          // Allocated frame capacity
    int frameCount;             // Saved frame count (when suspended)
    CoroutineState state;       // Current state
    float waitTimer;            // Seconds remaining before resume (0 = next frame)
    int waitFrames;             // Frames remaining before resume (0 = ready)
    ObjClosure* body;           // The closure to execute
} ObjCoroutine;

// ============================================================================
// Object Creation Functions
// ============================================================================

// Create a new closure wrapping a function
ObjClosure* btl_closure_new(VM* vm, ObjFunction* function);

// Create a new upvalue box with initial value
ObjUpvalue* btl_upvalue_box_new(VM* vm, BtlValue value);

// Create a new function in the given module
ObjFunction* btl_function_new(VM* vm, ObjModule* module);

// Create a new class with the given name
ObjClass* btl_class_new(VM* vm, ObjString* name);

// Create a new instance of a class
ObjInstance* btl_instance_new(VM* vm, ObjClass* klass);

// Create a bound method
ObjBoundMethod* btl_bound_method_new(VM* vm, BtlValue receiver, ObjClosure* method);

// Create an empty list
ObjList* btl_list_new(VM* vm);

// Create a new module with the given name
ObjModule* btl_module_new(VM* vm, ObjString* name);

// Create a native function wrapper
ObjNative* btl_native_new(VM* vm, BtlNativeFn function);

// Create a string by taking ownership of chars (will free on GC)
ObjString* btl_string_take(VM* vm, char* chars, int length);

// Create a string by copying chars
ObjString* btl_string_copy(VM* vm, const char* chars, int length);

// Create an empty table
ObjTable* btl_table_obj_new(VM* vm);

// Create an entity object (for game engine integration)
ObjEntity* btl_entity_new(VM* vm, uint64_t id, void* world, void* engine);

// Create a coroutine wrapping a closure (allocates own stack + frames)
ObjCoroutine* btl_coroutine_new(VM* vm, ObjClosure* body);

// ============================================================================
// Native Field Accessor API (for engine-backed component classes)
// ============================================================================

// Initialize native field accessor arrays on a class
void btl_class_init_native_fields(VM* vm, ObjClass* klass);

// Add a native-backed field with getter/setter. Returns the field index.
int btl_class_add_native_field(VM* vm, ObjClass* klass, const char* name,
                                BtlFieldGetterFn getter, BtlFieldSetterFn setter);

// Add a native C method to an ObjClass (for component classes like Tilemap).
// Unlike btl_native_class_add_method (which works on ObjNativeClass*), this
// stores methods in ObjClass.nativeMethods and is safe for classes with native fields.
void btl_class_add_native_method(VM* vm, ObjClass* klass, const char* name,
                                  BtlNativeMethodFn fn, int arity);

// ============================================================================
// Native Class/Method/Module Functions
// ============================================================================

// Create a native method
ObjNativeMethod* btl_native_method_new(VM* vm, BtlNativeMethodFn fn,
                                        const char* name, int arity);

// Create a native class
ObjNativeClass* btl_native_class_new(VM* vm, const char* name);

// Create a native module
ObjNativeModule* btl_native_module_new(VM* vm, const char* name);

// Add a method to a native class
void btl_native_class_add_method(VM* vm, ObjNativeClass* klass,
                                  const char* name, BtlNativeMethodFn fn, int arity);

// Add a value to a native module
void btl_native_module_add_value(VM* vm, ObjNativeModule* module,
                                  const char* name, BtlValue value);

// Add a function to a native module
void btl_native_module_add_function(VM* vm, ObjNativeModule* module,
                                     const char* name, BtlNativeFn fn, int arity);

// ============================================================================
// Future Functions
// ============================================================================

// Create a new pending future
ObjFuture* btl_future_new(struct VM* vm);

// Resolve a future with a value
void btl_future_resolve(ObjFuture* future, BtlValue result);

// Reject a future with an error
void btl_future_reject(ObjFuture* future, BtlValue error);

// Wait for and get the future's result
BtlValue btl_future_get(ObjFuture* future);

// Get the current state of a future
BtlFutureState btl_future_get_state(ObjFuture* future);

// Free future resources
void btl_future_free(ObjFuture* future);

// ============================================================================
// Message Queue Functions
// ============================================================================

// Initialize a message queue
void btl_message_queue_init(BtlMessageQueue* queue, BtlThreadHandles* th);

// Free a message queue
void btl_message_queue_free(BtlMessageQueue* queue);

// Push a message to the queue
void btl_message_queue_push(BtlMessageQueue* queue, BtlActorMessage message);

// Pop a message from the queue (with timeout)
bool btl_message_queue_pop(BtlMessageQueue* queue, BtlActorMessage* out,
                           int timeoutMs);

// ============================================================================
// Actor Functions
// ============================================================================

// Create a new actor wrapping a class instance
ObjActor* btl_actor_new(struct VM* parentVM, ObjClass* klass,
                        BtlValue* args, int argCount);

// Send a message to an actor
void btl_actor_send(ObjActor* actor, ObjString* method,
                    BtlValue* args, int argCount, ObjFuture* future);

// Stop an actor
void btl_actor_stop(ObjActor* actor);

// Deep copy a value for passing between actor VMs
BtlValue btl_deep_copy_value(struct VM* destVM, struct VM* srcVM, BtlValue value);

// ============================================================================
// Utility Functions
// ============================================================================

// Print an object value (for debugging)
void btl_object_print(BtlValue value);

// Check if a value is an object of a specific type
static inline bool btl_is_obj_type(BtlValue v, BtlObjType t) {
    return IS_OBJ(v) && AS_OBJ(v)->type == t;
}

#endif // btl_object_h
