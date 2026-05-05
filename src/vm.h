// Stack-based bytecode VM. Manages call frames, value stack, string interning,
// modules, native classes for built-in types, and a generational GC
// (nursery + old gen). Supports concurrent execution via actors and futures.

#ifndef btl_vm_h
#define btl_vm_h

#include "object.h"
#include "table.h"
#include "value.h"
#include "memory.h"
#include "threadpool.h"

#define BTL_FRAMES_INITIAL 64
#define BTL_STACK_INITIAL (BTL_FRAMES_INITIAL * BTL_UINT8_COUNT)
#define BTL_STACK_GROW_FACTOR 2

typedef struct BTLRuntime BTLRuntime;

// One frame per active function call. Open upvalues are linked per-frame.
typedef struct BtlCallFrame {
    ObjClosure* closure;
    uint8_t* ip;
    BtlValue* slots;
    BtlRuntimeUpvalue* openUpvalues;
} BtlCallFrame;

struct VM {
    BtlCallFrame* frames;
    int frameCount;
    int frameCapacity;

    BtlValue* stack;
    BtlValue* stackTop;
    int stackCapacity;

    BtlTable strings;
    ObjString* initString;
    BtlTable modules;
    ObjModule* rootModule;

    BtlTable nativeModules;
    BtlTable nativeClassInfo;       // Engine-registered class field info, used by the compiler for inheritance
    ObjNativeClass* stringClass;
    ObjNativeClass* numberClass;
    ObjNativeClass* intClass;
    ObjNativeClass* listClass;
    ObjNativeClass* tableClass;
    ObjNativeClass* entityClass;

    // Old-gen GC.
    size_t bytesAllocated;
    size_t nextGC;
    BtlObj* objects;
    int grayCount;
    int grayCapacity;
    BtlObj** grayStack;

    // Young gen.
    BtlNursery nursery;
    BtlRememberedSet rememberedSet;
    size_t nurseryAllocated;
    size_t minorGCCount;
    size_t majorGCCount;
    size_t promotedBytes;
    bool inMinorGC;
    int gcInhibit;

    struct BtlCompiler* compiler;
    BtlValue lastReturnValue;
    BTLRuntime* runtime;
    int runFloor;                   // Frame count floor; btl_run stops when reached
    bool coroutineYield;            // Set by coroutine.yield()/wait() to exit btl_run

    // Engine-registered values that must survive GC.
    #define BTL_MAX_NATIVE_ROOTS 64
    BtlValue nativeRoots[BTL_MAX_NATIVE_ROOTS];
    int nativeRootCount;

    // Engine-side mark callback. Called during GC marking; the callback should
    // call btl_gc_mark_value() for any BtlValues stored in C structs that
    // must survive collection.
    void (*nativeMarkFn)(struct VM* vm, void* userData);
    void* nativeMarkUserData;
};

typedef enum {
    BTL_INTERPRET_OK,
    BTL_INTERPRET_COMPILE_ERROR,
    BTL_INTERPRET_RUNTIME_ERROR,
    BTL_INTERPRET_YIELD             // Coroutine yield. Unwinds to scheduler
} BtlInterpretResult;

void btl_vm_init(VM* vm);

// mainVM=true for the primary VM, false for worker VMs.
void btl_vm_free(VM* vm, bool mainVM);

BtlInterpretResult btl_interpret(VM* vm, ObjModule* module, const char* source);

// Execute bytecode starting from the current frame.
BtlInterpretResult btl_run(VM* vm);

// Call a value (function, class, bound method, native).
bool btl_call_value(VM* vm, BtlValue callee, int argCount);

void btl_push(VM* vm, BtlValue value);
BtlValue btl_pop(VM* vm);

void btl_runtime_error(VM* vm, const char* format, ...);

bool btl_ensure_stack_capacity(VM* vm, int needed);
bool btl_ensure_frame_capacity(VM* vm);

// Returns root index, or -1 if full.
int btl_gc_add_root(VM* vm, BtlValue value);
void btl_gc_remove_root(VM* vm, int index);
void btl_gc_clear_roots(VM* vm);

// Lets the compiler inherit a native class's field layout when a user script
// subclasses it (e.g. class Foo < Component).
void btl_register_native_class_info(VM* vm, const char* class_name,
                                    const char** field_names, int field_count);

#endif
