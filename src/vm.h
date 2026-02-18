// ============================================================================
// vm.h - BTL Virtual Machine
//
// The VM executes BTL bytecode using a stack-based architecture. It manages:
// - Call frames for function execution
// - Value stack for operands and results
// - String interning for efficient string comparison
// - Module system for code organization
// - Native classes for built-in types (string, number, list, table)
// - Garbage collection (generational: nursery + old generation)
//
// The VM supports concurrent execution via actors and futures, with thread
// pool integration for async operations.
// ============================================================================

#ifndef btl_vm_h
#define btl_vm_h

#include "object.h"
#include "table.h"
#include "value.h"
#include "memory.h"
#include "threadpool.h"

// ----------------------------------------------------------------------------
// Stack Configuration
//
// The VM uses dynamically growing stacks for both call frames and values.
// Initial sizes are conservative; stacks grow as needed.
// ----------------------------------------------------------------------------

// Initial capacities - will grow as needed
#define BTL_FRAMES_INITIAL 64
#define BTL_STACK_INITIAL (BTL_FRAMES_INITIAL * BTL_UINT8_COUNT)

// Growth factor for stack/frames expansion
#define BTL_STACK_GROW_FACTOR 2

// ----------------------------------------------------------------------------
// Forward Declarations
// ----------------------------------------------------------------------------

typedef struct BTLRuntime BTLRuntime;

// ----------------------------------------------------------------------------
// BtlCallFrame - Represents a function call on the call stack
//
// Each call frame tracks the executing closure, instruction pointer,
// and base slot for local variables. Open upvalues are linked per-frame.
// ----------------------------------------------------------------------------
typedef struct {
    ObjClosure* closure;        // Function being executed
    uint8_t* ip;                // Instruction pointer (next bytecode)
    BtlValue* slots;            // Base of local variable slots
    BtlRuntimeUpvalue* openUpvalues; // Linked list of open upvalues
} BtlCallFrame;

// ----------------------------------------------------------------------------
// VM - The BTL Virtual Machine
//
// The VM is the central execution engine. It contains all runtime state
// including the value stack, call frames, interned strings, and GC state.
// ----------------------------------------------------------------------------
struct VM {
    // Call frame stack (dynamically allocated)
    BtlCallFrame* frames;       // Array of call frames
    int frameCount;             // Current number of active frames
    int frameCapacity;          // Current capacity of frames array

    // Value stack (dynamically allocated)
    BtlValue* stack;            // Array of values
    BtlValue* stackTop;         // Pointer to next free slot
    int stackCapacity;          // Current capacity of stack array

    // String interning and modules
    BtlTable strings;           // Interned strings (weak references)
    ObjString* initString;      // Cached "init" string
    BtlTable modules;           // Loaded modules
    ObjModule* rootModule;      // Main module

    // Native modules and classes
    BtlTable nativeModules;     // Built-in module table
    BtlTable nativeClassInfo;   // Engine-registered class field info for compiler inheritance
    ObjNativeClass* stringClass; // Native string methods
    ObjNativeClass* numberClass; // Native number methods
    ObjNativeClass* intClass;    // Native int methods
    ObjNativeClass* listClass;   // Native list methods
    ObjNativeClass* tableClass;  // Native table methods
    ObjNativeClass* entityClass; // Native entity methods (game engine)

    // Old generation GC fields
    size_t bytesAllocated;      // Total bytes allocated in old gen
    size_t nextGC;              // Threshold for next major GC
    BtlObj* objects;            // Head of old generation object list
    int grayCount;              // Number of gray objects in stack
    int grayCapacity;           // Capacity of gray stack
    BtlObj** grayStack;         // Stack of gray objects for marking

    // Young generation (nursery) GC fields
    BtlNursery nursery;         // Young generation nursery
    BtlRememberedSet rememberedSet; // Old->young pointer tracking
    size_t nurseryAllocated;    // Bytes allocated in nursery
    size_t minorGCCount;        // Statistics: number of minor GCs
    size_t majorGCCount;        // Statistics: number of major GCs
    size_t promotedBytes;       // Statistics: bytes promoted to old gen
    bool inMinorGC;             // Flag to prevent recursive GC
    int gcInhibit;              // Counter to inhibit GC during critical sections

    // Compiler and runtime state
    struct BtlCompiler* compiler; // Current compiler (for GC roots)
    BtlValue lastReturnValue;     // Last returned value
    BTLRuntime* runtime;          // Parent runtime context
    int runFloor;                 // Frame count floor for run() - stops when reached

    // Native GC roots (engine-registered values protected from collection)
    #define BTL_MAX_NATIVE_ROOTS 64
    BtlValue nativeRoots[BTL_MAX_NATIVE_ROOTS];
    int nativeRootCount;
};

// ----------------------------------------------------------------------------
// BtlInterpretResult - Result of bytecode interpretation
// ----------------------------------------------------------------------------
typedef enum {
    BTL_INTERPRET_OK,            // Execution completed successfully
    BTL_INTERPRET_COMPILE_ERROR, // Compilation failed
    BTL_INTERPRET_RUNTIME_ERROR  // Runtime error occurred
} BtlInterpretResult;

// ----------------------------------------------------------------------------
// VM Lifecycle
// ----------------------------------------------------------------------------

// Initialize a VM instance
void btl_vm_init(VM* vm);

// Free all VM resources
// mainVM: true if this is the main VM (vs. worker VMs)
void btl_vm_free(VM* vm, bool mainVM);

// ----------------------------------------------------------------------------
// Execution
// ----------------------------------------------------------------------------

// Compile and execute source code in a module
BtlInterpretResult btl_interpret(VM* vm, ObjModule* module, const char* source);

// Execute bytecode starting from current frame
BtlInterpretResult btl_run(VM* vm);

// Call a value (function, class, bound method, native)
bool btl_call_value(VM* vm, BtlValue callee, int argCount);

// ----------------------------------------------------------------------------
// Stack Operations
// ----------------------------------------------------------------------------

// Push a value onto the stack
void btl_push(VM* vm, BtlValue value);

// Pop a value from the stack
BtlValue btl_pop(VM* vm);

// ----------------------------------------------------------------------------
// Error Handling
// ----------------------------------------------------------------------------

// Report a runtime error with formatted message
void btl_runtime_error(VM* vm, const char* format, ...);

// ----------------------------------------------------------------------------
// Stack Growth
// ----------------------------------------------------------------------------

// Ensure value stack has space for 'needed' more values
bool btl_ensure_stack_capacity(VM* vm, int needed);

// Ensure frame stack has space for one more frame
bool btl_ensure_frame_capacity(VM* vm);

// ----------------------------------------------------------------------------
// Native GC Roots
// ----------------------------------------------------------------------------

// Register a value as a GC root (prevents collection). Returns root index, or -1 if full.
int btl_gc_add_root(VM* vm, BtlValue value);

// Remove a GC root by index
void btl_gc_remove_root(VM* vm, int index);

// Clear all native GC roots
void btl_gc_clear_roots(VM* vm);

// ----------------------------------------------------------------------------
// Native Class Info (for compiler field inheritance)
// ----------------------------------------------------------------------------

// Register field layout for a native class so the compiler can inherit
// field indices when user scripts subclass it (e.g. class Foo < Component).
// field_names: array of field name strings, field_count: number of fields.
void btl_register_native_class_info(VM* vm, const char* class_name,
                                    const char** field_names, int field_count);

#endif // btl_vm_h
