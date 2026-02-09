// ============================================================================
// compiler.h - BTL Compiler
//
// The compiler transforms BTL source code into bytecode. It uses a single-pass
// recursive descent parser with Pratt parsing for expressions. The compiler
// generates bytecode for the VM's stack-based architecture.
//
// Key components:
// - Parser: Converts tokens into an AST implicitly via recursive calls
// - Compiler: Manages local variables, upvalues, and bytecode emission
// - ClassCompiler: Tracks class-specific state during class compilation
// ============================================================================

#ifndef btl_compiler_h
#define btl_compiler_h

#include "object.h"
#include "vm.h"
#include "scanner.h"

// ----------------------------------------------------------------------------
// BtlUpvaluePatch - Tracks locations needing mutability patching
//
// When a closure captures a local, we don't know if the local will be
// modified later. This structure records where the mutability byte was
// emitted so it can be patched if the local is later modified.
// ----------------------------------------------------------------------------
typedef struct {
    int localIndex;     // Index of the captured local variable
    int codeOffset;     // Offset in bytecode where mutability byte is
} BtlUpvaluePatch;

// ----------------------------------------------------------------------------
// BtlLocal - Represents a local variable in the current scope
//
// Locals are stored in the compiler's locals array, indexed by slot number.
// The depth field tracks nesting level for proper scoping.
// ----------------------------------------------------------------------------
typedef struct {
    BtlToken name;      // Token containing the variable name
    int depth;          // Scope depth (-1 if not yet initialized)
    bool isCaptured;    // True if captured by a closure
    bool isModified;    // True if ever assigned after initialization
} BtlLocal;

// ----------------------------------------------------------------------------
// BtlUpvalue - Describes a captured variable for closures
//
// Upvalues form a linked chain from inner to outer scopes, allowing
// closures to access variables from enclosing functions.
// ----------------------------------------------------------------------------
typedef struct {
    uint8_t index;      // Index into enclosing locals or upvalues array
    bool isLocal;       // True if capturing a local, false if an upvalue
    bool isMutable;     // True if the captured variable may be modified
} BtlUpvalue;

// ----------------------------------------------------------------------------
// BtlLoop - Tracks loop state for break/continue handling
//
// Loops are tracked in a linked list (enclosing chain) to support nested
// loops. Break jumps are recorded for patching when the loop ends.
// ----------------------------------------------------------------------------
typedef struct BtlLoop {
    struct BtlLoop* enclosing;  // Enclosing loop (for nested loops)
    int start;                   // Bytecode offset of loop start
    int bodyJump;                // Jump offset for loop body
    int scopeDepth;              // Scope depth when loop started
    int breakJumps[255];         // Offsets of break jump instructions
    int breakCount;              // Number of break statements
} BtlLoop;

// ----------------------------------------------------------------------------
// BtlSwitchContext - Tracks switch statement state
//
// Switch statements need to track case jumps, break jumps, and whether
// the switch is being used as an expression (break with value).
// ----------------------------------------------------------------------------
typedef struct BtlSwitchContext {
    struct BtlSwitchContext* enclosing;  // Enclosing switch (for nested)
    int* caseJumps;                       // Array of case jump offsets
    int caseJumpCount;                    // Number of case jumps
    int caseJumpCapacity;                 // Capacity of case jumps array
    int* breakJumps;                      // Array of break jump offsets
    int breakCount;                       // Number of break statements
    int breakCapacity;                    // Capacity of break jumps array
    int scopeDepth;                       // Scope depth when switch started
    bool hasDefault;                      // True if default case exists
    bool isExpression;                    // True if switch returns a value
} BtlSwitchContext;

// ----------------------------------------------------------------------------
// BtlFunctionType - Distinguishes different function contexts
//
// The compiler needs to know the function type to handle 'this', 'return',
// and initializer-specific behavior correctly.
// ----------------------------------------------------------------------------
typedef enum {
    BTL_TYPE_FUNCTION,      // Regular function
    BTL_TYPE_INITIALIZER,   // Class init() method
    BTL_TYPE_METHOD,        // Class method (non-init)
    BTL_TYPE_SCRIPT         // Top-level script
} BtlFunctionType;

// ----------------------------------------------------------------------------
// BtlCompiler - Main compiler state
//
// Each function being compiled has its own compiler instance. Compilers
// are linked via 'enclosing' to support nested function compilation.
// ----------------------------------------------------------------------------
typedef struct BtlCompiler {
    struct BtlCompiler* enclosing;  // Enclosing compiler (for nested fns)
    ObjFunction* function;          // Function being compiled
    BtlFunctionType type;           // Type of function

    BtlLocal locals[BTL_UINT8_COUNT];   // Local variables in this scope
    int localCount;                      // Number of locals
    BtlUpvalue upvalues[BTL_UINT8_COUNT]; // Upvalues for this closure

    // Patch tracking for upvalue mutability
    BtlUpvaluePatch patches[BTL_UINT8_COUNT];
    int patchCount;

    int scopeDepth;             // Current nesting depth (0 = global)
    int lastInstruction;        // Offset of last emitted instruction
    int previousInstruction;    // Offset of second-to-last instruction
    struct VM* vm;              // VM for allocation and error reporting
    ObjModule* module;          // Module being compiled into
    BtlTable constants;         // Constant deduplication table
    struct BtlLoop* currentLoop;          // Current loop context
    BtlSwitchContext* currentSwitch;      // Current switch context

    // Inline cache slot counters
    int fieldICCount;           // Number of field IC slots needed
    int methodICCount;          // Number of method IC slots needed

    // Prevents fusion from removing comparison ops when and/or
    // short-circuit jumps target the current position
    bool inhibitFusion;
} BtlCompiler;

// ----------------------------------------------------------------------------
// BtlFieldInfo - Tracks class field information during compilation
//
// Used to store field initializer expressions for injection into init().
// ----------------------------------------------------------------------------
typedef struct {
    ObjString* fieldName;   // Field name string
    int fieldIndex;         // Index in instance fields array
    const char* initSource; // Source code of initializer expression
    int initLength;         // Length of initializer source
    bool hasInit;           // True if field has an initializer
} BtlFieldInfo;

// ----------------------------------------------------------------------------
// BtlClassCompiler - Tracks class compilation state
//
// Manages class-specific information like field table, method indices,
// and superclass state.
// ----------------------------------------------------------------------------
typedef struct BtlClassCompiler {
    struct BtlClassCompiler* enclosing;  // Enclosing class (for nested)
    bool hasSuperclass;                   // True if class has superclass
    BtlTable fields;                      // Field name -> index mapping
    int fieldCount;                       // Number of fields
    BtlTable methodIndices;               // Method signature -> index
    int nextMethodIndex;                  // Next available method index

    BtlFieldInfo* fieldInfos;             // Array of field info
    int fieldInfoCapacity;                // Capacity of field info array
} BtlClassCompiler;

// ----------------------------------------------------------------------------
// Compiler API
// ----------------------------------------------------------------------------

// Compile source code into a function object
// Returns NULL on compile error
ObjFunction* btl_compile(struct VM* vm, ObjModule* module, const char* source);

// Mark compiler roots for garbage collection
void btl_compiler_mark_roots(struct VM* vm);

#endif // btl_compiler_h
