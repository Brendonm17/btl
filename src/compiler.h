#ifndef btl_compiler_h
#define btl_compiler_h

#include "object.h"
#include "vm.h"
#include "scanner.h"

// When a closure captures a local, mutability isn't known at capture time.
// We record the bytecode offset of the mutability byte so it can be patched
// later if the local is assigned to.
typedef struct {
    int localIndex;
    int codeOffset;
} BtlUpvaluePatch;

typedef struct {
    BtlToken name;
    int depth;          // -1 means declared but not initialized
    bool isCaptured;
    bool isModified;
} BtlLocal;

typedef struct {
    uint8_t index;
    bool isLocal;
    bool isMutable;
} BtlUpvalue;

typedef struct BtlLoop {
    struct BtlLoop* enclosing;
    int start;
    int bodyJump;
    int scopeDepth;
    int breakJumps[255];
    int breakCount;
} BtlLoop;

typedef struct BtlSwitchContext {
    struct BtlSwitchContext* enclosing;
    int* caseJumps;
    int caseJumpCount;
    int caseJumpCapacity;
    int* breakJumps;
    int breakCount;
    int breakCapacity;
    int scopeDepth;
    bool hasDefault;
    bool isExpression;
} BtlSwitchContext;

typedef enum {
    BTL_TYPE_FUNCTION,
    BTL_TYPE_INITIALIZER,
    BTL_TYPE_METHOD,
    BTL_TYPE_SCRIPT
} BtlFunctionType;

typedef struct BtlCompiler {
    struct BtlCompiler* enclosing;
    ObjFunction* function;
    BtlFunctionType type;

    BtlLocal locals[BTL_UINT8_COUNT];
    int localCount;
    BtlUpvalue upvalues[BTL_UINT8_COUNT];

    BtlUpvaluePatch patches[BTL_UINT8_COUNT];
    int patchCount;

    int scopeDepth;
    int lastInstruction;
    int previousInstruction;
    struct VM* vm;
    ObjModule* module;
    BtlTable constants;
    struct BtlLoop* currentLoop;
    BtlSwitchContext* currentSwitch;

    int fieldICCount;
    int methodICCount;

    // Prevents fusion from removing comparison ops when and/or short-circuit
    // jumps target the current position.
    bool inhibitFusion;
} BtlCompiler;

typedef struct {
    ObjString* fieldName;
    int fieldIndex;
    const char* initSource;
    int initLength;
    bool hasInit;
} BtlFieldInfo;

typedef struct BtlClassCompiler {
    struct BtlClassCompiler* enclosing;
    bool hasSuperclass;
    BtlTable fields;
    int fieldCount;
    BtlTable methodIndices;
    int nextMethodIndex;

    BtlFieldInfo* fieldInfos;
    int fieldInfoCapacity;
} BtlClassCompiler;

// Returns NULL on compile error.
ObjFunction* btl_compile(struct VM* vm, ObjModule* module, const char* source);

void btl_compiler_mark_roots(struct VM* vm);

#endif
