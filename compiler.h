#ifndef btl_compiler_h
#define btl_compiler_h

#include "object.h"
#include "vm.h"
#include "scanner.h"

typedef struct {
    int localIndex;
    int codeOffset;
} UpvaluePatch;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
    bool isModified;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
    bool isMutable;
} Upvalue;

typedef struct Loop {
    struct Loop* enclosing;
    int start;
    int bodyJump;
    int scopeDepth;
    int breakJumps[255];
    int breakCount;
} Loop;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];

    // Patch tracking
    UpvaluePatch patches[UINT8_COUNT];
    int patchCount;

    int scopeDepth;
    int lastInstruction;
    int previousInstruction;
    struct VM* vm;
    ObjModule* module;
    Table constants;
    struct Loop* currentLoop;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

ObjFunction* compile(struct VM* vm, ObjModule* module, const char* source);
void markCompilerRoots(struct VM* vm);

#endif