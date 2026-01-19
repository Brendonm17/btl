#ifndef btl_compiler_h
#define btl_compiler_h

#include "object.h"
#include "vm.h"
#include "scanner.h"

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
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
    int scopeDepth;

    Loop* currentLoop;
    ObjModule* module;

    struct VM* vm;
    int lastInstruction;
    int previousInstruction;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

ObjFunction* compile(struct VM* vm, ObjModule* module, const char* source);
void markCompilerRoots(struct VM* vm);

#endif