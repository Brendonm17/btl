#ifndef btl_compiler_h
#define btl_compiler_h

#include "object.h"
#include "vm.h"
#include "scanner.h"

// We define the Compiler struct here so memory.c can see it for GC marking
typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    int type; // Internal FunctionType

    // We only need the locals/upvalues for the compiler's own logic
    // but memory.c only needs to see the linked list of compilers.
    struct Local {
        Token name;
        int depth;
        bool isCaptured;
    } locals[UINT8_COUNT];
    int localCount;

    struct Upvalue {
        uint8_t index;
        bool isLocal;
    } upvalues[UINT8_COUNT];

    int scopeDepth;
    VM* vm;
} Compiler;

ObjFunction* compile(VM* vm, const char* source);
void markCompilerRoots(VM* vm);

#endif