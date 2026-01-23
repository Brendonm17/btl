#ifndef btl_vm_h
#define btl_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

struct Obj;
struct ObjClosure;
struct ObjString;
struct ObjModule;
typedef struct ObjUpvalue ObjUpvalue;

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    struct ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct VM {
    CallFrame frames[FRAMES_MAX];
    int frameCount;
    Value stack[STACK_MAX];
    Value* stackTop;

    Table strings;
    Table modules;
    struct ObjModule* rootModule;

    struct ObjString* initString;
    ObjUpvalue** openUpvalues;
    int openUpvalueCount;
    int openUpvalueCapacity;

    size_t bytesAllocated;
    size_t nextGC;
    struct Obj* objects;

    int grayCount;
    int grayCapacity;
    struct Obj** grayStack;

    void* compiler;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM* vm);
void freeVM(VM* vm);
InterpretResult interpret(VM* vm, struct ObjModule* module, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);

#endif