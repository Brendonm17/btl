#ifndef btl_vm_h
#define btl_vm_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

/* --- FORWARD DECLARATIONS --- */
/* This tells the compiler these structs exist without needing their full definitions yet. */
struct Obj;
struct ObjClosure;
struct ObjString;
struct ObjUpvalue;

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    struct ObjClosure* closure; // This no longer causes the "incomplete type" error
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct VM {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;
    Table globals;
    Table strings;

    struct ObjString* initString;
    struct ObjUpvalue* openUpvalues;

    size_t bytesAllocated;
    size_t nextGC;
    struct Obj* objects; // Forward declared

    int grayCount;
    int grayCapacity;
    struct Obj** grayStack;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM* vm);
void freeVM(VM* vm);
InterpretResult interpret(VM* vm, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);

#endif