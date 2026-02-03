#ifndef btl_vm_h
#define btl_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
    RuntimeUpvalue* openUpvalues;
} CallFrame;

struct VM {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;

    Table strings;
    ObjString* initString;
    Table modules;
    ObjModule* rootModule;

    // Native modules and classes
    Table nativeModules;
    ObjNativeClass* stringClass;
    ObjNativeClass* numberClass;
    ObjNativeClass* listClass;
    ObjNativeClass* tableClass;

    size_t bytesAllocated;
    size_t nextGC;
    Obj* objects;
    int grayCount;
    int grayCapacity;
    Obj** grayStack;

    struct Compiler* compiler;
};

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM* vm);
void freeVM(VM* vm);
InterpretResult interpret(VM* vm, ObjModule* module, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);
void runtimeError(VM* vm, const char* format, ...);

#endif