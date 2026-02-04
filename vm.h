#ifndef btl_vm_h
#define btl_vm_h

#include "object.h"
#include "table.h"
#include "value.h"
#include "memory.h"
#include "threadpool.h"
#include <pthread.h>

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct BTLRuntime BTLRuntime;

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

    // ===== OLD GC FIELDS (keep these) =====
    size_t bytesAllocated;
    size_t nextGC;
    Obj* objects;           // Old generation linked list
    int grayCount;
    int grayCapacity;
    Obj** grayStack;

    // ===== YOUNG GC FIELDS =====
    Nursery nursery;                // Young generation nursery
    RememberedSet rememberedSet;    // Old->young pointer tracking
    size_t nurseryAllocated;        // Bytes allocated in nursery
    size_t minorGCCount;            // Stats: number of minor GCs
    size_t majorGCCount;            // Stats: number of major GCs
    size_t promotedBytes;           // Stats: bytes promoted to old gen
    bool inMinorGC;                 // Flag to prevent recursive GC
    int gcInhibit;                  // Counter to inhibit GC during critical sections

    struct Compiler* compiler;
    Value lastReturnValue;
    BTLRuntime* runtime;
};

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM* vm);
void freeVM(VM* vm, bool mainVM);
InterpretResult interpret(VM* vm, ObjModule* module, const char* source);
void push(VM* vm, Value value);
Value pop(VM* vm);
void runtimeError(VM* vm, const char* format, ...);

#endif