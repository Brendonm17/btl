#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <pthread.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"
#include "value.h"
#include "threadpool.h"

#include "native_string.h"
#include "native_list.h"
#include "native_table.h"
#include "native_number.h"
#include "native_int.h"
#include "native_system.h"
#include "native_math.h"
#include "native_random.h"
#include "runtime.h"

BtlInterpretResult btl_run_vm(VM* vm);

// --- Macros ---
#define PATCH_OP_1(newOp) (frame->closure->function->chunk.code[ip - frame->closure->function->chunk.code - 1] = (newOp))
#define PATCH_OP_2(newOp) (frame->closure->function->chunk.code[ip - frame->closure->function->chunk.code - 2] = (newOp))

#define DO_GET_UV_SLOT(idx) \
    BtlRuntimeUpvalue* uv = &frame->closure->upvalues[idx]; \
    if (uv->isOpen) { \
        btl_push(vm, *uv->loc.stack); \
        PATCH_OP_1(BTL_OP_GET_UPVALUE_OPEN_##idx); \
    } else { \
        if (uv->isMutable) { \
            btl_push(vm, uv->loc.box->closed); \
            PATCH_OP_1(BTL_OP_GET_UPVALUE_CLOSED_##idx); \
        } else { \
            btl_push(vm, uv->loc.immValue); \
            PATCH_OP_1(BTL_OP_GET_UPVALUE_IMMUTABLE_##idx); \
        } \
    }

#define DO_GET_UV_SLOT_OPEN(idx) \
    BtlRuntimeUpvalue* uv = &frame->closure->upvalues[idx]; \
    if (uv->isOpen) { \
        btl_push(vm, *uv->loc.stack); \
    } else { \
        if (uv->isMutable) { \
            btl_push(vm, uv->loc.box->closed); \
            PATCH_OP_1(BTL_OP_GET_UPVALUE_CLOSED_##idx); \
        } else { \
            btl_push(vm, uv->loc.immValue); \
            PATCH_OP_1(BTL_OP_GET_UPVALUE_IMMUTABLE_##idx); \
        } \
    }

#define DO_SET_UV_SLOT(idx) \
    BtlRuntimeUpvalue* uv = &frame->closure->upvalues[idx]; \
    if (uv->isOpen) { \
        *uv->loc.stack = peek(vm, 0); \
        PATCH_OP_1(BTL_OP_SET_UPVALUE_OPEN_##idx); \
    } else { \
        uv->loc.box->closed = peek(vm, 0); \
        PATCH_OP_1(BTL_OP_SET_UPVALUE_CLOSED_##idx); \
    }

#define DO_SET_UV_SLOT_OPEN(idx) \
    BtlRuntimeUpvalue* uv = &frame->closure->upvalues[idx]; \
    if (uv->isOpen) { \
        *uv->loc.stack = peek(vm, 0); \
    } else { \
        uv->loc.box->closed = peek(vm, 0); \
        PATCH_OP_1(BTL_OP_SET_UPVALUE_CLOSED_##idx); \
    }

static void resetStack(VM* vm) {
    vm->stackTop = vm->stack;
    vm->frameCount = 0;
    /* Clear the stack to help GC */
    for (int i = 0; i < vm->stackCapacity; i++) vm->stack[i] = BTL_NULL_VAL;
}

/* Grow the value stack when needed. Returns false on allocation failure. */
bool btl_ensure_stack_capacity(VM* vm, int needed) {
    int used = (int)(vm->stackTop - vm->stack);
    if (used + needed <= vm->stackCapacity) return true;  /* Already have space */

    /* Calculate new capacity */
    int newCapacity = vm->stackCapacity;
    while (newCapacity < used + needed) {
        newCapacity *= BTL_STACK_GROW_FACTOR;
    }

    /* Allocate new stack */
    BtlValue* newStack = (BtlValue*)realloc(vm->stack, sizeof(BtlValue) * newCapacity);
    if (newStack == NULL) return false;

    /* Update all frame slot pointers if stack moved */
    if (newStack != vm->stack) {
        ptrdiff_t offset = newStack - vm->stack;
        for (int i = 0; i < vm->frameCount; i++) {
            vm->frames[i].slots += offset;
        }
        vm->stackTop = newStack + used;
    }

    /* Initialize new slots to BTL_NULL_VAL */
    for (int i = vm->stackCapacity; i < newCapacity; i++) {
        newStack[i] = BTL_NULL_VAL;
    }

    vm->stack = newStack;
    vm->stackCapacity = newCapacity;
    return true;
}

/* Grow the frame stack when needed. Returns false on allocation failure. */
bool btl_ensure_frame_capacity(VM* vm) {
    if (vm->frameCount < vm->frameCapacity) return true;  /* Already have space */

    int newCapacity = vm->frameCapacity * BTL_STACK_GROW_FACTOR;
    BtlCallFrame* newFrames = (BtlCallFrame*)realloc(vm->frames, sizeof(BtlCallFrame) * newCapacity);
    if (newFrames == NULL) return false;

    vm->frames = newFrames;
    vm->frameCapacity = newCapacity;
    return true;
}

void btl_runtime_error(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    btl_verrorf(vm, format, args);
    va_end(args);
    btl_error(vm, "\n");

    for (int i = vm->frameCount - 1; i >= 0; i--) {
        BtlCallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        btl_errorf(vm, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            btl_error(vm, "script\n");
        } else {
            btl_errorf(vm, "%s()\n", function->name->chars);
        }
    }
    resetStack(vm);
}

static BtlValue peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

void btl_push(VM* vm, BtlValue value) {
    /* Grow stack if needed (rare case - check inline) */
    if (vm->stackTop >= vm->stack + vm->stackCapacity) {
        if (!btl_ensure_stack_capacity(vm, 1)) {
            /* Fatal: out of memory. In a real implementation we'd handle this better. */
            fprintf(stderr, "Fatal: out of memory, cannot grow stack.\n");
            exit(1);
        }
    }
    *vm->stackTop = value;
    vm->stackTop++;
}

BtlValue btl_pop(VM* vm) {
    vm->stackTop--;
    return *vm->stackTop;
}

static bool isFalsey(BtlValue value) {
    if (IS_NULL(value)) return true;
    if (IS_BOOL(value)) return !AS_BOOL(value);
    if (IS_INT(value)) return AS_INT(value) == 0;
    if (IS_NUMBER(value)) return AS_NUMBER(value) == 0.0;
    if (IS_STRING(value)) return AS_STRING(value)->length == 0;
    if (IS_LIST(value)) return AS_LIST(value)->items.count == 0;
    if (IS_TABLE(value)) return AS_TABLE(value)->table.count == 0;
    if (IS_FUTURE(value)) {
        BtlFutureState state = btl_future_get_state(AS_FUTURE(value));
        return state == BTL_FUTURE_ERROR;
    }
    if (IS_ACTOR(value)) {
        return !AS_ACTOR(value)->alive;
    }
    return false;
}

// Helper: Get native class for a value
static ObjNativeClass* getNativeClass(VM* vm, BtlValue value) {
    if (IS_STRING(value)) return vm->stringClass;
    if (IS_INT(value))    return vm->intClass;
    if (IS_NUMBER(value)) return vm->numberClass;
    if (IS_LIST(value)) return vm->listClass;
    if (IS_TABLE(value)) return vm->tableClass;
    if (IS_ENTITY(value)) return vm->entityClass;
    return NULL;
}

// Helper: Look up native method
static ObjNativeMethod* findNativeMethod(ObjNativeClass* klass, ObjString* name) {
    if (klass == NULL) return NULL;
    BtlValue method;
    if (btl_table_get(&klass->methods, OBJ_VAL(name), &method)) {
        return AS_NATIVE_METHOD(method);
    }
    return NULL;
}

static void growMethodTable(VM* vm, ObjClass* klass, int requiredIndex) {
    if (requiredIndex < klass->methodCapacity) return;

    int oldCapacity = klass->methodCapacity;
    int newCapacity = oldCapacity < 8 ? 8 : oldCapacity * 2;
    while (newCapacity <= requiredIndex) {
        newCapacity *= 2;
    }

    BtlMethodEntry* newMethods = BTL_ALLOCATE(vm, BtlMethodEntry, newCapacity);

    if (klass->methods != NULL) {
        memcpy(newMethods, klass->methods, sizeof(BtlMethodEntry) * klass->methodCount);
        BTL_FREE_ARRAY(vm, BtlMethodEntry, klass->methods, oldCapacity);
    }

    for (int i = klass->methodCount; i < newCapacity; i++) {
        newMethods[i].closure = NULL;
        newMethods[i].arity = 0;
    }

    klass->methods = newMethods;
    klass->methodCapacity = newCapacity;
}

// --- Upvalue Logic ---

static void closeUpvalues(VM* vm, BtlCallFrame* frame) {
    BtlRuntimeUpvalue* uv = frame->openUpvalues;
    while (uv != NULL) {
        BtlRuntimeUpvalue* next = uv->next;
        if (uv->isOpen) {
            BtlValue val = *uv->loc.stack;
            BtlValue* slotPtr = uv->loc.stack;
            if (uv->isMutable) {
                ObjUpvalue* box = btl_upvalue_box_new(vm, val);
                uv->isOpen = false;
                uv->loc.box = box;
                BtlRuntimeUpvalue* search = next;
                while (search != NULL) {
                    if (search->isOpen && search->loc.stack == slotPtr) {
                        search->isOpen = false;
                        search->loc.box = box;
                    }
                    search = search->next;
                }
            } else {
                uv->isOpen = false;
                uv->loc.immValue = val;
                BtlRuntimeUpvalue* search = next;
                while (search != NULL) {
                    if (search->isOpen && search->loc.stack == slotPtr) {
                        search->isOpen = false;
                        search->loc.immValue = val;
                    }
                    search = search->next;
                }
            }
        }
        uv = next;
    }
    frame->openUpvalues = NULL;
}

// --- Call Logic ---

static bool bindMethod(VM* vm, ObjClass* klass, ObjString* name) {
    for (int i = 0; i < klass->methodCount; i++) {
        if (klass->methods[i].closure != NULL &&
            klass->methods[i].name != NULL &&
            klass->methods[i].name == name) {

            ObjBoundMethod* bound = btl_bound_method_new(vm, peek(vm, 0),
                klass->methods[i].closure);
            btl_pop(vm);
            btl_push(vm, OBJ_VAL(bound));
            return true;
        }
    }
    return false;
}

static bool call(VM* vm, ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        btl_runtime_error(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }
    /* Grow frame stack if needed */
    if (!btl_ensure_frame_capacity(vm)) {
        btl_runtime_error(vm, "Out of memory: cannot grow call stack.");
        return false;
    }
    BtlCallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm->stackTop - argCount - 1;
    frame->openUpvalues = NULL;
    return true;
}

bool btl_call_value(VM* vm, BtlValue callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case BTL_OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
            vm->stackTop[-argCount - 1] = bound->receiver;
            return call(vm, bound->method, argCount);
        }
        case BTL_OBJ_CLASS: {
            ObjClass* klass = AS_CLASS(callee);

            // Native constructor: bypass normal instantiation
            if (klass->nativeConstructor) {
                BtlValue result = klass->nativeConstructor(vm, argCount, vm->stackTop - argCount);
                vm->stackTop -= argCount + 1;  // Pop args + callee
                btl_push(vm, result);
                return true;
            }

            vm->stackTop[-argCount - 1] = OBJ_VAL(btl_instance_new(vm, klass));

            char initSig[6];
            memcpy(initSig, "init", 4);
            initSig[4] = (char) argCount;
            initSig[5] = '\0';

            ObjString* initSignature = btl_string_copy(vm, initSig, 5);
            btl_push(vm, OBJ_VAL(initSignature));

            BtlValue indexValue;
            if (btl_table_get(&klass->methodIndices, OBJ_VAL(initSignature), &indexValue)) {
                int methodIndex = (int) AS_NUMBER(indexValue);
                btl_pop(vm);

                if (methodIndex >= 0 && methodIndex < klass->methodCount) {
                    ObjClosure* initializer = klass->methods[methodIndex].closure;
                    if (initializer != NULL) {
                        return call(vm, initializer, argCount);
                    }
                }
            }
            btl_pop(vm);

            if (argCount != 0) {
                btl_runtime_error(vm, "Expected 0 arguments but got %d.", argCount);
                return false;
            }
            return true;
        }
        case BTL_OBJ_CLOSURE:
            return call(vm, AS_CLOSURE(callee), argCount);
        case BTL_OBJ_NATIVE: {
            BtlNativeFn native = AS_NATIVE(callee);
            BtlValue result = native(vm, argCount, vm->stackTop - argCount);
            vm->stackTop -= argCount + 1;
            btl_push(vm, result);
            return true;
        }
        case BTL_OBJ_NATIVE_METHOD: {
            ObjNativeMethod* method = AS_NATIVE_METHOD(callee);
            if (method->arity >= 0 && argCount != method->arity) {
                btl_runtime_error(vm, "Expected %d arguments but got %d.", method->arity, argCount);
                return false;
            }
            BtlValue* args = vm->stackTop - argCount;
            // For native module methods, receiver is the module on stack
            BtlValue receiver = vm->stackTop[-argCount - 1];
            BtlValue result = method->function(vm, receiver, argCount, args);
            vm->stackTop -= argCount + 1;
            btl_push(vm, result);
            return true;
        }
        case BTL_OBJ_FUTURE: {
            if (argCount != 0) {
                btl_runtime_error(vm, "Futures take no arguments.");
                return false;
            }
            ObjFuture* future = AS_FUTURE(callee);
            BtlValue result = btl_future_get(future);
            BtlFutureState state = btl_future_get_state(future);
            vm->stackTop--;
            if (state == BTL_FUTURE_ERROR) {
                btl_runtime_error(vm, "Future error: %s",
                    IS_STRING(result) ? AS_CSTRING(result) : "unknown");
                return false;
            }
            btl_push(vm, result);
            return true;
        }
        case BTL_OBJ_ACTOR: {
            btl_runtime_error(vm, "Cannot call actor directly. Use actor.method().");
            return false;
        }
        default:
            break;
        }
    }
    btl_runtime_error(vm, "Can only call functions and classes.");
    return false;
}

static ObjString* valueToString(VM* vm, BtlValue value) {
    if (IS_STRING(value)) return AS_STRING(value);
    char buf[32];
    if (IS_INT(value)) {
        int len = snprintf(buf, 32, "%" PRId64, AS_INT(value));
        return btl_string_copy(vm, buf, len);
    }
    if (IS_NUMBER(value)) {
        int len = snprintf(buf, 32, "%g", AS_NUMBER(value));
        return btl_string_copy(vm, buf, len);
    }
    if (IS_BOOL(value)) return btl_string_copy(vm, AS_BOOL(value) ? "true" : "false", AS_BOOL(value) ? 4 : 5);
    if (IS_NULL(value)) return btl_string_copy(vm, "null", 4);
    return btl_string_copy(vm, "<object>", 8);
}

static void concatenate(VM* vm) {
    BtlValue bVal = peek(vm, 0); BtlValue aVal = peek(vm, 1);
    ObjString* a = valueToString(vm, aVal); btl_push(vm, OBJ_VAL(a));
    ObjString* b = valueToString(vm, bVal); btl_push(vm, OBJ_VAL(b));
    int length = a->length + b->length;
    char* chars = BTL_ALLOCATE(vm, char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';
    ObjString* result = btl_string_take(vm, chars, length);
    btl_pop(vm); btl_pop(vm); btl_pop(vm); btl_pop(vm);
    btl_push(vm, OBJ_VAL(result));
}
#ifdef BTL_DEBUG_TRACE_EXECUTION
static void printValueTrace(VM* vm, BtlValue value) {
    char buffer[256];

    if (IS_BOOL(value)) {
        snprintf(buffer, sizeof(buffer), "%s", AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        snprintf(buffer, sizeof(buffer), "null");
    } else if (IS_INT(value)) {
        snprintf(buffer, sizeof(buffer), "%" PRId64, AS_INT(value));
    } else if (IS_NUMBER(value)) {
        snprintf(buffer, sizeof(buffer), "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case BTL_OBJ_STRING:
            snprintf(buffer, sizeof(buffer), "'%s'", AS_CSTRING(value));
            break;
        case BTL_OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            snprintf(buffer, sizeof(buffer), "<fn %s>", fn->name ? fn->name->chars : "script");
            break;
        }
        case BTL_OBJ_CLASS:
            snprintf(buffer, sizeof(buffer), "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case BTL_OBJ_INSTANCE:
            snprintf(buffer, sizeof(buffer), "<%s>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case BTL_OBJ_BOUND_METHOD:
            snprintf(buffer, sizeof(buffer), "<bound %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case BTL_OBJ_CLOSURE: {
            ObjClosure* cl = AS_CLOSURE(value);
            snprintf(buffer, sizeof(buffer), "<fn %s>", cl->function->name ? cl->function->name->chars : "script");
            break;
        }
        case BTL_OBJ_NATIVE:
            snprintf(buffer, sizeof(buffer), "<native>");
            break;
        case BTL_OBJ_LIST:
            snprintf(buffer, sizeof(buffer), "<list[%d]>", AS_LIST(value)->items.count);
            break;
        case BTL_OBJ_TABLE:
            snprintf(buffer, sizeof(buffer), "<table>");
            break;
        case BTL_OBJ_MODULE:
            snprintf(buffer, sizeof(buffer), "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case BTL_OBJ_UPVALUE:
            snprintf(buffer, sizeof(buffer), "<upvalue>");
            break;
        case BTL_OBJ_FUTURE: {
            ObjFuture* future = AS_FUTURE(value);
            BtlFutureState state = btl_future_get_state(future);
            switch (state) {
            case BTL_FUTURE_PENDING: snprintf(buffer, sizeof(buffer), "<future:pending>"); break;
            case BTL_FUTURE_READY:   snprintf(buffer, sizeof(buffer), "<future:ready>"); break;
            case BTL_FUTURE_ERROR:   snprintf(buffer, sizeof(buffer), "<future:error>"); break;
            }
            break;
        }
        case BTL_OBJ_ACTOR: {
            ObjActor* actor = AS_ACTOR(value);
            if (actor->alive) {
                snprintf(buffer, sizeof(buffer), "<actor:%s>", actor->klass->name->chars);
            } else {
                snprintf(buffer, sizeof(buffer), "<actor:dead>");
            }
            break;
        }
        case BTL_OBJ_ENTITY:
            snprintf(buffer, sizeof(buffer), "<entity:%llu>", (unsigned long long)AS_ENTITY(value)->id);
            break;
        default:
            snprintf(buffer, sizeof(buffer), "<obj>");
            break;
        }
    } else {
        snprintf(buffer, sizeof(buffer), "<unknown>");
    }

    btl_errorf(vm, "%s", buffer);
}

static void traceExecution(VM* vm) {
    BtlCallFrame* frame = &vm->frames[vm->frameCount - 1];
    btl_errorf(vm, "          ");
    for (BtlValue* slot = vm->stack; slot < vm->stackTop; slot++) {
        btl_errorf(vm, "[ ");
        printValueTrace(vm, *slot);
        btl_errorf(vm, " ]");
    }
    btl_errorf(vm, "\n");
    int offset = (int) (frame->ip - frame->closure->function->chunk.code);
    btl_disassemble_instruction(vm->runtime, &frame->closure->function->chunk, offset);
}
#endif

// --- IO & Natives ---
static char* readFile(VM* vm, const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) return NULL;
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);
    char* buffer = (char*) btl_realloc(vm, NULL, 0, fileSize + 1);
    if (!buffer) {
        fclose(file); return NULL;
    }
    size_t bytesRead = fread(buffer, 1, fileSize, file);
    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

static ObjString* findGlobalName(ObjModule* module, int index) {
    for (int i = 0; i < module->globalNames.capacity; i++) {
        BtlEntry* entry = &module->globalNames.entries[i];
        if (!IS_EMPTY(entry->key) && IS_STRING(entry->key) && (int) AS_NUMBER(entry->value) == index) return AS_STRING(entry->key);
    }
    return NULL;
}

void btl_vm_init(VM* vm) {
    /* Allocate initial stack and frames */
    vm->stackCapacity = BTL_STACK_INITIAL;
    vm->stack = (BtlValue*)malloc(sizeof(BtlValue) * vm->stackCapacity);
    vm->frameCapacity = BTL_FRAMES_INITIAL;
    vm->frames = (BtlCallFrame*)malloc(sizeof(BtlCallFrame) * vm->frameCapacity);

    resetStack(vm);
    vm->objects = NULL;
    vm->bytesAllocated = 0;
    vm->nextGC = 1024 * 1024;
    vm->grayCount = 0;
    vm->grayCapacity = 0;
    vm->grayStack = NULL;
    // Initialize nursery
    btl_nursery_init(vm, &vm->nursery);
    vm->nurseryAllocated = 0;
    // Initialize remembered set
    btl_remembered_set_init(&vm->rememberedSet);
    // GC stats
    vm->minorGCCount = 0;
    vm->majorGCCount = 0;
    vm->promotedBytes = 0;
    vm->inMinorGC = false;
    vm->gcInhibit = 0;
    btl_table_init(&vm->strings); btl_table_init(&vm->modules);
    btl_table_init(&vm->nativeModules);
    vm->stringClass = NULL;
    vm->numberClass = NULL;
    vm->intClass = NULL;
    vm->listClass = NULL;
    vm->tableClass = NULL;
    vm->entityClass = NULL;
    vm->rootModule = btl_module_new(vm, btl_string_copy(vm, "main", 4));
    vm->initString = btl_string_copy(vm, "init", 4);
    vm->lastReturnValue = BTL_NULL_VAL;
    vm->runFloor = 0;
    vm->nativeRootCount = 0;
    memset(vm->nativeRoots, 0, sizeof(vm->nativeRoots));
    //defineNative(vm, "clock", clockNative);

    // Initialize native classes
    btl_string_class_init(vm);
    btl_list_class_init(vm);
    btl_table_class_init(vm);
    btl_number_class_init(vm);
    btl_int_class_init(vm);

    // Initialize native modules
    btl_system_module_init(vm);
    btl_math_module_init(vm);
    btl_random_module_init(vm);
}

void btl_vm_free(VM* vm, bool mainVM) {
    (void) mainVM;
    btl_table_free(vm, &vm->strings);
    btl_table_free(vm, &vm->modules);
    btl_table_free(vm, &vm->nativeModules);
    vm->initString = NULL;
    btl_gc_free_all(vm);

    /* Free dynamically allocated stack and frames */
    free(vm->stack);
    free(vm->frames);
    vm->stack = NULL;
    vm->frames = NULL;
    vm->stackCapacity = 0;
    vm->frameCapacity = 0;
}

typedef struct {
    VM* vm;
    ObjClosure* closure;
    BtlValue* args;
    int argCount;
    ObjFuture* future;
    ObjNativeClass* stringClass;
    ObjNativeClass* numberClass;
    ObjNativeClass* listClass;
    ObjNativeClass* tableClass;
} AsyncCallTask;

BtlInterpretResult btl_run(VM* vm);  // Forward declaration

static void asyncCallTaskRun(void* arg) {
    AsyncCallTask* task = (AsyncCallTask*) arg;
    VM* vm = task->vm;
    BTLRuntime* runtime = vm->runtime;  // Save runtime BEFORE freeing VM
    int savedArgCount = task->argCount;  // Save before cleanup

    btl_push(vm, OBJ_VAL(task->closure));

    for (int i = 0; i < task->argCount; i++) {
        btl_push(vm, task->args[i]);
    }

    /* Ensure frame capacity for async task */
    if (!btl_ensure_frame_capacity(vm)) {
        ObjString* errMsg = btl_string_copy(vm, "Out of memory: cannot grow call stack", 37);
        btl_future_reject(task->future, OBJ_VAL(errMsg));
        free(task->args);
        free(task);
        return;
    }

    BtlCallFrame* frame = &vm->frames[vm->frameCount++];
    frame->closure = task->closure;
    frame->ip = task->closure->function->chunk.code;
    frame->slots = vm->stack;
    frame->openUpvalues = NULL;

    BtlInterpretResult result = btl_run_vm(vm);

    if (result == BTL_INTERPRET_OK) {
        btl_future_resolve(task->future, vm->lastReturnValue);
    } else {
        ObjString* errMsg = btl_string_copy(vm, "Async call failed", 17);
        btl_future_reject(task->future, OBJ_VAL(errMsg));
    }

    // Cleanup using runtime allocator
    if (task->args != NULL) {
        btl_realloc(vm, task->args, sizeof(BtlValue) * savedArgCount, 0);
    }

    btl_vm_free(vm, false);

    // VM is gone - use runtime allocator directly
    btl_runtime_alloc(runtime, vm, sizeof(VM), 0);
    btl_runtime_alloc(runtime, task, sizeof(AsyncCallTask), 0);
}

BtlInterpretResult btl_run_vm(VM* vm) {
    return btl_run(vm);
}

BtlInterpretResult btl_run(VM* vm) {
    register BtlCallFrame* frame = &vm->frames[vm->frameCount - 1];
    register uint8_t* ip = frame->ip;
    int argCount;

#define STORE_FRAME() (frame->ip = ip)
#define REFRESH_FRAME() (frame = &vm->frames[vm->frameCount - 1], ip = frame->ip)
#define READ_BYTE() (*ip++)
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->closure->function->chunk.constants.values[READ_SHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

// Arithmetic: int+int -> int, double+double -> double, mixed -> double
#define ARITH_OP(op) do { \
    BtlValue _bv = peek(vm, 0), _av = peek(vm, 1); \
    if (IS_INT(_bv) && IS_INT(_av)) { \
        int64_t _bi = AS_INT(btl_pop(vm)); int64_t _ai = AS_INT(btl_pop(vm)); \
        btl_push(vm, INT_VAL(_ai op _bi)); \
    } else if (IS_NUMBER(_bv) && IS_NUMBER(_av)) { \
        double _bd = AS_NUMBER(btl_pop(vm)); double _ad = AS_NUMBER(btl_pop(vm)); \
        btl_push(vm, NUMBER_VAL(_ad op _bd)); \
    } else if (IS_NUMERIC(_bv) && IS_NUMERIC(_av)) { \
        double _bd = btl_numeric_to_double(btl_pop(vm)); \
        double _ad = btl_numeric_to_double(btl_pop(vm)); \
        btl_push(vm, NUMBER_VAL(_ad op _bd)); \
    } else { \
        STORE_FRAME(); btl_runtime_error(vm, "Operands must be numbers."); return BTL_INTERPRET_RUNTIME_ERROR; \
    } \
  } while (false)

// Comparison: always returns BOOL_VAL
#define COMPARE_OP(op) do { \
    BtlValue _bv = peek(vm, 0), _av = peek(vm, 1); \
    if (IS_INT(_bv) && IS_INT(_av)) { \
        int64_t _bi = AS_INT(btl_pop(vm)); int64_t _ai = AS_INT(btl_pop(vm)); \
        btl_push(vm, BOOL_VAL(_ai op _bi)); \
    } else if (IS_NUMBER(_bv) && IS_NUMBER(_av)) { \
        double _bd = AS_NUMBER(btl_pop(vm)); double _ad = AS_NUMBER(btl_pop(vm)); \
        btl_push(vm, BOOL_VAL(_ad op _bd)); \
    } else if (IS_NUMERIC(_bv) && IS_NUMERIC(_av)) { \
        double _bd = btl_numeric_to_double(btl_pop(vm)); \
        double _ad = btl_numeric_to_double(btl_pop(vm)); \
        btl_push(vm, BOOL_VAL(_ad op _bd)); \
    } else { \
        STORE_FRAME(); btl_runtime_error(vm, "Operands must be numbers."); return BTL_INTERPRET_RUNTIME_ERROR; \
    } \
  } while (false)

#ifdef BTL_DEBUG_TRACE_EXECUTION
#define TRACE_IF_ENABLED() traceExecution(vm)
#else
#define TRACE_IF_ENABLED() do { } while (0)
#endif

#ifdef BTL_HAS_COMPUTED_GOTOS
    static void* dispatchTable [] = {
    && L_BTL_OP_CONSTANT,
    && L_BTL_OP_CONSTANT_LONG,
    && L_BTL_OP_NULL,
    && L_BTL_OP_TRUE,
    && L_BTL_OP_FALSE,
    && L_BTL_OP_0,
    && L_BTL_OP_1,
    && L_BTL_OP_2,
    && L_BTL_OP_INT_0,
    && L_BTL_OP_INT_1,
    && L_BTL_OP_INT_2,
    && L_BTL_OP_POP,
    && L_BTL_OP_POP_N,
    && L_BTL_OP_DUP,
    && L_BTL_OP_SWAP,
    && L_BTL_OP_GET_LOCAL,
    && L_BTL_OP_GET_LOCAL_0,
    && L_BTL_OP_GET_LOCAL_1,
    && L_BTL_OP_GET_LOCAL_2,
    && L_BTL_OP_GET_LOCAL_3,
    && L_BTL_OP_GET_LOCAL_4,
    && L_BTL_OP_GET_LOCAL_5,
    && L_BTL_OP_GET_LOCAL_6,
    && L_BTL_OP_GET_LOCAL_7,
    && L_BTL_OP_SET_LOCAL,
    && L_BTL_OP_SET_LOCAL_0,
    && L_BTL_OP_SET_LOCAL_1,
    && L_BTL_OP_SET_LOCAL_2,
    && L_BTL_OP_SET_LOCAL_3,
    && L_BTL_OP_SET_LOCAL_4,
    && L_BTL_OP_SET_LOCAL_5,
    && L_BTL_OP_SET_LOCAL_6,
    && L_BTL_OP_SET_LOCAL_7,
    && L_BTL_OP_SET_LOCAL_0_POP,
    && L_BTL_OP_SET_LOCAL_1_POP,
    && L_BTL_OP_SET_LOCAL_2_POP,
    && L_BTL_OP_SET_LOCAL_3_POP,
    && L_BTL_OP_SET_LOCAL_4_POP,
    && L_BTL_OP_SET_LOCAL_5_POP,
    && L_BTL_OP_SET_LOCAL_6_POP,
    && L_BTL_OP_SET_LOCAL_7_POP,
    && L_BTL_OP_INC_LOCAL_POP,
    && L_BTL_OP_INC_LOCAL,
    && L_BTL_OP_INCREMENT,
    && L_BTL_OP_DECREMENT,
    && L_BTL_OP_GET_GLOBAL,
    && L_BTL_OP_GET_GLOBAL_LONG,
    && L_BTL_OP_DEFINE_GLOBAL,
    && L_BTL_OP_DEFINE_GLOBAL_LONG,
    && L_BTL_OP_SET_GLOBAL,
    && L_BTL_OP_SET_GLOBAL_LONG,
    && L_BTL_OP_GET_UPVALUE,
    && L_BTL_OP_GET_UPVALUE_OPEN,
    && L_BTL_OP_GET_UPVALUE_CLOSED,
    && L_BTL_OP_GET_UPVALUE_IMMUTABLE,
    && L_BTL_OP_SET_UPVALUE,
    && L_BTL_OP_SET_UPVALUE_OPEN,
    && L_BTL_OP_SET_UPVALUE_CLOSED,
    && L_BTL_OP_GET_UPVALUE_0,
    && L_BTL_OP_GET_UPVALUE_OPEN_0,
    && L_BTL_OP_GET_UPVALUE_CLOSED_0,
    && L_BTL_OP_GET_UPVALUE_IMMUTABLE_0,
    && L_BTL_OP_SET_UPVALUE_0,
    && L_BTL_OP_SET_UPVALUE_OPEN_0,
    && L_BTL_OP_SET_UPVALUE_CLOSED_0,
    && L_BTL_OP_GET_UPVALUE_1,
    && L_BTL_OP_GET_UPVALUE_OPEN_1,
    && L_BTL_OP_GET_UPVALUE_CLOSED_1,
    && L_BTL_OP_GET_UPVALUE_IMMUTABLE_1,
    && L_BTL_OP_SET_UPVALUE_1,
    && L_BTL_OP_SET_UPVALUE_OPEN_1,
    && L_BTL_OP_SET_UPVALUE_CLOSED_1,
    && L_BTL_OP_GET_UPVALUE_2,
    && L_BTL_OP_GET_UPVALUE_OPEN_2,
    && L_BTL_OP_GET_UPVALUE_CLOSED_2,
    && L_BTL_OP_GET_UPVALUE_IMMUTABLE_2,
    && L_BTL_OP_SET_UPVALUE_2,
    && L_BTL_OP_SET_UPVALUE_OPEN_2,
    && L_BTL_OP_SET_UPVALUE_CLOSED_2,
    && L_BTL_OP_GET_UPVALUE_3,
    && L_BTL_OP_GET_UPVALUE_OPEN_3,
    && L_BTL_OP_GET_UPVALUE_CLOSED_3,
    && L_BTL_OP_GET_UPVALUE_IMMUTABLE_3,
    && L_BTL_OP_SET_UPVALUE_3,
    && L_BTL_OP_SET_UPVALUE_OPEN_3,
    && L_BTL_OP_SET_UPVALUE_CLOSED_3,
    && L_BTL_OP_FIELD,
    && L_BTL_OP_GET_FIELD_THIS,
    && L_BTL_OP_SET_FIELD_THIS,
    && L_BTL_OP_GET_PROPERTY_IC,
    && L_BTL_OP_SET_PROPERTY_IC,
    && L_BTL_OP_GET_SUPER,
    && L_BTL_OP_GET_SUPER_LONG,
    && L_BTL_OP_EQUAL,
    && L_BTL_OP_GREATER,
    && L_BTL_OP_LESS,
    && L_BTL_OP_ADD,
    && L_BTL_OP_SUBTRACT,
    && L_BTL_OP_MULTIPLY,
    && L_BTL_OP_DIVIDE,
    && L_BTL_OP_MODULO,
    && L_BTL_OP_NOT,
    && L_BTL_OP_NEGATE,
    && L_BTL_OP_JUMP,
    && L_BTL_OP_JUMP_IF_FALSE,
    && L_BTL_OP_POP_JUMP_IF_FALSE,
    && L_BTL_OP_JUMP_IF_TRUE,
    && L_BTL_OP_POP_JUMP_IF_TRUE,
    && L_BTL_OP_JUMP_IF_NOT_EQUAL,
    && L_BTL_OP_JUMP_IF_EQUAL,
    && L_BTL_OP_JUMP_IF_NOT_GREATER,
    && L_BTL_OP_JUMP_IF_NOT_LESS,
    && L_BTL_OP_LOOP,
    && L_BTL_OP_CALL_0,
    && L_BTL_OP_CALL_1,
    && L_BTL_OP_CALL_2,
    && L_BTL_OP_CALL_3,
    && L_BTL_OP_CALL_4,
    && L_BTL_OP_CALL_5,
    && L_BTL_OP_CALL_6,
    && L_BTL_OP_CALL_7,
    && L_BTL_OP_CALL_8,
    && L_BTL_OP_CALL,
    && L_BTL_OP_TAIL_CALL_0,
    && L_BTL_OP_TAIL_CALL_1,
    && L_BTL_OP_TAIL_CALL_2,
    && L_BTL_OP_TAIL_CALL_3,
    && L_BTL_OP_TAIL_CALL_4,
    && L_BTL_OP_TAIL_CALL_5,
    && L_BTL_OP_TAIL_CALL_6,
    && L_BTL_OP_TAIL_CALL_7,
    && L_BTL_OP_TAIL_CALL_8,
    && L_BTL_OP_TAIL_CALL,
    && L_BTL_OP_INVOKE_0,
    && L_BTL_OP_INVOKE_1,
    && L_BTL_OP_INVOKE_2,
    && L_BTL_OP_INVOKE_3,
    && L_BTL_OP_INVOKE_4,
    && L_BTL_OP_INVOKE_5,
    && L_BTL_OP_INVOKE_6,
    && L_BTL_OP_INVOKE_7,
    && L_BTL_OP_INVOKE_8,
    && L_BTL_OP_TAIL_INVOKE_0,
    && L_BTL_OP_TAIL_INVOKE_1,
    && L_BTL_OP_TAIL_INVOKE_2,
    && L_BTL_OP_TAIL_INVOKE_3,
    && L_BTL_OP_TAIL_INVOKE_4,
    && L_BTL_OP_TAIL_INVOKE_5,
    && L_BTL_OP_TAIL_INVOKE_6,
    && L_BTL_OP_TAIL_INVOKE_7,
    && L_BTL_OP_TAIL_INVOKE_8,
    && L_BTL_OP_INVOKE,
    && L_BTL_OP_INVOKE_LONG,
    && L_BTL_OP_INVOKE_IC,
    && L_BTL_OP_TAIL_INVOKE,
    && L_BTL_OP_TAIL_INVOKE_LONG,
    && L_BTL_OP_TAIL_INVOKE_IC,
    && L_BTL_OP_SUPER_INVOKE_0,
    && L_BTL_OP_SUPER_INVOKE_1,
    && L_BTL_OP_SUPER_INVOKE_2,
    && L_BTL_OP_SUPER_INVOKE_3,
    && L_BTL_OP_SUPER_INVOKE_4,
    && L_BTL_OP_SUPER_INVOKE_5,
    && L_BTL_OP_SUPER_INVOKE_6,
    && L_BTL_OP_SUPER_INVOKE_7,
    && L_BTL_OP_SUPER_INVOKE_8,
    && L_BTL_OP_TAIL_SUPER_INVOKE_0,
    && L_BTL_OP_TAIL_SUPER_INVOKE_1,
    && L_BTL_OP_TAIL_SUPER_INVOKE_2,
    && L_BTL_OP_TAIL_SUPER_INVOKE_3,
    && L_BTL_OP_TAIL_SUPER_INVOKE_4,
    && L_BTL_OP_TAIL_SUPER_INVOKE_5,
    && L_BTL_OP_TAIL_SUPER_INVOKE_6,
    && L_BTL_OP_TAIL_SUPER_INVOKE_7,
    && L_BTL_OP_TAIL_SUPER_INVOKE_8,
    && L_BTL_OP_SUPER_INVOKE,
    && L_BTL_OP_SUPER_INVOKE_LONG,
    && L_BTL_OP_TAIL_SUPER_INVOKE,
    && L_BTL_OP_TAIL_SUPER_INVOKE_LONG,
    && L_BTL_OP_CLOSURE,
    && L_BTL_OP_CLOSURE_LONG,
    && L_BTL_OP_CLOSE_UPVALUE,
    && L_BTL_OP_RETURN,
    && L_BTL_OP_CLASS,
    && L_BTL_OP_CLASS_LONG,
    && L_BTL_OP_INHERIT,
    && L_BTL_OP_METHOD,
    && L_BTL_OP_METHOD_LONG,
    && L_BTL_OP_BUILD_LIST,
    && L_BTL_OP_BUILD_TABLE,
    && L_BTL_OP_INDEX_GET,
    && L_BTL_OP_INDEX_SET,
    && L_BTL_OP_IMPORT,
    && L_BTL_OP_IMPORT_LONG,
    && L_BTL_OP_DO_NEW,
    && L_BTL_OP_DO_INVOKE,
    && L_BTL_OP_ITER_INIT,
    && L_BTL_OP_ITER_NEXT
    };

#define DISPATCH() do { \
    TRACE_IF_ENABLED(); goto *dispatchTable[*ip++]; } while (0)
#define OPCODE(name) L_##name
    DISPATCH();
#else
#define DISPATCH() break
#define OPCODE(name) case name
    while (true) {
        TRACE_IF_ENABLED();
        switch (READ_BYTE()) {
#endif

            OPCODE(BTL_OP_CONSTANT) : btl_push(vm, READ_CONSTANT()); DISPATCH();
            OPCODE(BTL_OP_CONSTANT_LONG) : btl_push(vm, READ_CONSTANT_LONG()); DISPATCH();
            OPCODE(BTL_OP_NULL) : btl_push(vm, BTL_NULL_VAL); DISPATCH();
            OPCODE(BTL_OP_TRUE) : btl_push(vm, BOOL_VAL(true)); DISPATCH();
            OPCODE(BTL_OP_FALSE) : btl_push(vm, BOOL_VAL(false)); DISPATCH();
            OPCODE(BTL_OP_0) : btl_push(vm, NUMBER_VAL(0.0)); DISPATCH();
            OPCODE(BTL_OP_1) : btl_push(vm, NUMBER_VAL(1.0)); DISPATCH();
            OPCODE(BTL_OP_2) : btl_push(vm, NUMBER_VAL(2.0)); DISPATCH();
            OPCODE(BTL_OP_INT_0) : btl_push(vm, INT_VAL(0)); DISPATCH();
            OPCODE(BTL_OP_INT_1) : btl_push(vm, INT_VAL(1)); DISPATCH();
            OPCODE(BTL_OP_INT_2) : btl_push(vm, INT_VAL(2)); DISPATCH();
            OPCODE(BTL_OP_POP) : btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_POP_N) : vm->stackTop -= READ_BYTE(); DISPATCH();
            OPCODE(BTL_OP_DUP) : btl_push(vm, peek(vm, 0)); DISPATCH();
            OPCODE(BTL_OP_SWAP) : {
                BtlValue temp = vm->stackTop[-1];
                vm->stackTop[-1] = vm->stackTop[-2];
                vm->stackTop[-2] = temp;
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_LOCAL) : btl_push(vm, frame->slots[READ_BYTE()]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_0) : btl_push(vm, frame->slots[0]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_1) : btl_push(vm, frame->slots[1]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_2) : btl_push(vm, frame->slots[2]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_3) : btl_push(vm, frame->slots[3]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_4) : btl_push(vm, frame->slots[4]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_5) : btl_push(vm, frame->slots[5]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_6) : btl_push(vm, frame->slots[6]); DISPATCH();
            OPCODE(BTL_OP_GET_LOCAL_7) : btl_push(vm, frame->slots[7]); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL) : frame->slots[READ_BYTE()] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_0) : frame->slots[0] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_1) : frame->slots[1] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_2) : frame->slots[2] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_3) : frame->slots[3] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_4) : frame->slots[4] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_5) : frame->slots[5] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_6) : frame->slots[6] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_7) : frame->slots[7] = peek(vm, 0); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_0_POP) : frame->slots[0] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_1_POP) : frame->slots[1] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_2_POP) : frame->slots[2] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_3_POP) : frame->slots[3] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_4_POP) : frame->slots[4] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_5_POP) : frame->slots[5] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_6_POP) : frame->slots[6] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_LOCAL_7_POP) : frame->slots[7] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_INC_LOCAL_POP) : {
                uint8_t slot = READ_BYTE();
                BtlValue val = frame->slots[slot];
                if (IS_INT(val)) {
                    frame->slots[slot] = INT_VAL(AS_INT(val) + 1);
                } else if (IS_NUMBER(val)) {
                    frame->slots[slot] = NUMBER_VAL(AS_NUMBER(val) + 1.0);
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Can only increment numbers."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_INC_LOCAL) : {
                uint8_t slot = READ_BYTE();
                BtlValue val = frame->slots[slot];
                if (IS_INT(val)) {
                    BtlValue res = INT_VAL(AS_INT(val) + 1);
                    frame->slots[slot] = res;
                    btl_push(vm, res);
                } else if (IS_NUMBER(val)) {
                    double num = AS_NUMBER(val) + 1.0;
                    frame->slots[slot] = NUMBER_VAL(num);
                    btl_push(vm, NUMBER_VAL(num));
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Can only increment numbers."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_INCREMENT) : {
                BtlValue val = peek(vm, 0);
                if (IS_INT(val)) {
                    btl_pop(vm);
                    btl_push(vm, INT_VAL(AS_INT(val) + 1));
                } else if (IS_NUMBER(val)) {
                    btl_pop(vm);
                    btl_push(vm, NUMBER_VAL(AS_NUMBER(val) + 1));
                } else {
                    STORE_FRAME();
                    btl_runtime_error(vm, "Operand must be a number.");
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_DECREMENT) : {
                BtlValue val = peek(vm, 0);
                if (IS_INT(val)) {
                    btl_pop(vm);
                    btl_push(vm, INT_VAL(AS_INT(val) - 1));
                } else if (IS_NUMBER(val)) {
                    btl_pop(vm);
                    btl_push(vm, NUMBER_VAL(AS_NUMBER(val) - 1));
                } else {
                    STORE_FRAME();
                    btl_runtime_error(vm, "Operand must be a number.");
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                BtlValue val = frame->closure->function->module->globalValues.values[index];
                if (IS_EMPTY(val)) {
                    STORE_FRAME(); btl_runtime_error(vm, "Undefined variable '%s'.", findGlobalName(frame->closure->function->module, (int) index)->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                btl_push(vm, val); DISPATCH();
            }
            OPCODE(BTL_OP_GET_GLOBAL_LONG) : {
                uint16_t index = READ_SHORT();
                BtlValue val = frame->closure->function->module->globalValues.values[index];
                if (IS_EMPTY(val)) {
                    STORE_FRAME(); btl_runtime_error(vm, "Undefined variable '%s'.", findGlobalName(frame->closure->function->module, (int) index)->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                btl_push(vm, val); DISPATCH();
            }
            OPCODE(BTL_OP_DEFINE_GLOBAL) : frame->closure->function->module->globalValues.values[READ_BYTE()] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_DEFINE_GLOBAL_LONG) : frame->closure->function->module->globalValues.values[READ_SHORT()] = btl_pop(vm); DISPATCH();
            OPCODE(BTL_OP_SET_GLOBAL) : {
                uint8_t index = READ_BYTE();
                if (IS_EMPTY(frame->closure->function->module->globalValues.values[index])) {
                    STORE_FRAME(); btl_runtime_error(vm, "Undefined variable '%s'.", findGlobalName(frame->closure->function->module, (int) index)->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                BtlValue value = peek(vm, 0);
                frame->closure->function->module->globalValues.values[index] = value;
                btl_gc_write_barrier(vm, (BtlObj*) frame->closure->function->module, value);
                DISPATCH();
            }
            OPCODE(BTL_OP_SET_GLOBAL_LONG) : {
                uint16_t index = READ_SHORT();
                if (IS_EMPTY(frame->closure->function->module->globalValues.values[index])) {
                    STORE_FRAME(); btl_runtime_error(vm, "Undefined variable '%s'.", findGlobalName(frame->closure->function->module, (int) index)->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                BtlValue value = peek(vm, 0);
                frame->closure->function->module->globalValues.values[index] = value;
                btl_gc_write_barrier(vm, (BtlObj*) frame->closure->function->module, value);
                DISPATCH();
            }

            OPCODE(BTL_OP_CLOSURE) : {
                ObjFunction* f = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* c = btl_closure_new(vm, f);
                btl_push(vm, OBJ_VAL(c));
                for (int i = 0; i < f->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    uint8_t isMutable = READ_BYTE();
                    BtlRuntimeUpvalue* dest = &c->upvalues[i];
                    dest->isMutable = (bool) isMutable;
                    if (isLocal) {
                        dest->isOpen = true; dest->loc.stack = frame->slots + index;
                        dest->next = frame->openUpvalues; frame->openUpvalues = dest;
                    } else {
                        BtlRuntimeUpvalue* parentUV = &frame->closure->upvalues[index];
                        dest->isOpen = parentUV->isOpen;
                        if (parentUV->isOpen) {
                            dest->loc.stack = parentUV->loc.stack; dest->next = parentUV->next; parentUV->next = dest;
                        } else {
                            if (parentUV->isMutable) dest->loc.box = parentUV->loc.box;
                            else dest->loc.immValue = parentUV->loc.immValue;
                            dest->next = NULL;
                        }
                    }
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_CLOSURE_LONG) : {
                ObjFunction* f = AS_FUNCTION(READ_CONSTANT_LONG());
                ObjClosure* c = btl_closure_new(vm, f);
                btl_push(vm, OBJ_VAL(c));
                for (int i = 0; i < f->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    uint8_t isMutable = READ_BYTE();
                    BtlRuntimeUpvalue* dest = &c->upvalues[i];
                    dest->isMutable = (bool) isMutable;
                    if (isLocal) {
                        dest->isOpen = true; dest->loc.stack = frame->slots + index;
                        dest->next = frame->openUpvalues; frame->openUpvalues = dest;
                    } else {
                        BtlRuntimeUpvalue* parentUV = &frame->closure->upvalues[index];
                        dest->isOpen = parentUV->isOpen;
                        if (parentUV->isOpen) {
                            dest->loc.stack = parentUV->loc.stack; dest->next = parentUV->next; parentUV->next = dest;
                        } else {
                            if (parentUV->isMutable) dest->loc.box = parentUV->loc.box;
                            else dest->loc.immValue = parentUV->loc.immValue;
                            dest->next = NULL;
                        }
                    }
                }
                DISPATCH();
            }

            OPCODE(BTL_OP_GET_UPVALUE) : {
                uint8_t slot = READ_BYTE();
                BtlRuntimeUpvalue* uv = &frame->closure->upvalues[slot];
                if (uv->isOpen) {
                    btl_push(vm, *uv->loc.stack); PATCH_OP_2(BTL_OP_GET_UPVALUE_OPEN);
                } else {
                    if (uv->isMutable) {
                        btl_push(vm, uv->loc.box->closed); PATCH_OP_2(BTL_OP_GET_UPVALUE_CLOSED);
                    } else {
                        btl_push(vm, uv->loc.immValue); PATCH_OP_2(BTL_OP_GET_UPVALUE_IMMUTABLE);
                    }
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_UPVALUE_OPEN) : {
                uint8_t slot = READ_BYTE();
                BtlRuntimeUpvalue* uv = &frame->closure->upvalues[slot];
                if (uv->isOpen) {
                    btl_push(vm, *uv->loc.stack);
                } else {
                    if (uv->isMutable) {
                        btl_push(vm, uv->loc.box->closed); PATCH_OP_2(BTL_OP_GET_UPVALUE_CLOSED);
                    } else {
                        btl_push(vm, uv->loc.immValue); PATCH_OP_2(BTL_OP_GET_UPVALUE_IMMUTABLE);
                    }
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_UPVALUE_CLOSED) : { uint8_t slot = READ_BYTE(); btl_push(vm, frame->closure->upvalues[slot].loc.box->closed); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_IMMUTABLE) : { uint8_t slot = READ_BYTE(); btl_push(vm, frame->closure->upvalues[slot].loc.immValue); DISPATCH(); }

            OPCODE(BTL_OP_GET_UPVALUE_0) : { DO_GET_UV_SLOT(0); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_OPEN_0) : { DO_GET_UV_SLOT_OPEN(0); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_CLOSED_0) : { btl_push(vm, frame->closure->upvalues[0].loc.box->closed); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_IMMUTABLE_0) : { btl_push(vm, frame->closure->upvalues[0].loc.immValue); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_1) : { DO_GET_UV_SLOT(1); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_OPEN_1) : { DO_GET_UV_SLOT_OPEN(1); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_CLOSED_1) : { btl_push(vm, frame->closure->upvalues[1].loc.box->closed); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_IMMUTABLE_1) : { btl_push(vm, frame->closure->upvalues[1].loc.immValue); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_2) : { DO_GET_UV_SLOT(2); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_OPEN_2) : { DO_GET_UV_SLOT_OPEN(2); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_CLOSED_2) : { btl_push(vm, frame->closure->upvalues[2].loc.box->closed); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_IMMUTABLE_2) : { btl_push(vm, frame->closure->upvalues[2].loc.immValue); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_3) : { DO_GET_UV_SLOT(3); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_OPEN_3) : { DO_GET_UV_SLOT_OPEN(3); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_CLOSED_3) : { btl_push(vm, frame->closure->upvalues[3].loc.box->closed); DISPATCH(); }
            OPCODE(BTL_OP_GET_UPVALUE_IMMUTABLE_3) : { btl_push(vm, frame->closure->upvalues[3].loc.immValue); DISPATCH(); }

            OPCODE(BTL_OP_SET_UPVALUE) : {
                uint8_t slot = READ_BYTE();
                BtlRuntimeUpvalue* uv = &frame->closure->upvalues[slot];
                if (uv->isOpen) {
                    *uv->loc.stack = peek(vm, 0); PATCH_OP_2(BTL_OP_SET_UPVALUE_OPEN);
                } else {
                    uv->loc.box->closed = peek(vm, 0); PATCH_OP_2(BTL_OP_SET_UPVALUE_CLOSED);
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_SET_UPVALUE_OPEN) : {
                uint8_t slot = READ_BYTE();
                BtlRuntimeUpvalue* uv = &frame->closure->upvalues[slot];
                if (uv->isOpen) {
                    *uv->loc.stack = peek(vm, 0);
                } else {
                    uv->loc.box->closed = peek(vm, 0); PATCH_OP_2(BTL_OP_SET_UPVALUE_CLOSED);
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_SET_UPVALUE_CLOSED) : { uint8_t slot = READ_BYTE(); frame->closure->upvalues[slot].loc.box->closed = peek(vm, 0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_0) : { DO_SET_UV_SLOT(0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_OPEN_0) : { DO_SET_UV_SLOT_OPEN(0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_CLOSED_0) : { frame->closure->upvalues[0].loc.box->closed = peek(vm, 0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_1) : { DO_SET_UV_SLOT(1); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_OPEN_1) : { DO_SET_UV_SLOT_OPEN(1); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_CLOSED_1) : { frame->closure->upvalues[1].loc.box->closed = peek(vm, 0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_2) : { DO_SET_UV_SLOT(2); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_OPEN_2) : { DO_SET_UV_SLOT_OPEN(2); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_CLOSED_2) : { frame->closure->upvalues[2].loc.box->closed = peek(vm, 0); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_3) : { DO_SET_UV_SLOT(3); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_OPEN_3) : { DO_SET_UV_SLOT_OPEN(3); DISPATCH(); }
            OPCODE(BTL_OP_SET_UPVALUE_CLOSED_3) : { frame->closure->upvalues[3].loc.box->closed = peek(vm, 0); DISPATCH(); }

            OPCODE(BTL_OP_FIELD) : {
                ObjString* name = READ_STRING();
                ObjClass* klass = AS_CLASS(peek(vm, 0));
                BtlValue dummy;
                if (!btl_table_get(&klass->fieldIndices, OBJ_VAL(name), &dummy)) {
                    btl_table_set(vm, &klass->fieldIndices, OBJ_VAL(name), NUMBER_VAL(klass->fieldCount));
                    klass->fieldCount++;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_FIELD_THIS) : {
                uint8_t index = READ_BYTE();
                BtlValue receiver = frame->slots[0];
                ObjInstance* instance = AS_INSTANCE(receiver);
                if (instance->klass->nativeGetters && instance->klass->nativeGetters[index]) {
                    btl_push(vm, instance->klass->nativeGetters[index](vm, instance->nativeData, index));
                } else {
                    btl_push(vm, instance->fields[index]);
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_SET_FIELD_THIS) : {
                uint8_t index = READ_BYTE();
                BtlValue receiver = frame->slots[0];
                ObjInstance* instance = AS_INSTANCE(receiver);
                BtlValue value = peek(vm, 0);
                if (instance->klass->nativeSetters && instance->klass->nativeSetters[index]) {
                    instance->klass->nativeSetters[index](vm, instance->nativeData, index, value);
                } else {
                    instance->fields[index] = value;
                    btl_gc_write_barrier(vm, (BtlObj*) instance, value);
                }
                DISPATCH();
            }

            OPCODE(BTL_OP_GET_PROPERTY_IC) : {
                uint8_t nameIdx = READ_BYTE();
                uint8_t icSlot = READ_BYTE();

                BtlValue receiver = peek(vm, 0);

                if (IS_INSTANCE(receiver)) {
                    ObjInstance* instance = AS_INSTANCE(receiver);
                    BtlFieldIC* ic = &frame->closure->fieldICs[icSlot];

                    // FAST PATH
                    if (ic->cachedClass == instance->klass && ic->fieldIndex >= 0) {
                        int fi = ic->fieldIndex;
                        if (instance->klass->nativeGetters && instance->klass->nativeGetters[fi]) {
                            vm->stackTop[-1] = instance->klass->nativeGetters[fi](vm, instance->nativeData, fi);
                        } else {
                            vm->stackTop[-1] = instance->fields[fi];
                        }
                        DISPATCH();
                    }

                    // SLOW PATH
                    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                    BtlValue indexVal;

                    if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
                        int idx = (int) AS_NUMBER(indexVal);
                        ic->cachedClass = instance->klass;
                        ic->fieldIndex = idx;
                        if (instance->klass->nativeGetters && instance->klass->nativeGetters[idx]) {
                            vm->stackTop[-1] = instance->klass->nativeGetters[idx](vm, instance->nativeData, idx);
                        } else {
                            vm->stackTop[-1] = instance->fields[idx];
                        }
                        DISPATCH();
                    }

                    if (bindMethod(vm, instance->klass, name)) DISPATCH();

                    STORE_FRAME();
                    btl_runtime_error(vm, "Undefined property '%s'.", name->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;

                } else if (IS_MODULE(receiver)) {
                    ObjModule* m = AS_MODULE(receiver);
                    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                    BtlValue idx;

                    if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
                        btl_pop(vm);
                        btl_push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]);
                        DISPATCH();
                    }

                    if (name->length > 0) {
                        ObjString* rawName = btl_string_copy(vm, name->chars, name->length - 1);
                        btl_push(vm, OBJ_VAL(rawName));
                        if (btl_table_get(&m->globalNames, OBJ_VAL(rawName), &idx)) {
                            btl_pop(vm);
                            btl_pop(vm);
                            btl_push(vm, m->globalValues.values[(int) AS_NUMBER(idx)]);
                            DISPATCH();
                        }
                        btl_pop(vm);
                    }

                    STORE_FRAME();
                    btl_runtime_error(vm, "Undefined property '%s' in module.", name->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                } else if (IS_NATIVE_MODULE(receiver)) {
                    ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
                    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                    BtlValue value;
                    if (btl_table_get(&module->globals, OBJ_VAL(name), &value)) {
                        btl_pop(vm);
                        btl_push(vm, value);
                        DISPATCH();
                    }
                    STORE_FRAME();
                    btl_runtime_error(vm, "Undefined property '%s' in native module.", name->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                } else {
                    // Check for native class method
                    ObjNativeClass* nativeClass = getNativeClass(vm, receiver);
                    if (nativeClass != NULL) {
                        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                        ObjNativeMethod* method = findNativeMethod(nativeClass, name);
                        // Int fallback: if method not found on intClass, try numberClass
                        if (method == NULL && IS_INT(receiver)) {
                            method = findNativeMethod(vm->numberClass, name);
                        }
                        if (method != NULL) {
                            // Leave receiver on stack, push method
                            // This will be handled by INVOKE
                            vm->stackTop[-1] = OBJ_VAL(method);
                            DISPATCH();
                        }
                    }
                }

                STORE_FRAME();
                btl_runtime_error(vm, "Only instances and modules have properties.");
                return BTL_INTERPRET_RUNTIME_ERROR;
            }

            OPCODE(BTL_OP_SET_PROPERTY_IC) : {
                uint8_t nameIdx = READ_BYTE();
                uint8_t icSlot = READ_BYTE();

                BtlValue receiver = peek(vm, 1);

                if (IS_INSTANCE(receiver)) {
                    ObjInstance* instance = AS_INSTANCE(receiver);
                    BtlFieldIC* ic = &frame->closure->fieldICs[icSlot];

                    // FAST PATH
                    if (ic->cachedClass == instance->klass && ic->fieldIndex >= 0) {
                        int fi = ic->fieldIndex;
                        BtlValue val = peek(vm, 0);
                        if (instance->klass->nativeSetters && instance->klass->nativeSetters[fi]) {
                            instance->klass->nativeSetters[fi](vm, instance->nativeData, fi, val);
                        } else {
                            instance->fields[fi] = val;
                        }
                        btl_pop(vm);
                        btl_pop(vm);
                        btl_push(vm, val);
                        DISPATCH();
                    }

                    // SLOW PATH
                    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                    BtlValue indexVal;

                    if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &indexVal)) {
                        int idx = (int) AS_NUMBER(indexVal);
                        ic->cachedClass = instance->klass;
                        ic->fieldIndex = idx;
                        BtlValue val = peek(vm, 0);
                        if (instance->klass->nativeSetters && instance->klass->nativeSetters[idx]) {
                            instance->klass->nativeSetters[idx](vm, instance->nativeData, idx, val);
                        } else {
                            instance->fields[idx] = val;
                        }
                        btl_pop(vm);
                        btl_pop(vm);
                        btl_push(vm, val);
                        DISPATCH();
                    }

                    STORE_FRAME();
                    btl_runtime_error(vm, "Cannot add new property '%s' to fixed class layout.", name->chars);
                    return BTL_INTERPRET_RUNTIME_ERROR;

                } else if (IS_MODULE(receiver)) {
                    ObjModule* m = AS_MODULE(receiver);
                    ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                    BtlValue idx;
                    if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
                        BtlValue val = peek(vm, 0);
                        m->globalValues.values[(int) AS_NUMBER(idx)] = val;
                        btl_pop(vm);
                        btl_pop(vm);
                        btl_push(vm, val);
                        DISPATCH();
                    }
                    STORE_FRAME();
                    btl_runtime_error(vm, "Cannot set undefined property in module.");
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }

                STORE_FRAME();
                btl_runtime_error(vm, "Only instances have fields.");
                return BTL_INTERPRET_RUNTIME_ERROR;
            }

            OPCODE(BTL_OP_GET_SUPER) : {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(btl_pop(vm));
                STORE_FRAME();
                if (!bindMethod(vm, superclass, name)) return BTL_INTERPRET_RUNTIME_ERROR;
                DISPATCH();
            }
            OPCODE(BTL_OP_GET_SUPER_LONG) : {
                ObjString* name = READ_STRING_LONG();
                ObjClass* superclass = AS_CLASS(btl_pop(vm));
                STORE_FRAME();
                if (!bindMethod(vm, superclass, name)) return BTL_INTERPRET_RUNTIME_ERROR;
                DISPATCH();
            }
            OPCODE(BTL_OP_EQUAL) : {
                BtlValue b = btl_pop(vm);
                BtlValue a = btl_pop(vm);

                // future == null checks if pending
                if (IS_FUTURE(a) && IS_NULL(b)) {
                    btl_push(vm, BOOL_VAL(btl_future_get_state(AS_FUTURE(a)) == BTL_FUTURE_PENDING));
                    DISPATCH();
                }
                if (IS_NULL(a) && IS_FUTURE(b)) {
                    btl_push(vm, BOOL_VAL(btl_future_get_state(AS_FUTURE(b)) == BTL_FUTURE_PENDING));
                    DISPATCH();
                }

                // actor == null checks if dead
                if (IS_ACTOR(a) && IS_NULL(b)) {
                    btl_push(vm, BOOL_VAL(!AS_ACTOR(a)->alive));
                    DISPATCH();
                }
                if (IS_NULL(a) && IS_ACTOR(b)) {
                    btl_push(vm, BOOL_VAL(!AS_ACTOR(b)->alive));
                    DISPATCH();
                }

                btl_push(vm, BOOL_VAL(btl_values_equal(a, b)));
                DISPATCH();
            }
            OPCODE(BTL_OP_GREATER) : { COMPARE_OP(>); DISPATCH(); }
            OPCODE(BTL_OP_LESS) : { COMPARE_OP(<); DISPATCH(); }
            OPCODE(BTL_OP_ADD) : {
                BtlValue _peek0 = peek(vm, 0), _peek1 = peek(vm, 1);
                if (IS_INT(_peek0) && IS_INT(_peek1)) {
                    int64_t b = AS_INT(btl_pop(vm)); int64_t a = AS_INT(btl_pop(vm));
                    btl_push(vm, INT_VAL(a + b));
                } else if (IS_NUMBER(_peek0) && IS_NUMBER(_peek1)) {
                    double b = AS_NUMBER(btl_pop(vm)); double a = AS_NUMBER(btl_pop(vm));
                    btl_push(vm, NUMBER_VAL(a + b));
                } else if (IS_NUMERIC(_peek0) && IS_NUMERIC(_peek1)) {
                    double b = btl_numeric_to_double(btl_pop(vm));
                    double a = btl_numeric_to_double(btl_pop(vm));
                    btl_push(vm, NUMBER_VAL(a + b));
                } else if (IS_STRING(_peek0) || IS_STRING(_peek1)) {
                    if ((IS_STRING(_peek0) || IS_NUMERIC(_peek0)) &&
                        (IS_STRING(_peek1) || IS_NUMERIC(_peek1))) {
                        STORE_FRAME(); concatenate(vm);
                    } else {
                        STORE_FRAME(); btl_runtime_error(vm, "Operands must be two numbers or two strings."); return BTL_INTERPRET_RUNTIME_ERROR;
                    }
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Operands must be two numbers or two strings."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_SUBTRACT) : { ARITH_OP(-); DISPATCH(); }
            OPCODE(BTL_OP_MULTIPLY) : { ARITH_OP(*); DISPATCH(); }
            OPCODE(BTL_OP_DIVIDE) : {
                BtlValue _dv0 = peek(vm, 0), _dv1 = peek(vm, 1);
                if (IS_INT(_dv0) && IS_INT(_dv1)) {
                    int64_t b = AS_INT(btl_pop(vm)); int64_t a = AS_INT(btl_pop(vm));
                    if (b == 0) { STORE_FRAME(); btl_runtime_error(vm, "Division by zero."); return BTL_INTERPRET_RUNTIME_ERROR; }
                    btl_push(vm, INT_VAL(a / b));
                } else if (IS_NUMERIC(_dv0) && IS_NUMERIC(_dv1)) {
                    double b = btl_numeric_to_double(btl_pop(vm));
                    double a = btl_numeric_to_double(btl_pop(vm));
                    btl_push(vm, NUMBER_VAL(a / b));
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Operands must be numbers."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_MODULO) : {
                BtlValue _mv0 = peek(vm, 0), _mv1 = peek(vm, 1);
                if (IS_INT(_mv0) && IS_INT(_mv1)) {
                    int64_t b = AS_INT(btl_pop(vm)); int64_t a = AS_INT(btl_pop(vm));
                    if (b == 0) { STORE_FRAME(); btl_runtime_error(vm, "Division by zero."); return BTL_INTERPRET_RUNTIME_ERROR; }
                    btl_push(vm, INT_VAL(a % b));
                } else if (IS_NUMERIC(_mv0) && IS_NUMERIC(_mv1)) {
                    double b = btl_numeric_to_double(btl_pop(vm));
                    double a = btl_numeric_to_double(btl_pop(vm));
                    btl_push(vm, NUMBER_VAL(fmod(a, b)));
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Operands must be numbers."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_NOT) : { btl_push(vm, BOOL_VAL(isFalsey(btl_pop(vm)))); DISPATCH(); }
            OPCODE(BTL_OP_NEGATE) : {
                BtlValue _nv = peek(vm, 0);
                if (IS_INT(_nv)) {
                    btl_pop(vm); btl_push(vm, INT_VAL(-AS_INT(_nv)));
                } else if (IS_NUMBER(_nv)) {
                    btl_pop(vm); btl_push(vm, NUMBER_VAL(-AS_NUMBER(_nv)));
                } else {
                    STORE_FRAME(); btl_runtime_error(vm, "Must be number."); return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }

            OPCODE(BTL_OP_JUMP) : { uint16_t offset = READ_SHORT(); ip += offset; DISPATCH(); }
            OPCODE(BTL_OP_JUMP_IF_FALSE) : { uint16_t offset = READ_SHORT(); if (isFalsey(peek(vm, 0))) ip += offset; DISPATCH(); }
            OPCODE(BTL_OP_POP_JUMP_IF_FALSE) : { uint16_t offset = READ_SHORT(); if (isFalsey(btl_pop(vm))) ip += offset; DISPATCH(); }
            OPCODE(BTL_OP_JUMP_IF_TRUE) : { uint16_t offset = READ_SHORT(); if (!isFalsey(peek(vm, 0))) ip += offset; DISPATCH(); }
            OPCODE(BTL_OP_POP_JUMP_IF_TRUE) : { uint16_t offset = READ_SHORT(); if (!isFalsey(btl_pop(vm))) ip += offset; DISPATCH(); }
            OPCODE(BTL_OP_JUMP_IF_NOT_EQUAL) : {
                uint16_t offset = READ_SHORT();
                BtlValue b = btl_pop(vm);
                BtlValue a = btl_pop(vm);
                if (!btl_values_equal(a, b)) ip += offset;
                DISPATCH();
            }
            OPCODE(BTL_OP_JUMP_IF_EQUAL) : {
                uint16_t offset = READ_SHORT();
                BtlValue b = btl_pop(vm);
                BtlValue a = btl_pop(vm);
                if (btl_values_equal(a, b)) ip += offset;
                DISPATCH();
            }
            OPCODE(BTL_OP_JUMP_IF_NOT_GREATER) : {
                uint16_t offset = READ_SHORT();
                BtlValue _jb = peek(vm, 0), _ja = peek(vm, 1);
                if (IS_INT(_jb) && IS_INT(_ja)) {
                    int64_t b = AS_INT(btl_pop(vm)); int64_t a = AS_INT(btl_pop(vm));
                    if (!(a > b)) ip += offset;
                } else if (IS_NUMERIC(_jb) && IS_NUMERIC(_ja)) {
                    double b = btl_numeric_to_double(btl_pop(vm));
                    double a = btl_numeric_to_double(btl_pop(vm));
                    if (!(a > b)) ip += offset;
                } else {
                    btl_runtime_error(vm, "Operands must be numbers.");
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_JUMP_IF_NOT_LESS) : {
                uint16_t offset = READ_SHORT();
                BtlValue _jb2 = peek(vm, 0), _ja2 = peek(vm, 1);
                if (IS_INT(_jb2) && IS_INT(_ja2)) {
                    int64_t b = AS_INT(btl_pop(vm)); int64_t a = AS_INT(btl_pop(vm));
                    if (!(a < b)) ip += offset;
                } else if (IS_NUMERIC(_jb2) && IS_NUMERIC(_ja2)) {
                    double b = btl_numeric_to_double(btl_pop(vm));
                    double a = btl_numeric_to_double(btl_pop(vm));
                    if (!(a < b)) ip += offset;
                } else {
                    btl_runtime_error(vm, "Operands must be numbers.");
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                DISPATCH();
            }
            OPCODE(BTL_OP_LOOP) : { uint16_t offset = READ_SHORT(); ip -= offset; DISPATCH(); }
            OPCODE(BTL_OP_RETURN) : {
                BtlValue result = btl_pop(vm);
                closeUpvalues(vm, frame);
                vm->frameCount--;
                if (vm->frameCount == 0) {
                    vm->lastReturnValue = result;
                    btl_pop(vm); return BTL_INTERPRET_OK;
                }
                vm->stackTop = frame->slots;
                btl_push(vm, result);
                if (vm->frameCount <= vm->runFloor) {
                    return BTL_INTERPRET_OK;
                }
                REFRESH_FRAME();
                DISPATCH();
            }

            OPCODE(BTL_OP_CALL_0) : argCount = 0; goto do_call;
            OPCODE(BTL_OP_CALL_1) : argCount = 1; goto do_call;
            OPCODE(BTL_OP_CALL_2) : argCount = 2; goto do_call;
            OPCODE(BTL_OP_CALL_3) : argCount = 3; goto do_call;
            OPCODE(BTL_OP_CALL_4) : argCount = 4; goto do_call;
            OPCODE(BTL_OP_CALL_5) : argCount = 5; goto do_call;
            OPCODE(BTL_OP_CALL_6) : argCount = 6; goto do_call;
            OPCODE(BTL_OP_CALL_7) : argCount = 7; goto do_call;
            OPCODE(BTL_OP_CALL_8) : argCount = 8; goto do_call;
            OPCODE(BTL_OP_CALL) : argCount = READ_BYTE();
        do_call:
            STORE_FRAME();
            if (!btl_call_value(vm, peek(vm, argCount), argCount)) return BTL_INTERPRET_RUNTIME_ERROR;
            REFRESH_FRAME();
            DISPATCH();

            OPCODE(BTL_OP_TAIL_CALL_0) : argCount = 0; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_1) : argCount = 1; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_2) : argCount = 2; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_3) : argCount = 3; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_4) : argCount = 4; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_5) : argCount = 5; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_6) : argCount = 6; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_7) : argCount = 7; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL_8) : argCount = 8; goto do_tail_call;
            OPCODE(BTL_OP_TAIL_CALL) : argCount = READ_BYTE();
        do_tail_call: {
            BtlValue callee = peek(vm, argCount);
            if (!IS_CLOSURE(callee)) {
                STORE_FRAME(); if (!btl_call_value(vm, callee, argCount)) return BTL_INTERPRET_RUNTIME_ERROR; REFRESH_FRAME(); DISPATCH();
            }
            ObjClosure* c = AS_CLOSURE(callee);
            if (argCount != c->function->arity) {
                STORE_FRAME(); btl_runtime_error(vm, "Expected %d arguments but got %d.", c->function->arity, argCount); return BTL_INTERPRET_RUNTIME_ERROR;
            }
            closeUpvalues(vm, frame);
            memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue) * (argCount + 1));
            vm->stackTop = frame->slots + (argCount + 1);
            frame->closure = c;
            frame->ip = c->function->chunk.code;
            frame->openUpvalues = NULL;
            ip = frame->ip;
            DISPATCH();
            }

        // Indexed invoke opcodes (0-8 args)
        OPCODE(BTL_OP_INVOKE_0) : argCount = 0; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_1) : argCount = 1; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_2) : argCount = 2; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_3) : argCount = 3; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_4) : argCount = 4; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_5) : argCount = 5; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_6) : argCount = 6; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_7) : argCount = 7; goto do_invoke_indexed;
        OPCODE(BTL_OP_INVOKE_8) : argCount = 8; goto do_invoke_indexed;

    do_invoke_indexed: {
        uint8_t methodIndex = READ_BYTE();

        BtlValue receiver = peek(vm, argCount);
        if (!IS_INSTANCE(receiver)) {
            STORE_FRAME();
            btl_runtime_error(vm, "Only instances have methods.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        ObjInstance* instance = AS_INSTANCE(receiver);
        ObjClass* klass = instance->klass;

        if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
            STORE_FRAME();
            btl_runtime_error(vm, "Undefined method.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        BtlMethodEntry* entry = &klass->methods[methodIndex];

        STORE_FRAME();
        if (!call(vm, entry->closure, argCount)) {
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        REFRESH_FRAME();
        DISPATCH();
        }

    OPCODE(BTL_OP_INVOKE) : {
        uint8_t methodIndex = READ_BYTE();
        argCount = READ_BYTE();

        BtlValue receiver = peek(vm, argCount);
        if (!IS_INSTANCE(receiver)) {
            STORE_FRAME();
            btl_runtime_error(vm, "Only instances have methods.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        ObjInstance* instance = AS_INSTANCE(receiver);
        ObjClass* klass = instance->klass;

        if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
            STORE_FRAME();
            btl_runtime_error(vm, "Undefined method.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        BtlMethodEntry* entry = &klass->methods[methodIndex];

        STORE_FRAME();
        if (!call(vm, entry->closure, argCount)) {
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        REFRESH_FRAME();
        DISPATCH();
    }

    OPCODE(BTL_OP_INVOKE_LONG) : {
        uint16_t methodIndex = READ_SHORT();
        argCount = READ_BYTE();

        BtlValue receiver = peek(vm, argCount);
        if (!IS_INSTANCE(receiver)) {
            STORE_FRAME();
            btl_runtime_error(vm, "Only instances have methods.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        ObjInstance* instance = AS_INSTANCE(receiver);
        ObjClass* klass = instance->klass;

        if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
            STORE_FRAME();
            btl_runtime_error(vm, "Undefined method.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }

        BtlMethodEntry* entry = &klass->methods[methodIndex];

        STORE_FRAME();
        if (!call(vm, entry->closure, argCount)) {
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        REFRESH_FRAME();
        DISPATCH();
    }

    // IC-based invoke (name lookup with caching)
    OPCODE(BTL_OP_INVOKE_IC) : {
        uint8_t nameIdx = READ_BYTE();
        argCount = READ_BYTE();
        uint8_t icSlot = READ_BYTE();
        BtlValue receiver = peek(vm, argCount);
        // CHECK FOR ACTOR - handle async method call
        if (IS_ACTOR(receiver)) {
            ObjActor* actor = AS_ACTOR(receiver);
            ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            // Dead actor returns null
            if (!actor->alive) {
                for (int i = 0; i <= argCount; i++) btl_pop(vm);
                btl_push(vm, BTL_NULL_VAL);
                DISPATCH();
            }
            // Create future for result
            ObjFuture* future = btl_future_new(vm);
            // Collect args
            BtlValue* args = NULL;
            if (argCount > 0) {
                args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
                for (int i = 0; i < argCount; i++) {
                    args[i] = peek(vm, argCount - 1 - i);
                }
            }
            // Send message to actor
            btl_actor_send(actor, methodName, args, argCount, future);
            if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);
            // Pop args and receiver
            for (int i = 0; i <= argCount; i++) btl_pop(vm);
            // Push future as result
            btl_push(vm, OBJ_VAL(future));
            DISPATCH();
        }
        if (IS_INSTANCE(receiver)) {
            ObjInstance* instance = AS_INSTANCE(receiver);
            BtlMethodIC* ic = &frame->closure->methodICs[icSlot];

            // FAST PATH
            if (ic->cachedClass == instance->klass && ic->methodIndex >= 0) {
                BtlMethodEntry* entry = &instance->klass->methods[ic->methodIndex];

                if (argCount != entry->arity) {
                    STORE_FRAME();
                    btl_runtime_error(vm, "Expected %d arguments but got %d.", entry->arity, argCount);
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }

                STORE_FRAME();
                if (!call(vm, entry->closure, argCount)) {
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH();
            }

            // SLOW PATH
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

            // Check for callable field first
            BtlValue fieldIdx;
            if (btl_table_get(&instance->klass->fieldIndices, OBJ_VAL(name), &fieldIdx)) {
                int idx = (int) AS_NUMBER(fieldIdx);
                BtlValue field = instance->fields[idx];
                vm->stackTop[-argCount - 1] = field;
                STORE_FRAME();
                if (!btl_call_value(vm, field, argCount)) {
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH();
            }

            // Search methods by name AND arity (supports method overloading)
            {
                int foundIndex = -1;
                for (int i = 0; i < instance->klass->methodCount; i++) {
                    ObjString* methodName = instance->klass->methods[i].name;
                    if (instance->klass->methods[i].closure != NULL &&
                        methodName != NULL &&
                        methodName == name &&
                        instance->klass->methods[i].arity == argCount) {
                        foundIndex = i;
                        break;
                    }
                }

                if (foundIndex >= 0) {
                    ic->cachedClass = instance->klass;
                    ic->methodIndex = foundIndex;

                    STORE_FRAME();
                    if (!call(vm, instance->klass->methods[foundIndex].closure, argCount)) {
                        return BTL_INTERPRET_RUNTIME_ERROR;
                    }
                    REFRESH_FRAME();
                    DISPATCH();
                }
            }

            STORE_FRAME();
            btl_runtime_error(vm, "Undefined property '%s'.", name->chars);
            return BTL_INTERPRET_RUNTIME_ERROR;

        } else if (IS_MODULE(receiver)) {
            ObjModule* m = AS_MODULE(receiver);
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            BtlValue idx;

            if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
                BtlValue func = m->globalValues.values[(int) AS_NUMBER(idx)];
                vm->stackTop[-argCount - 1] = func;
                STORE_FRAME();
                if (!btl_call_value(vm, func, argCount)) {
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH();
            }

            if (name->length > 0) {
                ObjString* rawName = btl_string_copy(vm, name->chars, name->length - 1);
                btl_push(vm, OBJ_VAL(rawName));
                if (btl_table_get(&m->globalNames, OBJ_VAL(rawName), &idx)) {
                    btl_pop(vm);
                    BtlValue func = m->globalValues.values[(int) AS_NUMBER(idx)];
                    vm->stackTop[-argCount - 1] = func;
                    STORE_FRAME();
                    if (!btl_call_value(vm, func, argCount)) {
                        return BTL_INTERPRET_RUNTIME_ERROR;
                    }
                    REFRESH_FRAME();
                    DISPATCH();
                }
                btl_pop(vm);
            }

            STORE_FRAME();
            btl_runtime_error(vm, "Undefined function '%s' in module.", name->chars);
            return BTL_INTERPRET_RUNTIME_ERROR;
        } else if (IS_NATIVE_MODULE(receiver)) {
            ObjNativeModule* module = AS_NATIVE_MODULE(receiver);
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
            BtlValue func;
            if (btl_table_get(&module->globals, OBJ_VAL(name), &func)) {
                vm->stackTop[-argCount - 1] = func;
                STORE_FRAME();
                if (!btl_call_value(vm, func, argCount)) {
                    return BTL_INTERPRET_RUNTIME_ERROR;
                }
                REFRESH_FRAME();
                DISPATCH();
            }
            STORE_FRAME();
            btl_runtime_error(vm, "Undefined function '%s' in native module.", name->chars);
            return BTL_INTERPRET_RUNTIME_ERROR;
        } else {
            // Native method call (String, List, Table, Number)
            ObjNativeClass* nativeClass = getNativeClass(vm, receiver);
            if (nativeClass != NULL) {
                ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                ObjNativeMethod* method = findNativeMethod(nativeClass, name);
                // Int fallback: if method not found on intClass, try numberClass
                // Promote receiver to double so Number methods work correctly
                if (method == NULL && IS_INT(receiver)) {
                    method = findNativeMethod(vm->numberClass, name);
                    if (method != NULL) {
                        receiver = NUMBER_VAL((double)AS_INT(receiver));
                    }
                }
                if (method != NULL) {
                    // Check arity
                    if (method->arity >= 0 && argCount != method->arity) {
                        STORE_FRAME();
                        btl_runtime_error(vm, "Expected %d arguments but got %d.", method->arity, argCount);
                        return BTL_INTERPRET_RUNTIME_ERROR;
                    }

                    // Call native method
                    BtlValue* args = vm->stackTop - argCount;
                    BtlValue result = method->function(vm, receiver, argCount, args);

                    // Pop receiver and args, push result
                    vm->stackTop -= argCount + 1;
                    btl_push(vm, result);
                    DISPATCH();
                }
            }
        }

        STORE_FRAME();
        btl_runtime_error(vm, "Only instances and modules have methods.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    // Tail invoke opcodes (indexed)
    OPCODE(BTL_OP_TAIL_INVOKE_0) : argCount = 0; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_1) : argCount = 1; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_2) : argCount = 2; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_3) : argCount = 3; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_4) : argCount = 4; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_5) : argCount = 5; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_6) : argCount = 6; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_7) : argCount = 7; goto do_tail_invoke_indexed;
    OPCODE(BTL_OP_TAIL_INVOKE_8) : argCount = 8; goto do_tail_invoke_indexed;

do_tail_invoke_indexed: {
    uint8_t methodIndex = READ_BYTE();

    BtlValue receiver = peek(vm, argCount);
    if (!IS_INSTANCE(receiver)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Only instances have methods.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);
    ObjClass* klass = instance->klass;

    if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &klass->methods[methodIndex];
    ObjClosure* method = entry->closure;

    closeUpvalues(vm, frame);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue) * (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = method;
    frame->ip = method->function->chunk.code;
    frame->openUpvalues = NULL;
    ip = frame->ip;
    DISPATCH();
    }

OPCODE(BTL_OP_TAIL_INVOKE) : {
    uint8_t methodIndex = READ_BYTE();
    argCount = READ_BYTE();

    BtlValue receiver = peek(vm, argCount);
    if (!IS_INSTANCE(receiver)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Only instances have methods.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);
    ObjClass* klass = instance->klass;

    if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &klass->methods[methodIndex];
    ObjClosure* method = entry->closure;

    closeUpvalues(vm, frame);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = method;
    frame->ip = method->function->chunk.code;
    frame->openUpvalues = NULL;
    ip = frame->ip;
    DISPATCH();
}

OPCODE(BTL_OP_TAIL_INVOKE_LONG) : {
    uint16_t methodIndex = READ_SHORT();
    argCount = READ_BYTE();

    BtlValue receiver = peek(vm, argCount);
    if (!IS_INSTANCE(receiver)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Only instances have methods.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);
    ObjClass* klass = instance->klass;

    if (methodIndex >= klass->methodCount || klass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &klass->methods[methodIndex];
    ObjClosure* method = entry->closure;

    closeUpvalues(vm, frame);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = method;
    frame->ip = method->function->chunk.code;
    frame->openUpvalues = NULL;
    ip = frame->ip;
    DISPATCH();
}

// IC-based tail invoke
OPCODE(BTL_OP_TAIL_INVOKE_IC) : {
    uint8_t nameIdx = READ_BYTE();
    argCount = READ_BYTE();
    uint8_t icSlot = READ_BYTE();
    BtlValue receiver = peek(vm, argCount);
    // CHECK FOR ACTOR - handle async method call (no tail optimization for actors)
    if (IS_ACTOR(receiver)) {
        ObjActor* actor = AS_ACTOR(receiver);
        ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        if (!actor->alive) {
            for (int i = 0; i <= argCount; i++) btl_pop(vm);
            btl_push(vm, BTL_NULL_VAL);
            DISPATCH();
        }
        ObjFuture* future = btl_future_new(vm);
        BtlValue* args = NULL;
        if (argCount > 0) {
            args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                args[i] = peek(vm, argCount - 1 - i);
            }
        }
        btl_actor_send(actor, methodName, args, argCount, future);
        if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, OBJ_VAL(future));
        DISPATCH();
    }
    if (IS_INSTANCE(receiver)) {
        ObjInstance* instance = AS_INSTANCE(receiver);
        BtlMethodIC* ic = &frame->closure->methodICs[icSlot];
        ObjClosure* method = NULL;

        // FAST PATH
        if (ic->cachedClass == instance->klass && ic->methodIndex >= 0) {
            BtlMethodEntry* entry = &instance->klass->methods[ic->methodIndex];

            if (argCount != entry->arity) {
                STORE_FRAME();
                btl_runtime_error(vm, "Expected %d arguments but got %d.", entry->arity, argCount);
                return BTL_INTERPRET_RUNTIME_ERROR;
            }

            method = entry->closure;
        } else {
            // SLOW PATH
            ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);

            // Search methods by name AND arity (supports method overloading)
            for (int i = 0; i < instance->klass->methodCount; i++) {
                ObjString* methodName = instance->klass->methods[i].name;
                if (instance->klass->methods[i].closure != NULL &&
                    methodName != NULL &&
                    methodName == name &&
                    instance->klass->methods[i].arity == argCount) {

                    ic->cachedClass = instance->klass;
                    ic->methodIndex = i;
                    method = instance->klass->methods[i].closure;
                    break;
                }
            }

            if (method == NULL) {
                STORE_FRAME();
                ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
                btl_runtime_error(vm, "Undefined property '%s'.", name->chars);
                return BTL_INTERPRET_RUNTIME_ERROR;
            }
        }

        // Tail call
        closeUpvalues(vm, frame);
        memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue) * (argCount + 1));
        vm->stackTop = frame->slots + (argCount + 1);
        frame->closure = method;
        frame->ip = method->function->chunk.code;
        frame->openUpvalues = NULL;
        ip = frame->ip;
        DISPATCH();

    } else if (IS_MODULE(receiver)) {
        ObjModule* m = AS_MODULE(receiver);
        ObjString* name = AS_STRING(frame->closure->function->chunk.constants.values[nameIdx]);
        BtlValue idx;

        if (btl_table_get(&m->globalNames, OBJ_VAL(name), &idx)) {
            BtlValue func = m->globalValues.values[(int) AS_NUMBER(idx)];
            vm->stackTop[-argCount - 1] = func;
            STORE_FRAME();
            if (!btl_call_value(vm, func, argCount)) {
                return BTL_INTERPRET_RUNTIME_ERROR;
            }
            REFRESH_FRAME();
            DISPATCH();
        }

        STORE_FRAME();
        btl_runtime_error(vm, "Undefined function '%s' in module.", name->chars);
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    STORE_FRAME();
    btl_runtime_error(vm, "Only instances and modules have methods.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

// Super invoke opcodes (indexed)
OPCODE(BTL_OP_SUPER_INVOKE_0) : argCount = 0; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_1) : argCount = 1; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_2) : argCount = 2; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_3) : argCount = 3; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_4) : argCount = 4; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_5) : argCount = 5; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_6) : argCount = 6; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_7) : argCount = 7; goto do_super_invoke_indexed;
OPCODE(BTL_OP_SUPER_INVOKE_8) : argCount = 8; goto do_super_invoke_indexed;

do_super_invoke_indexed: {
uint8_t methodIndex = READ_BYTE();
ObjClass* superclass = AS_CLASS(btl_pop(vm));

if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
    STORE_FRAME();
    btl_runtime_error(vm, "Undefined method in superclass.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

BtlMethodEntry* entry = &superclass->methods[methodIndex];

STORE_FRAME();
if (!call(vm, entry->closure, argCount)) {
    return BTL_INTERPRET_RUNTIME_ERROR;
}
REFRESH_FRAME();
DISPATCH();
}

OPCODE(BTL_OP_SUPER_INVOKE) : {
    uint8_t methodIndex = READ_BYTE();
    argCount = READ_BYTE();
    ObjClass* superclass = AS_CLASS(btl_pop(vm));

    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method in superclass.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &superclass->methods[methodIndex];

    STORE_FRAME();
    if (!call(vm, entry->closure, argCount)) {
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    REFRESH_FRAME();
    DISPATCH();
}

OPCODE(BTL_OP_SUPER_INVOKE_LONG) : {
    uint16_t methodIndex = READ_SHORT();
    argCount = READ_BYTE();
    ObjClass* superclass = AS_CLASS(btl_pop(vm));

    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method in superclass.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &superclass->methods[methodIndex];

    STORE_FRAME();
    if (!call(vm, entry->closure, argCount)) {
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    REFRESH_FRAME();
    DISPATCH();
}

// Tail super invoke opcodes (indexed)
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_0) : argCount = 0; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_1) : argCount = 1; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_2) : argCount = 2; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_3) : argCount = 3; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_4) : argCount = 4; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_5) : argCount = 5; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_6) : argCount = 6; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_7) : argCount = 7; goto do_tail_super_invoke_indexed;
OPCODE(BTL_OP_TAIL_SUPER_INVOKE_8) : argCount = 8; goto do_tail_super_invoke_indexed;

do_tail_super_invoke_indexed: {
uint8_t methodIndex = READ_BYTE();
ObjClass* superclass = AS_CLASS(btl_pop(vm));

if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
    STORE_FRAME();
    btl_runtime_error(vm, "Undefined method in superclass.");
    return BTL_INTERPRET_RUNTIME_ERROR;
}

BtlMethodEntry* entry = &superclass->methods[methodIndex];
ObjClosure* method = entry->closure;

closeUpvalues(vm, frame);
memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue) * (argCount + 1));
vm->stackTop = frame->slots + (argCount + 1);
frame->closure = method;
frame->ip = method->function->chunk.code;
frame->openUpvalues = NULL;
ip = frame->ip;
DISPATCH();
}

OPCODE(BTL_OP_TAIL_SUPER_INVOKE) : {
    uint8_t methodIndex = READ_BYTE();
    argCount = READ_BYTE();
    ObjClass* superclass = AS_CLASS(btl_pop(vm));

    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method in superclass.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &superclass->methods[methodIndex];
    ObjClosure* method = entry->closure;

    closeUpvalues(vm, frame);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = method;
    frame->ip = method->function->chunk.code;
    frame->openUpvalues = NULL;
    ip = frame->ip;
    DISPATCH();
}

OPCODE(BTL_OP_TAIL_SUPER_INVOKE_LONG) : {
    uint16_t methodIndex = READ_SHORT();
    argCount = READ_BYTE();
    ObjClass* superclass = AS_CLASS(btl_pop(vm));

    if (methodIndex >= superclass->methodCount || superclass->methods[methodIndex].closure == NULL) {
        STORE_FRAME();
        btl_runtime_error(vm, "Undefined method in superclass.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    BtlMethodEntry* entry = &superclass->methods[methodIndex];
    ObjClosure* method = entry->closure;

    closeUpvalues(vm, frame);
    memmove(frame->slots, vm->stackTop - (argCount + 1), sizeof(BtlValue)* (argCount + 1));
    vm->stackTop = frame->slots + (argCount + 1);
    frame->closure = method;
    frame->ip = method->function->chunk.code;
    frame->openUpvalues = NULL;
    ip = frame->ip;
    DISPATCH();
}

OPCODE(BTL_OP_CLOSE_UPVALUE) : {
    closeUpvalues(vm, frame);
    btl_pop(vm);
    DISPATCH();
}

OPCODE(BTL_OP_CLASS) : {
    ObjString* name = READ_STRING();
    ObjClass* klass = btl_class_new(vm, name);

    BtlValue savedInfoValue;
    if (btl_table_get(&vm->rootModule->classInfo, OBJ_VAL(name), &savedInfoValue)) {
        BtlSavedClassInfo* savedInfo = (BtlSavedClassInfo*) (uintptr_t) AS_NUMBER(savedInfoValue);
        btl_table_add_all(vm, &savedInfo->methodIndices, &klass->methodIndices);
    }

    btl_push(vm, OBJ_VAL(klass));
    DISPATCH();
}

OPCODE(BTL_OP_CLASS_LONG) : {
    ObjString* name = READ_STRING_LONG();
    ObjClass* klass = btl_class_new(vm, name);

    BtlValue savedInfoValue;
    if (btl_table_get(&vm->rootModule->classInfo, OBJ_VAL(name), &savedInfoValue)) {
        BtlSavedClassInfo* savedInfo = (BtlSavedClassInfo*) (uintptr_t) AS_NUMBER(savedInfoValue);
        btl_table_add_all(vm, &savedInfo->methodIndices, &klass->methodIndices);
    }

    btl_push(vm, OBJ_VAL(klass));
    DISPATCH();
}

OPCODE(BTL_OP_INHERIT) : {
    BtlValue superclassVal = peek(vm, 1);
    if (!IS_CLASS(superclassVal)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Superclass must be a class.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    ObjClass* superclass = AS_CLASS(superclassVal);
    ObjClass* subclass = AS_CLASS(peek(vm, 0));

    if (superclass->methodCount > 0) {
        subclass->methodCapacity = superclass->methodCapacity;
        subclass->methodCount = superclass->methodCount;
        subclass->methods = BTL_ALLOCATE(vm, BtlMethodEntry, subclass->methodCapacity);
        memcpy(subclass->methods, superclass->methods, sizeof(BtlMethodEntry) * superclass->methodCount);
        btl_table_add_all(vm, &superclass->methodIndices, &subclass->methodIndices);
    }

    btl_table_add_all(vm, &superclass->fieldIndices, &subclass->fieldIndices);
    subclass->fieldCount = superclass->fieldCount;
    btl_pop(vm);
    btl_pop(vm);
    DISPATCH();
}

OPCODE(BTL_OP_METHOD) : {
    uint8_t methodIndex = READ_BYTE();
    uint8_t arity = READ_BYTE();
    ObjClosure* method = AS_CLOSURE(peek(vm, 0));
    ObjClass* klass = AS_CLASS(peek(vm, 1));

    if (methodIndex >= klass->methodCapacity) {
        growMethodTable(vm, klass, methodIndex);
    }

    klass->methods[methodIndex].closure = method;
    klass->methods[methodIndex].arity = arity;
    klass->methods[methodIndex].name = method->function->name;
    if (methodIndex >= klass->methodCount) {
        klass->methodCount = methodIndex + 1;
    }

    ObjString* name = method->function->name;
    int nameLen = name->length;
    char* buffer = BTL_ALLOCATE(vm, char, nameLen + 2);
    memcpy(buffer, name->chars, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = btl_string_copy(vm, buffer, nameLen + 1);
    BTL_FREE_ARRAY(vm, char, buffer, nameLen + 2);

    btl_push(vm, OBJ_VAL(signature));
    btl_table_set(vm, &klass->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    btl_pop(vm);
    btl_pop(vm);
    DISPATCH();
}
OPCODE(BTL_OP_METHOD_LONG) : {
    uint16_t methodIndex = READ_SHORT();
    uint8_t arity = READ_BYTE();
    ObjClosure* method = AS_CLOSURE(peek(vm, 0));
    ObjClass* klass = AS_CLASS(peek(vm, 1));
    if (methodIndex >= klass->methodCapacity) {
        growMethodTable(vm, klass, methodIndex);
    }

    klass->methods[methodIndex].closure = method;
    klass->methods[methodIndex].arity = arity;
    klass->methods[methodIndex].name = method->function->name;

    if (methodIndex >= klass->methodCount) {
        klass->methodCount = methodIndex + 1;
    }

    ObjString* name = method->function->name;
    int nameLen = name->length;
    char* buffer = BTL_ALLOCATE(vm, char, nameLen + 2);
    memcpy(buffer, name->chars, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = btl_string_copy(vm, buffer, nameLen + 1);
    BTL_FREE_ARRAY(vm, char, buffer, nameLen + 2);

    btl_push(vm, OBJ_VAL(signature));
    btl_table_set(vm, &klass->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    btl_pop(vm);
    btl_pop(vm);
    DISPATCH();
}
OPCODE(BTL_OP_BUILD_LIST) : {
    uint8_t count = READ_BYTE();
    ObjList* l = btl_list_new(vm);
    btl_push(vm, OBJ_VAL(l));
    for (int i = 0; i < count; i++)
        btl_value_array_write(vm, &l->items, peek(vm, count - i));
    vm->stackTop -= (count + 1);
    btl_push(vm, OBJ_VAL(l));
    DISPATCH();
}
OPCODE(BTL_OP_BUILD_TABLE) : {
    uint8_t count = READ_BYTE();
    ObjTable* table = btl_table_obj_new(vm);
    btl_push(vm, OBJ_VAL(table));
    BtlValue* pairs = vm->stackTop - (count * 2) - 1;
    for (int i = 0; i < count; i++) {
        BtlValue key = pairs[i * 2];
        BtlValue value = pairs[i * 2 + 1];
        btl_table_set(vm, &table->table, key, value);
    }

    vm->stackTop -= (count * 2 + 1);
    btl_push(vm, OBJ_VAL(table));
    DISPATCH();
}
OPCODE(BTL_OP_INDEX_GET) : {
    BtlValue key = btl_pop(vm);
    BtlValue obj = btl_pop(vm);
    if (IS_LIST(obj)) {
        if (!IS_NUMERIC(key)) {
            STORE_FRAME();
            btl_runtime_error(vm, "List index must be a number.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx >= l->items.count) {
            STORE_FRAME();
            btl_runtime_error(vm, "List index out of bounds.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        btl_push(vm, l->items.values[idx]);
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        BtlValue value;
        if (!btl_table_get(&table->table, key, &value)) {
            btl_push(vm, BTL_NULL_VAL);
        } else {
            btl_push(vm, value);
        }
    } else if (IS_STRING(obj)) {
        if (!IS_NUMERIC(key)) {
            STORE_FRAME();
            btl_runtime_error(vm, "String index must be a number.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        ObjString* str = AS_STRING(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx >= str->length) {
            STORE_FRAME();
            btl_runtime_error(vm, "String index out of bounds.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        char c = str->chars[idx];
        ObjString* result = btl_string_copy(vm, &c, 1);
        btl_push(vm, OBJ_VAL(result));
    } else {
        STORE_FRAME();
        btl_runtime_error(vm, "Only lists, tables, and strings can be indexed.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    DISPATCH();
}
OPCODE(BTL_OP_INDEX_SET) : {
    BtlValue value = btl_pop(vm);
    BtlValue key = btl_pop(vm);
    BtlValue obj = btl_pop(vm);
    if (IS_LIST(obj)) {
        if (!IS_NUMERIC(key)) {
            STORE_FRAME();
            btl_runtime_error(vm, "List index must be a number.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        ObjList* l = AS_LIST(obj);
        int idx = (int) btl_numeric_to_double(key);
        if (idx < 0 || idx > l->items.count) {
            STORE_FRAME();
            btl_runtime_error(vm, "List index out of bounds.");
            return BTL_INTERPRET_RUNTIME_ERROR;
        }
        if (idx == l->items.count) {
            btl_value_array_write(vm, &l->items, value);
        } else {
            l->items.values[idx] = value;
            btl_gc_write_barrier(vm, (BtlObj*) l, value);
        }
        btl_push(vm, value);
    } else if (IS_TABLE(obj)) {
        ObjTable* table = AS_TABLE(obj);
        btl_table_set(vm, &table->table, key, value);
        btl_gc_write_barrier(vm, (BtlObj*) table, value);
        btl_gc_write_barrier(vm, (BtlObj*) table, key);
        btl_push(vm, value);
    } else if (IS_STRING(obj)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Strings are immutable.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    } else {
        STORE_FRAME();
        btl_runtime_error(vm, "Only lists and tables can be indexed for assignment.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    DISPATCH();
}
OPCODE(BTL_OP_IMPORT) : {
    ObjString* fName = READ_STRING();
    // Check native modules first
    BtlValue nativeModule;
    if (btl_table_get(&vm->nativeModules, OBJ_VAL(fName), &nativeModule)) {
        btl_push(vm, nativeModule);
        DISPATCH();
    }
    BtlValue mVal;
    if (btl_table_get(&vm->modules, OBJ_VAL(fName), &mVal)) {
        btl_push(vm, mVal); DISPATCH();
    }
    char* src = readFile(vm, fName->chars);
    if (!src) {
        btl_runtime_error(vm, "Could not open file \"%s\".", fName->chars); return BTL_INTERPRET_RUNTIME_ERROR;
    }
    size_t srcLen = strlen(src);  // Track size for deallocation
    STORE_FRAME();
    ObjModule* m = btl_module_new(vm, fName);
    ObjFunction* f = btl_compile(vm, m, src);
    btl_realloc(vm, src, srcLen + 1, 0);  // Free source with correct size
    if (!f) return BTL_INTERPRET_RUNTIME_ERROR;
    ObjClosure* c = btl_closure_new(vm, f);
    btl_push(vm, OBJ_VAL(c));
    if (!call(vm, c, 0)) return BTL_INTERPRET_RUNTIME_ERROR;
    vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
    btl_table_set(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));
    REFRESH_FRAME();
    DISPATCH();
}
OPCODE(BTL_OP_IMPORT_LONG) : {
    ObjString* fName = READ_STRING_LONG();
    // Check native modules first
    BtlValue nativeModule;
    if (btl_table_get(&vm->nativeModules, OBJ_VAL(fName), &nativeModule)) {
        btl_push(vm, nativeModule);
        DISPATCH();
    }
    BtlValue mVal;
    if (btl_table_get(&vm->modules, OBJ_VAL(fName), &mVal)) {
        btl_push(vm, mVal); DISPATCH();
    }
    char* src = readFile(vm, fName->chars);
    if (!src) {
        btl_runtime_error(vm, "Could not open file \"%s\".", fName->chars); return BTL_INTERPRET_RUNTIME_ERROR;
    }
    size_t srcLen = strlen(src);  // Track size for deallocation
    STORE_FRAME();
    ObjModule* m = btl_module_new(vm, fName);
    ObjFunction* f = btl_compile(vm, m, src);
    btl_realloc(vm, src, srcLen + 1, 0);  // Free source with correct size
    if (!f) return BTL_INTERPRET_RUNTIME_ERROR;
    ObjClosure* c = btl_closure_new(vm, f);
    btl_push(vm, OBJ_VAL(c));
    if (!call(vm, c, 0)) return BTL_INTERPRET_RUNTIME_ERROR;
    vm->frames[vm->frameCount - 1].slots[0] = OBJ_VAL(m);
    btl_table_set(vm, &vm->modules, OBJ_VAL(fName), OBJ_VAL(m));
    REFRESH_FRAME();
    DISPATCH();
}
OPCODE(BTL_OP_DO_NEW) : {
    uint8_t argCount = READ_BYTE();
    BtlValue callee = peek(vm, argCount);

    if (IS_CLASS(callee)) {
        // Create actor from class
        ObjClass* klass = AS_CLASS(callee);

        BtlValue* args = NULL;
        if (argCount > 0) {
            args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                args[i] = peek(vm, argCount - 1 - i);
            }
        }

        for (int i = 0; i <= argCount; i++) btl_pop(vm);

        ObjActor* actor = btl_actor_new(vm, klass, args, argCount);

        if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue) * argCount, 0);

        btl_push(vm, OBJ_VAL(actor));

    } else if (IS_CLOSURE(callee)) {
        ObjClosure* closure = AS_CLOSURE(callee);
        ObjFuture* future = btl_future_new(vm);

        // Allocate async VM using runtime allocator
        VM* asyncVM = btl_realloc(vm, NULL, 0, sizeof(VM));
        memset(asyncVM, 0, sizeof(VM));  // Zero-initialize before initVM

        // CRITICAL: Share parent's runtime with async VM
        asyncVM->runtime = vm->runtime;

        btl_vm_init(asyncVM);

        // Copy native classes
        asyncVM->stringClass = vm->stringClass;
        asyncVM->numberClass = vm->numberClass;
        asyncVM->intClass = vm->intClass;
        asyncVM->listClass = vm->listClass;
        asyncVM->tableClass = vm->tableClass;
        asyncVM->entityClass = vm->entityClass;
        asyncVM->rootModule = closure->function->module;

        AsyncCallTask* task = btl_realloc(vm, NULL, 0, sizeof(AsyncCallTask));
        task->vm = asyncVM;
        task->closure = closure;
        task->argCount = argCount;
        task->future = future;

        if (argCount > 0) {
            task->args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
            for (int i = 0; i < argCount; i++) {
                task->args[i] = btl_deep_copy_value(asyncVM, vm, peek(vm, argCount - 1 - i));
            }
        } else {
            task->args = NULL;
        }

        for (int i = 0; i <= argCount; i++) btl_pop(vm);

        btl_threadpool_submit(vm->runtime->pool, asyncCallTaskRun, task);

        btl_push(vm, OBJ_VAL(future));
    } else {
        STORE_FRAME();
        btl_runtime_error(vm, "Can only use 'do' with classes or functions.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    DISPATCH();
}
OPCODE(BTL_OP_DO_INVOKE) : {
    uint8_t nameConstant = READ_BYTE();
    uint8_t argCount = READ_BYTE();
    ObjString* methodName = AS_STRING(frame->closure->function->chunk.constants.values[nameConstant]);

    BtlValue actorVal = peek(vm, argCount);

    if (IS_NULL(actorVal)) {
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, BTL_NULL_VAL);
        DISPATCH();
    }

    if (!IS_ACTOR(actorVal)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Expected actor for 'do' method call.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }

    ObjActor* actor = AS_ACTOR(actorVal);

    if (!actor->alive) {
        for (int i = 0; i <= argCount; i++) btl_pop(vm);
        btl_push(vm, BTL_NULL_VAL);
        DISPATCH();
    }

    ObjFuture* future = btl_future_new(vm);

    BtlValue* args = NULL;
    if (argCount > 0) {
        args = btl_realloc(vm, NULL, 0, sizeof(BtlValue) * argCount);
        for (int i = 0; i < argCount; i++) {
            args[i] = peek(vm, argCount - 1 - i);
        }
    }

    btl_actor_send(actor, methodName, args, argCount, future);

    if (args != NULL) btl_realloc(vm, args, sizeof(BtlValue)* argCount, 0);

    for (int i = 0; i <= argCount; i++) btl_pop(vm);

    btl_push(vm, OBJ_VAL(future));
    DISPATCH();
}
OPCODE(BTL_OP_ITER_INIT) : {
    // Stack: [..., collection]
    // Validates collection is a list or table, pushes index 0
    BtlValue collection = peek(vm, 0);
    if (!IS_LIST(collection) && !IS_TABLE(collection)) {
        STORE_FRAME();
        btl_runtime_error(vm, "Can only iterate over lists and tables.");
        return BTL_INTERPRET_RUNTIME_ERROR;
    }
    btl_push(vm, INT_VAL(0));  // push index 0
    // Stack: [..., collection, 0]
    DISPATCH();
}
OPCODE(BTL_OP_ITER_NEXT) : {
    // Operands: [slot:8][offset:16]
    // Stack: [..., loop_var, collection, index]
    uint8_t slot = READ_BYTE();
    uint16_t offset = READ_SHORT();
    BtlValue indexVal = peek(vm, 0);    // index
    BtlValue collection = peek(vm, 1);  // collection
    int index = (int) AS_INT(indexVal);

    if (IS_LIST(collection)) {
        ObjList* list = AS_LIST(collection);
        if (index >= list->items.count) {
            // Done iterating - jump to exit
            ip += offset;
            DISPATCH();
        }
        // Set loop variable to current element
        frame->slots[slot] = list->items.values[index];
        // Increment index
        vm->stackTop[-1] = INT_VAL(index + 1);
    } else {
        // Table iteration: find next non-empty entry
        ObjTable* table = AS_TABLE(collection);
        while (index < table->table.capacity) {
            BtlEntry* entry = &table->table.entries[index];
            if (!IS_EMPTY(entry->key)) {
                // Found a valid entry - set loop variable to the key
                frame->slots[slot] = entry->key;
                // Increment index past this entry
                vm->stackTop[-1] = INT_VAL(index + 1);
                DISPATCH();
            }
            index++;
        }
        // No more entries - jump to exit
        ip += offset;
    }
    DISPATCH();
}
#ifndef BTL_HAS_COMPUTED_GOTOS
        }
    }
#endif
}
BtlInterpretResult btl_interpret(VM* vm, ObjModule* m, const char* src) {
    ObjFunction* f = btl_compile(vm, m, src); if (f == NULL) return BTL_INTERPRET_COMPILE_ERROR;
    btl_push(vm, OBJ_VAL(f));
    ObjClosure* c = btl_closure_new(vm, f);
    btl_pop(vm); btl_push(vm, OBJ_VAL(c));
    call(vm, c, 0); return btl_run(vm);
}

// ============================================================================
// Native GC Roots
// ============================================================================

int btl_gc_add_root(VM* vm, BtlValue value) {
    if (vm->nativeRootCount >= BTL_MAX_NATIVE_ROOTS) {
        fprintf(stderr, "[BTL] Warning: Max native GC roots (%d) reached\n", BTL_MAX_NATIVE_ROOTS);
        return -1;
    }
    int index = vm->nativeRootCount++;
    vm->nativeRoots[index] = value;
    return index;
}

void btl_gc_remove_root(VM* vm, int index) {
    if (index < 0 || index >= vm->nativeRootCount) return;
    vm->nativeRoots[index] = BTL_NULL_VAL;
}

void btl_gc_clear_roots(VM* vm) {
    for (int i = 0; i < vm->nativeRootCount; i++) {
        vm->nativeRoots[i] = BTL_NULL_VAL;
    }
    vm->nativeRootCount = 0;
}