#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>

#include "object.h"
#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

// Object allocation macro - uses allocateObject from memory.c
#define ALLOCATE_OBJ(vm, type, objectType) \
    (type*)allocateObject(vm, sizeof(type), objectType)

// ============== FUTURE ==============

ObjFuture* newFuture(VM* vm) {
    ObjFuture* future = ALLOCATE_OBJ(vm, ObjFuture, OBJ_FUTURE);
    future->state = FUTURE_PENDING;
    future->result = NULL_VAL;
    pthread_mutex_init(&future->mutex, NULL);
    pthread_cond_init(&future->cond, NULL);
    return future;
}

void futureResolve(ObjFuture* future, Value result) {
    pthread_mutex_lock(&future->mutex);
    future->result = result;
    future->state = FUTURE_READY;
    pthread_cond_broadcast(&future->cond);
    pthread_mutex_unlock(&future->mutex);
}

void futureReject(ObjFuture* future, Value error) {
    pthread_mutex_lock(&future->mutex);
    future->result = error;
    future->state = FUTURE_ERROR;
    pthread_cond_broadcast(&future->cond);
    pthread_mutex_unlock(&future->mutex);
}

Value futureGet(ObjFuture* future) {
    pthread_mutex_lock(&future->mutex);
    while (future->state == FUTURE_PENDING) {
        pthread_cond_wait(&future->cond, &future->mutex);
    }
    Value result = future->result;
    pthread_mutex_unlock(&future->mutex);
    return result;
}

FutureState futureGetState(ObjFuture* future) {
    pthread_mutex_lock(&future->mutex);
    FutureState state = future->state;
    pthread_mutex_unlock(&future->mutex);
    return state;
}

void freeFuture(ObjFuture* future) {
    pthread_mutex_destroy(&future->mutex);
    pthread_cond_destroy(&future->cond);
}

// ============== MESSAGE QUEUE ==============

#define MESSAGE_QUEUE_INITIAL_CAPACITY 16

void messageQueueInit(MessageQueue* queue) {
    queue->capacity = MESSAGE_QUEUE_INITIAL_CAPACITY;
    queue->messages = malloc(sizeof(ActorMessage) * queue->capacity);
    queue->count = 0;
    queue->head = 0;
    queue->tail = 0;
    pthread_mutex_init(&queue->mutex, NULL);
    pthread_cond_init(&queue->cond, NULL);
}

void messageQueueFree(MessageQueue* queue) {
    while (queue->count > 0) {
        ActorMessage* msg = &queue->messages[queue->head];
        if (msg->args != NULL) free(msg->args);
        queue->head = (queue->head + 1) % queue->capacity;
        queue->count--;
    }
    free(queue->messages);
    pthread_mutex_destroy(&queue->mutex);
    pthread_cond_destroy(&queue->cond);
}

void messageQueuePush(MessageQueue* queue, ActorMessage message) {
    pthread_mutex_lock(&queue->mutex);

    if (queue->count == queue->capacity) {
        int newCapacity = queue->capacity * 2;
        ActorMessage* newMessages = malloc(sizeof(ActorMessage) * newCapacity);
        for (int i = 0; i < queue->count; i++) {
            int idx = (queue->head + i) % queue->capacity;
            newMessages[i] = queue->messages[idx];
        }
        free(queue->messages);
        queue->messages = newMessages;
        queue->capacity = newCapacity;
        queue->head = 0;
        queue->tail = queue->count;
    }

    queue->messages[queue->tail] = message;
    queue->tail = (queue->tail + 1) % queue->capacity;
    queue->count++;

    pthread_cond_signal(&queue->cond);
    pthread_mutex_unlock(&queue->mutex);
}

bool messageQueuePop(MessageQueue* queue, ActorMessage* out, int timeoutMs) {
    pthread_mutex_lock(&queue->mutex);

    if (timeoutMs < 0) {
        while (queue->count == 0) {
            pthread_cond_wait(&queue->cond, &queue->mutex);
        }
    } else if (timeoutMs == 0) {
        if (queue->count == 0) {
            pthread_mutex_unlock(&queue->mutex);
            return false;
        }
    } else {
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_sec += timeoutMs / 1000;
        ts.tv_nsec += (timeoutMs % 1000) * 1000000;
        if (ts.tv_nsec >= 1000000000) {
            ts.tv_sec++;
            ts.tv_nsec -= 1000000000;
        }
        while (queue->count == 0) {
            int rc = pthread_cond_timedwait(&queue->cond, &queue->mutex, &ts);
            if (rc != 0) {
                pthread_mutex_unlock(&queue->mutex);
                return false;
            }
        }
    }

    *out = queue->messages[queue->head];
    queue->head = (queue->head + 1) % queue->capacity;
    queue->count--;

    pthread_mutex_unlock(&queue->mutex);
    return true;
}

// ============== DEEP COPY ==============

Value deepCopyValue(VM* destVM, VM* srcVM, Value value) {
    (void) srcVM;  // May be NULL

    if (IS_NUMBER(value) || IS_BOOL(value) || IS_NULL(value)) {
        return value;
    }

    if (IS_STRING(value)) {
        ObjString* src = AS_STRING(value);
        return OBJ_VAL(copyString(destVM, src->chars, src->length));
    }

    if (IS_LIST(value)) {
        ObjList* src = AS_LIST(value);
        ObjList* dest = newList(destVM);
        push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->items.count; i++) {
            Value copied = deepCopyValue(destVM, srcVM, src->items.values[i]);
            writeValueArray(destVM, &dest->items, copied);
        }
        pop(destVM);
        return OBJ_VAL(dest);
    }

    if (IS_TABLE(value)) {
        ObjTable* src = AS_TABLE(value);
        ObjTable* dest = newTable(destVM);
        push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->table.capacity; i++) {
            Entry* entry = &src->table.entries[i];
            if (!IS_EMPTY(entry->key)) {
                Value keyCopy = deepCopyValue(destVM, srcVM, entry->key);
                Value valCopy = deepCopyValue(destVM, srcVM, entry->value);
                tableSet(destVM, &dest->table, keyCopy, valCopy);
            }
        }
        pop(destVM);
        return OBJ_VAL(dest);
    }

    if (IS_ACTOR(value) || IS_FUTURE(value)) {
        return value;  // Pass by reference - safe to share
    }

    if (IS_INSTANCE(value)) {
        ObjInstance* src = AS_INSTANCE(value);
        ObjInstance* dest = newInstance(destVM, src->klass);
        push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->klass->fieldCount; i++) {
            dest->fields[i] = deepCopyValue(destVM, srcVM, src->fields[i]);
        }
        pop(destVM);
        return OBJ_VAL(dest);
    }

    return NULL_VAL;
}

// ============== ACTOR ==============

// Forward declare - this will be in vm.c
extern InterpretResult runVM(VM* vm);

static void* actorThreadMain(void* arg) {
    ObjActor* actor = (ObjActor*) arg;
    VM* vm = actor->vm;

    while (!actor->shouldStop) {
        ActorMessage msg;

        if (!messageQueuePop(&actor->inbox, &msg, 100)) {
            continue;
        }

        // Find method by name and arity
        ObjClosure* method = NULL;
        for (int i = 0; i < actor->klass->methodCount; i++) {
            MethodEntry* entry = &actor->klass->methods[i];
            if (entry->closure != NULL && entry->name != NULL &&
                entry->name->length == msg.method->length &&
                memcmp(entry->name->chars, msg.method->chars, msg.method->length) == 0 &&
                entry->arity == msg.argCount) {
                method = entry->closure;
                break;
            }
        }

        if (method == NULL) {
            if (msg.future != NULL) {
                ObjString* errMsg = copyString(vm, "Method not found", 16);
                futureReject(msg.future, OBJ_VAL(errMsg));
            }
            if (msg.args != NULL) free(msg.args);
            continue;
        }

        // Reset VM stack
        vm->stackTop = vm->stack;
        vm->frameCount = 0;

        // Set the module so globals can be resolved
        vm->rootModule = method->function->module;

        // Push a dummy value for the "script" slot that OP_RETURN will pop
        push(vm, NULL_VAL);

        // Push receiver and args
        push(vm, actor->instance);
        for (int i = 0; i < msg.argCount; i++) {
            push(vm, msg.args[i]);
        }

        // Call method
        CallFrame* frame = &vm->frames[vm->frameCount++];
        frame->closure = method;
        frame->ip = method->function->chunk.code;
        frame->slots = vm->stack + 1;  // Skip the dummy slot
        frame->openUpvalues = NULL;

        InterpretResult result = runVM(vm);

        if (result == INTERPRET_OK) {
            if (msg.future != NULL) {
                futureResolve(msg.future, vm->lastReturnValue);
            }
        } else {
            if (msg.future != NULL) {
                ObjString* errMsg = copyString(vm, "Actor method failed", 19);
                futureReject(msg.future, OBJ_VAL(errMsg));
            }
        }

        if (msg.args != NULL) free(msg.args);
    }

    actor->alive = false;
    return NULL;
}

ObjActor* newActor(VM* parentVM, ObjClass* klass, Value* args, int argCount) {
    ObjActor* actor = ALLOCATE_OBJ(parentVM, ObjActor, OBJ_ACTOR);

    actor->vm = malloc(sizeof(VM));
    initVM(actor->vm);

    // Share native classes from parent
    actor->vm->stringClass = parentVM->stringClass;
    actor->vm->numberClass = parentVM->numberClass;
    actor->vm->listClass = parentVM->listClass;
    actor->vm->tableClass = parentVM->tableClass;

    // Share the module (contains globals, class definitions)
    actor->vm->rootModule = parentVM->rootModule;

    actor->klass = klass;

    // Create instance in actor's VM
    ObjInstance* instance = newInstance(actor->vm, klass);
    actor->instance = OBJ_VAL(instance);
    push(actor->vm, actor->instance);

    messageQueueInit(&actor->inbox);
    actor->alive = true;
    actor->shouldStop = false;

    // Find init method with matching arity
    ObjClosure* initMethod = NULL;
    for (int i = 0; i < klass->methodCount; i++) {
        MethodEntry* entry = &klass->methods[i];
        if (entry->closure != NULL && entry->name != NULL &&
            entry->name->length == 4 &&
            memcmp(entry->name->chars, "init", 4) == 0 &&
            entry->arity == argCount) {
            initMethod = entry->closure;
            break;
        }
    }

    if (initMethod != NULL) {
        // Copy args to actor's VM
        for (int i = 0; i < argCount; i++) {
            Value copied = deepCopyValue(actor->vm, parentVM, args[i]);
            push(actor->vm, copied);
        }

        // Call init synchronously
        CallFrame* frame = &actor->vm->frames[actor->vm->frameCount++];
        frame->closure = initMethod;
        frame->ip = initMethod->function->chunk.code;
        frame->slots = actor->vm->stack;
        frame->openUpvalues = NULL;

        runVM(actor->vm);

        // Reset stack after init
        actor->vm->stackTop = actor->vm->stack;
        actor->vm->frameCount = 0;
        push(actor->vm, actor->instance);
    }

    // Start thread
    pthread_create(&actor->thread, NULL, actorThreadMain, actor);

    return actor;
}

void actorSend(ObjActor* actor, ObjString* method, Value* args, int argCount, ObjFuture* future) {
    if (!actor->alive) {
        if (future != NULL) {
            futureReject(future, OBJ_VAL(copyString(actor->vm, "Actor is dead", 13)));
        }
        return;
    }

    ActorMessage msg;
    msg.method = copyString(actor->vm, method->chars, method->length);
    msg.future = future;
    msg.argCount = argCount;

    if (argCount > 0) {
        msg.args = malloc(sizeof(Value) * argCount);
        for (int i = 0; i < argCount; i++) {
            msg.args[i] = deepCopyValue(actor->vm, NULL, args[i]);
        }
    } else {
        msg.args = NULL;
    }

    messageQueuePush(&actor->inbox, msg);
}

void actorStop(ObjActor* actor) {
    actor->shouldStop = true;
    pthread_join(actor->thread, NULL);
    messageQueueFree(&actor->inbox);
    freeVM(actor->vm, false);
    free(actor->vm);
}

// ============== NATIVE METHODS/CLASSES/MODULES ==============

ObjNativeMethod* newNativeMethod(VM* vm, NativeMethodFn fn, const char* name, int arity) {
    ObjNativeMethod* method = ALLOCATE_OBJ(vm, ObjNativeMethod, OBJ_NATIVE_METHOD);
    method->function = fn;
    method->name = copyString(vm, name, (int) strlen(name));
    method->arity = arity;
    return method;
}

ObjNativeClass* newNativeClass(VM* vm, const char* name) {
    ObjNativeClass* klass = ALLOCATE_OBJ(vm, ObjNativeClass, OBJ_NATIVE_CLASS);
    klass->name = copyString(vm, name, (int) strlen(name));
    initTable(&klass->methods);
    return klass;
}

ObjNativeModule* newNativeModule(VM* vm, const char* name) {
    ObjNativeModule* module = ALLOCATE_OBJ(vm, ObjNativeModule, OBJ_NATIVE_MODULE);
    module->name = copyString(vm, name, (int) strlen(name));
    initTable(&module->globals);
    return module;
}

void defineNativeMethod(VM* vm, ObjNativeClass* klass, const char* name, NativeMethodFn fn, int arity) {
    ObjNativeMethod* method = newNativeMethod(vm, fn, name, arity);
    push(vm, OBJ_VAL(method));
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &klass->methods, OBJ_VAL(nameStr), OBJ_VAL(method));
    pop(vm);
    pop(vm);
}

void defineNativeModuleValue(VM* vm, ObjNativeModule* module, const char* name, Value value) {
    push(vm, value);
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &module->globals, OBJ_VAL(nameStr), value);
    pop(vm);
    pop(vm);
}

void defineNativeModuleFn(VM* vm, ObjNativeModule* module, const char* name, NativeFn fn, int arity) {
    (void) arity;
    ObjNative* native = newNative(vm, fn);
    push(vm, OBJ_VAL(native));
    ObjString* nameStr = copyString(vm, name, (int) strlen(name));
    push(vm, OBJ_VAL(nameStr));
    tableSet(vm, &module->globals, OBJ_VAL(nameStr), OBJ_VAL(native));
    pop(vm);
    pop(vm);
}

// ============== CORE OBJECT TYPES ==============

ObjTable* newTable(struct VM* vm) {
    ObjTable* table = ALLOCATE_OBJ(vm, ObjTable, OBJ_TABLE);
    initTable(&table->table);
    return table;
}

ObjBoundMethod* newBoundMethod(struct VM* vm, Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(vm, ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass* newClass(struct VM* vm, struct ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(vm, ObjClass, OBJ_CLASS);
    klass->name = name;

    // OPTIMIZED: Initialize vtable
    klass->methods = NULL;
    klass->methodCount = 0;
    klass->methodCapacity = 0;

    // Method indices for compile-time lookup
    initTable(&klass->methodIndices);

    // Field system
    klass->fieldCount = 0;
    initTable(&klass->fieldIndices);

    return klass;
}

ObjClosure* newClosure(VM* vm, ObjFunction* function) {
    gcInhibitStart(vm);  // Protect during construction

    size_t size = sizeof(ObjClosure) + sizeof(RuntimeUpvalue) * function->upvalueCount;
    ObjClosure* closure = (ObjClosure*) allocateObject(vm, size, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalueCount = function->upvalueCount;

    for (int i = 0; i < function->upvalueCount; i++) {
        closure->upvalues[i].isOpen = true;
        closure->upvalues[i].next = NULL;
    }

    // Allocate and initialize IC arrays
    if (function->fieldICCount > 0) {
        closure->fieldICs = ALLOCATE(vm, FieldIC, function->fieldICCount);
        initFieldICs(closure->fieldICs, function->fieldICCount);
    } else {
        closure->fieldICs = NULL;
    }

    if (function->methodICCount > 0) {
        closure->methodICs = ALLOCATE(vm, MethodIC, function->methodICCount);
        initMethodICs(closure->methodICs, function->methodICCount);
    } else {
        closure->methodICs = NULL;
    }

    gcInhibitEnd(vm);  // Safe now
    return closure;
}

ObjFunction* newFunction(struct VM* vm, ObjModule* module) {
    ObjFunction* function = ALLOCATE_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    function->module = module;
    function->fieldICCount = 0;
    function->methodICCount = 0;
    initChunk(&function->chunk);
    return function;
}

ObjInstance* newInstance(struct VM* vm, ObjClass* klass) {
    gcInhibitStart(vm);  // Protect during field allocation

    ObjInstance* instance = ALLOCATE_OBJ(vm, ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;

    // Safety: always allocate at least 1 byte if count is 0 
    // to prevent malloc(0) implementation-defined behavior
    int size = klass->fieldCount > 0 ? klass->fieldCount : 1;
    instance->fields = ALLOCATE(vm, Value, size);

    for (int i = 0; i < klass->fieldCount; i++) {
        instance->fields[i] = NULL_VAL;
    }

    gcInhibitEnd(vm);
    return instance;
}

ObjList* newList(struct VM* vm) {
    ObjList* list = ALLOCATE_OBJ(vm, ObjList, OBJ_LIST);
    initValueArray(&list->items);
    return list;
}

ObjModule* newModule(struct VM* vm, struct ObjString* name) {
    ObjModule* module = ALLOCATE_OBJ(vm, ObjModule, OBJ_MODULE);
    module->name = name;
    initTable(&module->globalNames);
    initValueArray(&module->globalValues);
    initTable(&module->classInfo);
    return module;
}

ObjNative* newNative(struct VM* vm, NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

// ============== STRING INTERNING ==============

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t) key[i];
        hash *= 16777619;
    }
    return hash;
}

static struct ObjString* allocateString(struct VM* vm, char* chars, int length, uint32_t hash) {
    gcInhibitStart(vm);  // Protect: allocate string, then add to table
    struct ObjString* string = (struct ObjString*) allocateObject(vm, sizeof(struct ObjString), OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    push(vm, OBJ_VAL(string));
    tableSet(vm, &vm->strings, OBJ_VAL(string), NULL_VAL);
    pop(vm);
    gcInhibitEnd(vm);
    return string;
}

struct ObjString* takeString(struct VM* vm, char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(vm, char, chars, length + 1);
        return interned;
    }
    return allocateString(vm, chars, length, hash);
}

struct ObjString* copyString(struct VM* vm, const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    struct ObjString* interned = tableFindString(&vm->strings, chars, length, hash);
    if (interned != NULL) return interned;

    gcInhibitStart(vm);  // Protect: allocate chars, then string object
    char* heapChars = ALLOCATE(vm, char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    struct ObjString* result = allocateString(vm, heapChars, length, hash);
    gcInhibitEnd(vm);
    return result;
}

// ============== UPVALUES ==============

ObjUpvalue* newUpvalueBox(VM* vm, Value value) {
    ObjUpvalue* box = ALLOCATE_OBJ(vm, ObjUpvalue, OBJ_UPVALUE);
    box->closed = value;
    return box;
}

// ============== PRINT OBJECT ==============

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_BOUND_METHOD:
        printf("<func %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
        break;
    case OBJ_CLASS:
        printf("<class %s>", AS_CLASS(value)->name->chars);
        break;
    case OBJ_CLOSURE:
        printf("<func %s>", AS_CLOSURE(value)->function->name->chars);
        break;
    case OBJ_FUNCTION:
        printf("<func %s>", AS_FUNCTION(value)->name->chars);
        break;
    case OBJ_INSTANCE:
        printf("<%s instance>", AS_INSTANCE(value)->klass->name->chars);
        break;
    case OBJ_LIST: {
        ObjList* list = AS_LIST(value);
        printf("[");
        for (int i = 0; i < list->items.count; i++) {
            printValue(list->items.values[i]);
            if (i < list->items.count - 1) printf(", ");
        }
        printf("]");
        break;
    }
    case OBJ_TABLE:
        printf("<table>");
        break;
    case OBJ_MODULE:
        printf("<module %s>", AS_MODULE(value)->name->chars);
        break;
    case OBJ_NATIVE:
        printf("<native func>");
        break;
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    case OBJ_UPVALUE:
        printf("<upvalue>");
        break;
    case OBJ_NATIVE_METHOD:
        printf("<native method %s>", AS_NATIVE_METHOD(value)->name->chars);
        break;
    case OBJ_NATIVE_CLASS:
        printf("<native class %s>", AS_NATIVE_CLASS(value)->name->chars);
        break;
    case OBJ_NATIVE_MODULE:
        printf("<native module %s>", AS_NATIVE_MODULE(value)->name->chars);
        break;
    case OBJ_FUTURE: {
        ObjFuture* future = AS_FUTURE(value);
        FutureState state = futureGetState(future);
        switch (state) {
        case FUTURE_PENDING: printf("<future:pending>"); break;
        case FUTURE_READY:   printf("<future:ready>"); break;
        case FUTURE_ERROR:   printf("<future:error>"); break;
        }
        break;
    }
    case OBJ_ACTOR: {
        ObjActor* actor = AS_ACTOR(value);
        if (actor->alive) {
            printf("<actor:%s>", actor->klass->name->chars);
        } else {
            printf("<actor:dead>");
        }
        break;
    }
    default:
        printf("<obj>");
        break;
    }
}