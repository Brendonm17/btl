// ============================================================================
// object.c - BTL Object System Implementation
//
// This file implements all heap-allocated object types and their operations.
// Includes:
// - Object creation (strings, functions, closures, classes, instances, etc.)
// - String interning
// - Futures and actors for concurrency
// - Native method/class/module registration
// ============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "runtime.h"
#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

// Object allocation macro - uses btl_object_allocate from memory.c
#define BTL_ALLOCATE_OBJ(vm, type, objectType) \
    (type*)btl_object_allocate(vm, sizeof(type), objectType)

// Compatibility alias
#define ALLOCATE_OBJ BTL_ALLOCATE_OBJ

// ============================================================================
// Future Operations
//
// Futures represent the result of asynchronous operations. They can be
// in pending, ready (resolved), or error (rejected) state.
// ============================================================================

ObjFuture* btl_future_new(VM* vm) {
    ObjFuture* future = ALLOCATE_OBJ(vm, ObjFuture, BTL_OBJ_FUTURE);
    future->state = BTL_FUTURE_PENDING;
    future->result = BTL_NULL_VAL;

    // Get thread handles from runtime
    BtlThreadHandles* th = &vm->runtime->config.platform.thread;
    future->threadHandles = th;
    future->mutex = th->mutex_create(th->user_data);
    future->cond = th->cond_create(th->user_data);
    return future;
}

void btl_future_resolve(ObjFuture* future, BtlValue result) {
    BtlThreadHandles* th = future->threadHandles;
    th->mutex_lock(future->mutex, th->user_data);
    future->result = result;
    future->state = BTL_FUTURE_READY;
    th->cond_broadcast(future->cond, th->user_data);
    th->mutex_unlock(future->mutex, th->user_data);
}

void btl_future_reject(ObjFuture* future, BtlValue error) {
    BtlThreadHandles* th = future->threadHandles;
    th->mutex_lock(future->mutex, th->user_data);
    future->result = error;
    future->state = BTL_FUTURE_ERROR;
    th->cond_broadcast(future->cond, th->user_data);
    th->mutex_unlock(future->mutex, th->user_data);
}

BtlValue btl_future_get(ObjFuture* future) {
    BtlThreadHandles* th = future->threadHandles;
    th->mutex_lock(future->mutex, th->user_data);
    while (future->state == BTL_FUTURE_PENDING) {
        th->cond_wait(future->cond, future->mutex, th->user_data);
    }
    BtlValue result = future->result;
    th->mutex_unlock(future->mutex, th->user_data);
    return result;
}

BtlFutureState btl_future_get_state(ObjFuture* future) {
    BtlThreadHandles* th = future->threadHandles;
    th->mutex_lock(future->mutex, th->user_data);
    BtlFutureState state = future->state;
    th->mutex_unlock(future->mutex, th->user_data);
    return state;
}

void btl_future_free(ObjFuture* future) {
    BtlThreadHandles* th = future->threadHandles;
    th->mutex_destroy(future->mutex, th->user_data);
    th->cond_destroy(future->cond, th->user_data);
}

// ============================================================================
// Message Queue Operations
//
// Thread-safe FIFO queue for actor messages. Uses a circular buffer.
// ============================================================================

#define BTL_MESSAGE_QUEUE_INITIAL_CAPACITY 16

void btl_message_queue_init(BtlMessageQueue* queue, BtlThreadHandles* th) {
    queue->capacity = BTL_MESSAGE_QUEUE_INITIAL_CAPACITY;
    queue->messages = malloc(sizeof(BtlActorMessage) * queue->capacity);
    queue->count = 0;
    queue->head = 0;
    queue->tail = 0;
    queue->threadHandles = th;
    queue->mutex = th->mutex_create(th->user_data);
    queue->cond = th->cond_create(th->user_data);
}

void btl_message_queue_free(BtlMessageQueue* queue) {
    BtlThreadHandles* th = queue->threadHandles;
    while (queue->count > 0) {
        BtlActorMessage* msg = &queue->messages[queue->head];
        if (msg->args != NULL) free(msg->args);
        queue->head = (queue->head + 1) % queue->capacity;
        queue->count--;
    }
    free(queue->messages);
    th->mutex_destroy(queue->mutex, th->user_data);
    th->cond_destroy(queue->cond, th->user_data);
}

void btl_message_queue_push(BtlMessageQueue* queue, BtlActorMessage message) {
    BtlThreadHandles* th = queue->threadHandles;
    th->mutex_lock(queue->mutex, th->user_data);

    if (queue->count == queue->capacity) {
        int newCapacity = queue->capacity * 2;
        BtlActorMessage* newMessages = malloc(sizeof(BtlActorMessage) * newCapacity);
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

    th->cond_signal(queue->cond, th->user_data);
    th->mutex_unlock(queue->mutex, th->user_data);
}

bool btl_message_queue_pop(BtlMessageQueue* queue, BtlActorMessage* out, int timeoutMs) {
    BtlThreadHandles* th = queue->threadHandles;
    th->mutex_lock(queue->mutex, th->user_data);

    if (timeoutMs < 0) {
        // Infinite wait
        while (queue->count == 0) {
            th->cond_wait(queue->cond, queue->mutex, th->user_data);
        }
    } else if (timeoutMs == 0) {
        // Non-blocking check
        if (queue->count == 0) {
            th->mutex_unlock(queue->mutex, th->user_data);
            return false;
        }
    } else {
        // Timed wait - simplified polling approach
        // Check once after waiting; for more precise timing, platform would need
        // to implement cond_timedwait
        if (queue->count == 0) {
            th->mutex_unlock(queue->mutex, th->user_data);
            // Use a simple poll approach
            int waited = 0;
            int pollInterval = timeoutMs < 10 ? timeoutMs : 10;
            while (waited < timeoutMs) {
                // Yield and wait a bit using a short busy wait
                for (volatile int i = 0; i < 100000; i++) { } // ~1ms on most systems
                waited += pollInterval;

                th->mutex_lock(queue->mutex, th->user_data);
                if (queue->count > 0) {
                    goto have_message;
                }
                th->mutex_unlock(queue->mutex, th->user_data);
            }
            return false;
        }
    }

have_message:
    *out = queue->messages[queue->head];
    queue->head = (queue->head + 1) % queue->capacity;
    queue->count--;

    th->mutex_unlock(queue->mutex, th->user_data);
    return true;
}

// ============================================================================
// Deep Copy
//
// Deep copies a value for passing between actors. Primitives are copied
// directly, objects are recursively copied. Actors and futures are passed
// by reference (safe to share).
// ============================================================================

BtlValue btl_deep_copy_value(VM* destVM, VM* srcVM, BtlValue value) {
    (void) srcVM;  // May be NULL

    if (IS_NUMBER(value) || IS_INT(value) || IS_BOOL(value) || IS_NULL(value)) {
        return value;
    }

    if (IS_STRING(value)) {
        ObjString* src = AS_STRING(value);
        return OBJ_VAL(btl_string_copy(destVM, src->chars, src->length));
    }

    if (IS_LIST(value)) {
        ObjList* src = AS_LIST(value);
        ObjList* dest = btl_list_new(destVM);
        btl_push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->items.count; i++) {
            BtlValue copied = btl_deep_copy_value(destVM, srcVM, src->items.values[i]);
            btl_value_array_write(destVM, &dest->items, copied);
        }
        btl_pop(destVM);
        return OBJ_VAL(dest);
    }

    if (IS_TABLE(value)) {
        ObjTable* src = AS_TABLE(value);
        ObjTable* dest = btl_table_obj_new(destVM);
        btl_push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->table.capacity; i++) {
            BtlEntry* entry = &src->table.entries[i];
            if (!IS_EMPTY(entry->key)) {
                BtlValue keyCopy = btl_deep_copy_value(destVM, srcVM, entry->key);
                BtlValue valCopy = btl_deep_copy_value(destVM, srcVM, entry->value);
                btl_table_set(destVM, &dest->table, keyCopy, valCopy);
            }
        }
        btl_pop(destVM);
        return OBJ_VAL(dest);
    }

    if (IS_ACTOR(value) || IS_FUTURE(value)) {
        return value;  // Pass by reference - safe to share
    }

    if (IS_INSTANCE(value)) {
        ObjInstance* src = AS_INSTANCE(value);
        ObjInstance* dest = btl_instance_new(destVM, src->klass);
        btl_push(destVM, OBJ_VAL(dest));
        for (int i = 0; i < src->klass->fieldCount; i++) {
            dest->fields[i] = btl_deep_copy_value(destVM, srcVM, src->fields[i]);
        }
        btl_pop(destVM);
        return OBJ_VAL(dest);
    }

    return BTL_NULL_VAL;
}

// ============================================================================
// Actor Operations
//
// Actors are isolated execution contexts that process messages sequentially
// on their own thread. Each actor has its own VM instance.
// ============================================================================

// Forward declare - this will be in vm.c
extern BtlInterpretResult btl_run_vm(VM* vm);

static void* actorThreadMain(void* arg) {
    ObjActor* actor = (ObjActor*) arg;
    VM* vm = actor->vm;

    while (!actor->shouldStop) {
        BtlActorMessage msg;

        if (!btl_message_queue_pop(&actor->inbox, &msg, 100)) {
            continue;
        }

        // Find method by name and arity
        ObjClosure* method = NULL;
        for (int i = 0; i < actor->klass->methodCount; i++) {
            BtlMethodEntry* entry = &actor->klass->methods[i];
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
                ObjString* errMsg = btl_string_copy(vm, "Method not found", 16);
                btl_future_reject(msg.future, OBJ_VAL(errMsg));
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
        btl_push(vm, BTL_NULL_VAL);

        // Push receiver and args
        btl_push(vm, actor->instance);
        for (int i = 0; i < msg.argCount; i++) {
            btl_push(vm, msg.args[i]);
        }

        // Call method
        BtlCallFrame* frame = &vm->frames[vm->frameCount++];
        frame->closure = method;
        frame->ip = method->function->chunk.code;
        frame->slots = vm->stack + 1;  // Skip the dummy slot
        frame->openUpvalues = NULL;

        BtlInterpretResult result = btl_run_vm(vm);

        if (result == BTL_INTERPRET_OK) {
            if (msg.future != NULL) {
                btl_future_resolve(msg.future, vm->lastReturnValue);
            }
        } else {
            if (msg.future != NULL) {
                ObjString* errMsg = btl_string_copy(vm, "Actor method failed", 19);
                btl_future_reject(msg.future, OBJ_VAL(errMsg));
            }
        }

        if (msg.args != NULL) free(msg.args);
    }

    actor->alive = false;
    return NULL;
}

ObjActor* btl_actor_new(VM* parentVM, ObjClass* klass, BtlValue* args, int argCount) {
    ObjActor* actor = ALLOCATE_OBJ(parentVM, ObjActor, BTL_OBJ_ACTOR);

    // Get thread handles from parent runtime
    BtlThreadHandles* th = &parentVM->runtime->config.platform.thread;
    actor->threadHandles = th;

    actor->vm = malloc(sizeof(VM));
    memset(actor->vm, 0, sizeof(VM));  // Zero-initialize before initVM
    btl_vm_init(actor->vm);

    // Share runtime from parent (needed for platform handles)
    actor->vm->runtime = parentVM->runtime;

    // Share native classes from parent
    actor->vm->stringClass = parentVM->stringClass;
    actor->vm->numberClass = parentVM->numberClass;
    actor->vm->listClass = parentVM->listClass;
    actor->vm->tableClass = parentVM->tableClass;

    // Share the module (contains globals, class definitions)
    actor->vm->rootModule = parentVM->rootModule;

    actor->klass = klass;

    // Create instance in actor's VM
    ObjInstance* instance = btl_instance_new(actor->vm, klass);
    actor->instance = OBJ_VAL(instance);
    btl_push(actor->vm, actor->instance);

    btl_message_queue_init(&actor->inbox, th);
    actor->alive = true;
    actor->shouldStop = false;

    // Find init method with matching arity
    ObjClosure* initMethod = NULL;
    for (int i = 0; i < klass->methodCount; i++) {
        BtlMethodEntry* entry = &klass->methods[i];
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
            BtlValue copied = btl_deep_copy_value(actor->vm, parentVM, args[i]);
            btl_push(actor->vm, copied);
        }

        // Call init synchronously
        BtlCallFrame* frame = &actor->vm->frames[actor->vm->frameCount++];
        frame->closure = initMethod;
        frame->ip = initMethod->function->chunk.code;
        frame->slots = actor->vm->stack;
        frame->openUpvalues = NULL;

        btl_run_vm(actor->vm);

        // Reset stack after init
        actor->vm->stackTop = actor->vm->stack;
        actor->vm->frameCount = 0;
        btl_push(actor->vm, actor->instance);
    }

    // Start thread using platform handles
    actor->thread = th->thread_create(actorThreadMain, actor, th->user_data);

    return actor;
}

void btl_actor_send(ObjActor* actor, ObjString* method, BtlValue* args, int argCount, ObjFuture* future) {
    if (!actor->alive) {
        if (future != NULL) {
            btl_future_reject(future, OBJ_VAL(btl_string_copy(actor->vm, "Actor is dead", 13)));
        }
        return;
    }

    BtlActorMessage msg;
    msg.method = btl_string_copy(actor->vm, method->chars, method->length);
    msg.future = future;
    msg.argCount = argCount;

    if (argCount > 0) {
        msg.args = malloc(sizeof(BtlValue) * argCount);
        for (int i = 0; i < argCount; i++) {
            msg.args[i] = btl_deep_copy_value(actor->vm, NULL, args[i]);
        }
    } else {
        msg.args = NULL;
    }

    btl_message_queue_push(&actor->inbox, msg);
}

void btl_actor_stop(ObjActor* actor) {
    BtlThreadHandles* th = actor->threadHandles;
    actor->shouldStop = true;
    th->thread_join(actor->thread, th->user_data);
    btl_message_queue_free(&actor->inbox);
    btl_vm_free(actor->vm, false);
    free(actor->vm);
}

// ============================================================================
// Native Methods/Classes/Modules
//
// Functions for creating and registering native (C-implemented) methods,
// classes, and modules.
// ============================================================================

ObjNativeMethod* btl_native_method_new(VM* vm, BtlNativeMethodFn fn, const char* name, int arity) {
    ObjNativeMethod* method = ALLOCATE_OBJ(vm, ObjNativeMethod, BTL_OBJ_NATIVE_METHOD);
    method->function = fn;
    method->name = btl_string_copy(vm, name, (int) strlen(name));
    method->arity = arity;
    return method;
}

ObjNativeClass* btl_native_class_new(VM* vm, const char* name) {
    ObjNativeClass* klass = BTL_ALLOCATE_OBJ(vm, ObjNativeClass, BTL_OBJ_NATIVE_CLASS);
    klass->name = btl_string_copy(vm, name, (int) strlen(name));
    btl_table_init(&klass->methods);
    return klass;
}

ObjNativeModule* btl_native_module_new(VM* vm, const char* name) {
    ObjNativeModule* module = BTL_ALLOCATE_OBJ(vm, ObjNativeModule, BTL_OBJ_NATIVE_MODULE);
    module->name = btl_string_copy(vm, name, (int) strlen(name));
    btl_table_init(&module->globals);
    return module;
}

void btl_native_class_add_method(VM* vm, ObjNativeClass* klass, const char* name,
                                  BtlNativeMethodFn fn, int arity) {
    ObjNativeMethod* method = btl_native_method_new(vm, fn, name, arity);
    btl_push(vm, OBJ_VAL(method));
    ObjString* nameStr = btl_string_copy(vm, name, (int) strlen(name));
    btl_push(vm, OBJ_VAL(nameStr));
    btl_table_set(vm, &klass->methods, OBJ_VAL(nameStr), OBJ_VAL(method));
    btl_pop(vm);
    btl_pop(vm);
}

void btl_native_module_add_value(VM* vm, ObjNativeModule* module, const char* name, BtlValue value) {
    btl_push(vm, value);
    ObjString* nameStr = btl_string_copy(vm, name, (int) strlen(name));
    btl_push(vm, OBJ_VAL(nameStr));
    btl_table_set(vm, &module->globals, OBJ_VAL(nameStr), value);
    btl_pop(vm);
    btl_pop(vm);
}

void btl_native_module_add_function(VM* vm, ObjNativeModule* module, const char* name,
                                     BtlNativeFn fn, int arity) {
    (void) arity;
    ObjNative* native = btl_native_new(vm, fn);
    btl_push(vm, OBJ_VAL(native));
    ObjString* nameStr = btl_string_copy(vm, name, (int) strlen(name));
    btl_push(vm, OBJ_VAL(nameStr));
    btl_table_set(vm, &module->globals, OBJ_VAL(nameStr), OBJ_VAL(native));
    btl_pop(vm);
    btl_pop(vm);
}

// ============================================================================
// Core Object Types
//
// Creation functions for the main object types.
// ============================================================================

ObjTable* btl_table_obj_new(struct VM* vm) {
    ObjTable* table = BTL_ALLOCATE_OBJ(vm, ObjTable, BTL_OBJ_TABLE);
    btl_table_init(&table->table);
    return table;
}

ObjBoundMethod* btl_bound_method_new(struct VM* vm, BtlValue receiver, ObjClosure* method) {
    ObjBoundMethod* bound = BTL_ALLOCATE_OBJ(vm, ObjBoundMethod, BTL_OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass* btl_class_new(struct VM* vm, struct ObjString* name) {
    ObjClass* klass = BTL_ALLOCATE_OBJ(vm, ObjClass, BTL_OBJ_CLASS);
    klass->name = name;

    // Initialize vtable
    klass->methods = NULL;
    klass->methodCount = 0;
    klass->methodCapacity = 0;

    // Method indices for compile-time lookup
    btl_table_init(&klass->methodIndices);

    // Field system
    klass->fieldCount = 0;
    btl_table_init(&klass->fieldIndices);

    // Init cache: -1 means not yet cached
    for (int i = 0; i < 9; i++) {
        klass->initCache[i] = -1;
    }

    return klass;
}

ObjClosure* btl_closure_new(VM* vm, ObjFunction* function) {
    btl_gc_inhibit_start(vm);  // Protect during construction

    size_t size = sizeof(ObjClosure) + sizeof(BtlRuntimeUpvalue) * function->upvalueCount;
    ObjClosure* closure = (ObjClosure*) btl_object_allocate(vm, size, BTL_OBJ_CLOSURE);
    closure->function = function;
    closure->upvalueCount = function->upvalueCount;

    for (int i = 0; i < function->upvalueCount; i++) {
        closure->upvalues[i].isOpen = true;
        closure->upvalues[i].next = NULL;
    }

    // Allocate and initialize IC arrays
    if (function->fieldICCount > 0) {
        closure->fieldICs = BTL_ALLOCATE(vm, BtlFieldIC, function->fieldICCount);
        btl_field_ic_init(closure->fieldICs, function->fieldICCount);
    } else {
        closure->fieldICs = NULL;
    }

    if (function->methodICCount > 0) {
        closure->methodICs = BTL_ALLOCATE(vm, BtlMethodIC, function->methodICCount);
        btl_method_ic_init(closure->methodICs, function->methodICCount);
    } else {
        closure->methodICs = NULL;
    }

    btl_gc_inhibit_end(vm);  // Safe now
    return closure;
}

ObjFunction* btl_function_new(struct VM* vm, ObjModule* module) {
    ObjFunction* function = BTL_ALLOCATE_OBJ(vm, ObjFunction, BTL_OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    function->module = module;
    function->fieldICCount = 0;
    function->methodICCount = 0;
    function->compiledHandler = NULL;
    btl_chunk_init(&function->chunk);
    return function;
}

ObjInstance* btl_instance_new(struct VM* vm, ObjClass* klass) {
    btl_gc_inhibit_start(vm);  // Protect during field allocation

    ObjInstance* instance = BTL_ALLOCATE_OBJ(vm, ObjInstance, BTL_OBJ_INSTANCE);
    instance->klass = klass;

    // Safety: always allocate at least 1 byte if count is 0
    // to prevent malloc(0) implementation-defined behavior
    int size = klass->fieldCount > 0 ? klass->fieldCount : 1;
    instance->fields = BTL_ALLOCATE(vm, BtlValue, size);

    for (int i = 0; i < klass->fieldCount; i++) {
        instance->fields[i] = BTL_NULL_VAL;
    }

    btl_gc_inhibit_end(vm);
    return instance;
}

ObjList* btl_list_new(struct VM* vm) {
    ObjList* list = BTL_ALLOCATE_OBJ(vm, ObjList, BTL_OBJ_LIST);
    btl_value_array_init(&list->items);
    return list;
}

ObjModule* btl_module_new(struct VM* vm, struct ObjString* name) {
    ObjModule* module = BTL_ALLOCATE_OBJ(vm, ObjModule, BTL_OBJ_MODULE);
    module->name = name;
    btl_table_init(&module->globalNames);
    btl_value_array_init(&module->globalValues);
    btl_table_init(&module->classInfo);
    return module;
}

ObjNative* btl_native_new(struct VM* vm, BtlNativeFn function) {
    ObjNative* native = BTL_ALLOCATE_OBJ(vm, ObjNative, BTL_OBJ_NATIVE);
    native->function = function;
    return native;
}

// ============================================================================
// String Interning
//
// All strings are interned (deduplicated). This allows O(1) string
// comparison by pointer equality. Uses FNV-1a hash.
// ============================================================================

static uint32_t btl_hash_string(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t) key[i];
        hash *= 16777619;
    }
    return hash;
}

static struct ObjString* btl_string_allocate(struct VM* vm, char* chars, int length, uint32_t hash) {
    btl_gc_inhibit_start(vm);  // Protect: allocate string, then add to table
    struct ObjString* string = (struct ObjString*) btl_object_allocate(vm, sizeof(struct ObjString), BTL_OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    btl_push(vm, OBJ_VAL(string));
    btl_table_set(vm, &vm->strings, OBJ_VAL(string), BTL_NULL_VAL);
    btl_pop(vm);
    btl_gc_inhibit_end(vm);
    return string;
}

struct ObjString* btl_string_take(struct VM* vm, char* chars, int length) {
    uint32_t hash = btl_hash_string(chars, length);
    struct ObjString* interned = btl_table_find_string(&vm->strings, chars, length, hash);
    if (interned != NULL) {
        BTL_FREE_ARRAY(vm, char, chars, length + 1);
        return interned;
    }
    return btl_string_allocate(vm, chars, length, hash);
}

struct ObjString* btl_string_copy(struct VM* vm, const char* chars, int length) {
    uint32_t hash = btl_hash_string(chars, length);
    struct ObjString* interned = btl_table_find_string(&vm->strings, chars, length, hash);
    if (interned != NULL) return interned;

    btl_gc_inhibit_start(vm);  // Protect: allocate chars, then string object
    char* heapChars = BTL_ALLOCATE(vm, char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    struct ObjString* result = btl_string_allocate(vm, heapChars, length, hash);
    btl_gc_inhibit_end(vm);
    return result;
}

// ============================================================================
// Upvalues
//
// Upvalue boxes hold closed-over values after the stack slot goes away.
// ============================================================================

ObjUpvalue* btl_upvalue_box_new(VM* vm, BtlValue value) {
    ObjUpvalue* box = BTL_ALLOCATE_OBJ(vm, ObjUpvalue, BTL_OBJ_UPVALUE);
    box->closed = value;
    return box;
}

// ============================================================================
// Print Object
//
// Print an object value for debugging purposes.
// ============================================================================

void btl_object_print(BtlValue value) {
    switch (OBJ_TYPE(value)) {
    case BTL_OBJ_BOUND_METHOD:
        printf("<func %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
        break;
    case BTL_OBJ_CLASS:
        printf("<class %s>", AS_CLASS(value)->name->chars);
        break;
    case BTL_OBJ_CLOSURE:
        printf("<func %s>", AS_CLOSURE(value)->function->name->chars);
        break;
    case BTL_OBJ_FUNCTION:
        printf("<func %s>", AS_FUNCTION(value)->name->chars);
        break;
    case BTL_OBJ_INSTANCE:
        printf("<%s instance>", AS_INSTANCE(value)->klass->name->chars);
        break;
    case BTL_OBJ_LIST: {
        ObjList* list = AS_LIST(value);
        printf("[");
        for (int i = 0; i < list->items.count; i++) {
            btl_value_print(list->items.values[i]);
            if (i < list->items.count - 1) printf(", ");
        }
        printf("]");
        break;
    }
    case BTL_OBJ_TABLE:
        printf("<table>");
        break;
    case BTL_OBJ_MODULE:
        printf("<module %s>", AS_MODULE(value)->name->chars);
        break;
    case BTL_OBJ_NATIVE:
        printf("<native func>");
        break;
    case BTL_OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    case BTL_OBJ_UPVALUE:
        printf("<upvalue>");
        break;
    case BTL_OBJ_NATIVE_METHOD:
        printf("<native method %s>", AS_NATIVE_METHOD(value)->name->chars);
        break;
    case BTL_OBJ_NATIVE_CLASS:
        printf("<native class %s>", AS_NATIVE_CLASS(value)->name->chars);
        break;
    case BTL_OBJ_NATIVE_MODULE:
        printf("<native module %s>", AS_NATIVE_MODULE(value)->name->chars);
        break;
    case BTL_OBJ_FUTURE: {
        ObjFuture* future = AS_FUTURE(value);
        BtlFutureState state = btl_future_get_state(future);
        switch (state) {
        case BTL_FUTURE_PENDING: printf("<future:pending>"); break;
        case BTL_FUTURE_READY:   printf("<future:ready>"); break;
        case BTL_FUTURE_ERROR:   printf("<future:error>"); break;
        }
        break;
    }
    case BTL_OBJ_ACTOR: {
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