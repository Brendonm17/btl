// Top-level API for embedding BTL in applications. Manages config, platform
// abstraction, thread pool, and VM lifecycle.
//
// Quick start:
//   BTLConfig config = btl_config_default();
//   config.thread_count = 4;
//   BTLRuntime* runtime = btl_runtime_new(&config);
//   btl_runtime_exec(runtime, "print(\"Hello!\");");
//   btl_runtime_free(runtime);
//
// To customize platform handles (memory, threading, I/O):
//   BtlPlatform platform = btl_platform_default();
//   platform.mem.alloc = my_alloc;
//   BTLConfig config = btl_config_default();
//   config.platform = platform;

#ifndef btl_runtime_h
#define btl_runtime_h

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "platform.h"

struct VM;
typedef struct VM VM;

struct ThreadPool;
typedef struct ThreadPool ThreadPool;

struct BTLRuntime;
typedef struct BTLRuntime BTLRuntime;

// All fields optional; zero/NULL falls back to defaults.
typedef struct {
    uint32_t thread_count;          // 0 = auto-detect
    size_t initial_heap_size;
    size_t max_heap_size;           // 0 = unlimited
    size_t nursery_size;
    size_t gc_threshold;
    float gc_grow_factor;

    BtlPlatform platform;

    void* user_data;
} BTLConfig;

struct BTLRuntime {
    BTLConfig config;
    ThreadPool* pool;
    bool pool_initialized;
    VM* vm;
    bool initialized;

    // Live actors, tracked so we can join their threads on shutdown.
    struct ObjActor** actors;
    int actor_count;
    int actor_capacity;
    BtlMutexHandle actor_mutex;
};

typedef enum {
    BTL_OK = 0,
    BTL_COMPILE_ERROR,
    BTL_RUNTIME_ERROR,
    BTL_OUT_OF_MEMORY
} BTLResult;

BTLConfig btl_config_default(void);

// Pass NULL config for defaults.
BTLRuntime* btl_runtime_new(const BTLConfig* config);

void btl_runtime_free(BTLRuntime* runtime);

// Actor registry. btl_actor_new registers; btl_actor_stop unregisters.
// btl_runtime_stop_all_actors must be called before btl_runtime_free.
struct ObjActor;

void btl_runtime_register_actor(BTLRuntime* rt, struct ObjActor* actor);
void btl_runtime_unregister_actor(BTLRuntime* rt, struct ObjActor* actor);
void btl_runtime_stop_all_actors(BTLRuntime* rt);

BTLResult btl_runtime_exec(BTLRuntime* runtime, const char* source);
BTLResult btl_runtime_exec_file(BTLRuntime* runtime, const char* path);

bool btl_runtime_get_result(BTLRuntime* runtime, void* out_value);

ThreadPool* btl_runtime_get_pool(BTLRuntime* runtime);
int btl_runtime_thread_count(BTLRuntime* runtime);

// Embedding API: lets host applications register native modules, call into
// BTL, and read/write globals.

#include "value.h"

VM* btl_runtime_get_vm(BTLRuntime* runtime);

typedef void (*BtlModuleInitFn)(VM* vm);

void btl_runtime_register_module(BTLRuntime* runtime, const char* name, BtlModuleInitFn init_fn);

// Pushes callable + args, invokes, returns the result. *ok is set to false
// on error (NULL allowed).
BtlValue btl_runtime_call(BTLRuntime* runtime, BtlValue callable,
                           int argCount, BtlValue* args, bool* ok);

// Returns BTL_NULL_VAL if not found.
BtlValue btl_runtime_get_global(BTLRuntime* runtime, const char* name);

// Creates the global if it doesn't exist.
void btl_runtime_set_global(BTLRuntime* runtime, const char* name, BtlValue value);

int btl_get_cpu_count(void);

const char* btl_version(void);

void btl_set_system_args(int argc, const char* argv[]);

#endif
