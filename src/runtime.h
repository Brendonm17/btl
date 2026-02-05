#ifndef btl_runtime_h
#define btl_runtime_h

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

// ============================================================================
// Forward declarations
// ============================================================================

struct VM;
typedef struct VM VM;

struct ThreadPool;
typedef struct ThreadPool ThreadPool;

struct BTLRuntime;
typedef struct BTLRuntime BTLRuntime;

// ============================================================================
// Configuration
// ============================================================================

typedef struct {
    // Threading
    uint32_t thread_count;          // 0 = auto-detect (num CPU cores)

    // Memory
    size_t initial_heap_size;       // Initial heap size (0 = default 1MB)
    size_t max_heap_size;           // Max heap size (0 = unlimited)
    size_t nursery_size;            // Young gen size (0 = default)

    // GC tuning
    size_t gc_threshold;            // Bytes before first GC (0 = default)
    float gc_grow_factor;           // Heap growth factor (0 = default 2.0)

    // Memory callbacks (all optional, NULL = use system malloc/free)
    void* user_data;                // Passed to all callbacks
    void* (*alloc)(size_t size, void* user_data);
    void* (*realloc)(void* ptr, size_t old_size, size_t new_size, void* user_data);
    void  (*free)(void* ptr, size_t size, void* user_data);

    // I/O callbacks (all optional, NULL = use stdio)
    void (*print)(const char* text, void* user_data);
    void (*error)(const char* text, void* user_data);
} BTLConfig;

// ============================================================================
// Runtime Context
// ============================================================================

struct BTLRuntime {
    BTLConfig config;
    ThreadPool* pool;
    bool pool_initialized;
    VM* vm;
    bool initialized;
    void* user_data;
};

// ============================================================================
// API Functions
// ============================================================================

BTLConfig btl_config_default(void);

BTLRuntime* btl_runtime_new(const BTLConfig* config);
void btl_runtime_free(BTLRuntime* runtime);

typedef enum {
    BTL_OK = 0,
    BTL_COMPILE_ERROR,
    BTL_RUNTIME_ERROR,
    BTL_OUT_OF_MEMORY
} BTLResult;

BTLResult btl_runtime_exec(BTLRuntime* runtime, const char* source);
BTLResult btl_runtime_exec_file(BTLRuntime* runtime, const char* path);

bool btl_runtime_get_result(BTLRuntime* runtime, void* out_value);

// ============================================================================
// Accessors
// ============================================================================

ThreadPool* btl_runtime_get_pool(BTLRuntime* runtime);
int btl_runtime_thread_count(BTLRuntime* runtime);

// ============================================================================
// Utility
// ============================================================================

int btl_get_cpu_count(void);
const char* btl_version(void);

// native system args
void setSystemArgs(int argc, const char* argv []);

#endif