// ============================================================================
// runtime.h - BTL Runtime API
//
// The runtime provides the top-level API for embedding BTL in applications.
// It manages:
// - Configuration (threading, memory, GC tuning)
// - Platform abstraction for portability
// - Thread pool for async operations
// - VM lifecycle
//
// Usage:
//   BTLConfig config = btl_config_default();
//   config.thread_count = 4;
//   BTLRuntime* runtime = btl_runtime_new(&config);
//   btl_runtime_exec(runtime, "print(\"Hello!\");");
//   btl_runtime_free(runtime);
//
// Custom platform:
//   BtlPlatform platform = btl_platform_default();
//   platform.mem.alloc = my_alloc;  // Custom allocator
//   BTLConfig config = btl_config_default();
//   config.platform = platform;
//   BTLRuntime* runtime = btl_runtime_new(&config);
// ============================================================================

#ifndef btl_runtime_h
#define btl_runtime_h

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "platform.h"

// ----------------------------------------------------------------------------
// Forward Declarations
// ----------------------------------------------------------------------------

struct VM;
typedef struct VM VM;

struct ThreadPool;
typedef struct ThreadPool ThreadPool;

struct BTLRuntime;
typedef struct BTLRuntime BTLRuntime;

// ----------------------------------------------------------------------------
// BTLConfig - Runtime Configuration
//
// All fields are optional. Zero/NULL values use sensible defaults.
// Platform abstraction allows complete customization of memory, threading,
// and I/O for embedded environments and cross-platform portability.
// ----------------------------------------------------------------------------
typedef struct {
    // Threading configuration
    uint32_t thread_count;          // 0 = auto-detect (num CPU cores)

    // Memory configuration
    size_t initial_heap_size;       // Initial heap size (0 = default 1MB)
    size_t max_heap_size;           // Maximum heap size (0 = unlimited)
    size_t nursery_size;            // Young generation size (0 = default)

    // GC tuning
    size_t gc_threshold;            // Bytes before first GC (0 = default)
    float gc_grow_factor;           // Heap growth factor (0 = default 2.0)

    // Platform abstraction (all handles optional, defaults used if not set)
    // Contains memory, threading, time, and I/O function handles
    BtlPlatform platform;

    // User data (for backward compatibility, also available in platform handles)
    void* user_data;
} BTLConfig;

// ----------------------------------------------------------------------------
// BTLRuntime - Runtime Context
//
// Contains all state for a BTL execution environment including the VM,
// thread pool, platform handles, and configuration. Multiple runtimes can
// exist independently with their own platform configurations.
// ----------------------------------------------------------------------------
struct BTLRuntime {
    BTLConfig config;               // Active configuration (includes platform)
    ThreadPool* pool;               // Thread pool for async operations
    bool pool_initialized;          // True if pool was successfully created
    VM* vm;                         // Virtual machine instance
    bool initialized;               // True if VM was successfully created
};

// ----------------------------------------------------------------------------
// BTLResult - Execution Result Codes
// ----------------------------------------------------------------------------
typedef enum {
    BTL_OK = 0,                     // Execution completed successfully
    BTL_COMPILE_ERROR,              // Compilation failed
    BTL_RUNTIME_ERROR,              // Runtime error occurred
    BTL_OUT_OF_MEMORY               // Memory allocation failed
} BTLResult;

// ----------------------------------------------------------------------------
// Configuration
// ----------------------------------------------------------------------------

// Get default configuration with sensible values
BTLConfig btl_config_default(void);

// ----------------------------------------------------------------------------
// Runtime Lifecycle
// ----------------------------------------------------------------------------

// Create a new runtime with the given configuration
// Pass NULL for default configuration
BTLRuntime* btl_runtime_new(const BTLConfig* config);

// Free all runtime resources
void btl_runtime_free(BTLRuntime* runtime);

// ----------------------------------------------------------------------------
// Execution
// ----------------------------------------------------------------------------

// Execute BTL source code
BTLResult btl_runtime_exec(BTLRuntime* runtime, const char* source);

// Execute BTL source from a file
BTLResult btl_runtime_exec_file(BTLRuntime* runtime, const char* path);

// Get the last execution result value
bool btl_runtime_get_result(BTLRuntime* runtime, void* out_value);

// ----------------------------------------------------------------------------
// Accessors
// ----------------------------------------------------------------------------

// Get the thread pool (for async operations)
ThreadPool* btl_runtime_get_pool(BTLRuntime* runtime);

// Get the number of worker threads
int btl_runtime_thread_count(BTLRuntime* runtime);

// ----------------------------------------------------------------------------
// Utility
// ----------------------------------------------------------------------------

// Get the number of CPU cores
int btl_get_cpu_count(void);

// Get BTL version string
const char* btl_version(void);

// Set command-line arguments for system.args native function
void btl_set_system_args(int argc, const char* argv[]);

#endif // btl_runtime_h
