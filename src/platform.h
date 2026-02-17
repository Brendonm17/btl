// ============================================================================
// platform.h - BTL Platform Abstraction Layer
//
// This header defines the platform abstraction for BTL, allowing it to run
// on different platforms (Windows, Linux, macOS, embedded systems, WASM, etc.)
// by providing configurable function pointers for:
// - Memory allocation
// - Threading primitives
// - Time functions
//
// Usage:
//   BtlPlatform platform = btl_platform_default();
//   // Override specific functions for your platform:
//   platform.mem.alloc = my_custom_alloc;
//   platform.thread.create = my_thread_create;
//   BTLConfig config = btl_config_default();
//   config.platform = &platform;
//   BTLRuntime* runtime = btl_runtime_new(&config);
// ============================================================================

#ifndef btl_platform_h
#define btl_platform_h

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// ============================================================================
// Opaque Handle Types
//
// These abstract away platform-specific types (pthread_t, HANDLE, etc.)
// ============================================================================

typedef void* BtlThreadHandle;      // Thread handle (pthread_t, HANDLE, etc.)
typedef void* BtlMutexHandle;       // Mutex handle
typedef void* BtlCondHandle;        // Condition variable handle

// ============================================================================
// Memory Function Handles
//
// These allow custom memory allocation strategies for embedded systems,
// arena allocators, memory pools, etc.
// ============================================================================

typedef struct {
    // Allocate memory of given size. Returns NULL on failure.
    void* (*alloc)(size_t size, void* user_data);

    // Reallocate memory. If ptr is NULL, behaves like alloc.
    // If new_size is 0, behaves like free and returns NULL.
    void* (*realloc)(void* ptr, size_t old_size, size_t new_size, void* user_data);

    // Free memory. ptr may be NULL (no-op in that case).
    void (*free)(void* ptr, size_t size, void* user_data);

    // User data passed to all memory functions
    void* user_data;
} BtlMemoryHandles;

// ============================================================================
// Threading Function Handles
//
// These allow custom threading implementations for platforms without pthreads
// (Windows, embedded RTOS, single-threaded environments, etc.)
// ============================================================================

typedef void* (*BtlThreadFunc)(void* arg);

typedef struct {
    // --- Thread Operations ---

    // Create a new thread running func(arg). Returns handle on success, NULL on failure.
    BtlThreadHandle (*thread_create)(BtlThreadFunc func, void* arg, void* user_data);

    // Wait for thread to finish and clean up resources
    void (*thread_join)(BtlThreadHandle thread, void* user_data);

    // --- Mutex Operations ---

    // Create a new mutex. Returns handle on success, NULL on failure.
    BtlMutexHandle (*mutex_create)(void* user_data);

    // Destroy a mutex and free resources
    void (*mutex_destroy)(BtlMutexHandle mutex, void* user_data);

    // Lock a mutex (blocks until acquired)
    void (*mutex_lock)(BtlMutexHandle mutex, void* user_data);

    // Unlock a mutex
    void (*mutex_unlock)(BtlMutexHandle mutex, void* user_data);

    // --- Condition Variable Operations ---

    // Create a new condition variable. Returns handle on success, NULL on failure.
    BtlCondHandle (*cond_create)(void* user_data);

    // Destroy a condition variable and free resources
    void (*cond_destroy)(BtlCondHandle cond, void* user_data);

    // Wait on condition variable (releases mutex while waiting)
    void (*cond_wait)(BtlCondHandle cond, BtlMutexHandle mutex, void* user_data);

    // Signal one waiting thread
    void (*cond_signal)(BtlCondHandle cond, void* user_data);

    // Signal all waiting threads
    void (*cond_broadcast)(BtlCondHandle cond, void* user_data);

    // User data passed to all threading functions
    void* user_data;
} BtlThreadHandles;

// ============================================================================
// Time Function Handles
//
// These allow custom time implementations for platforms without standard
// time functions or for deterministic testing.
// ============================================================================

typedef struct {
    // Get current time in seconds (high resolution, monotonic preferred)
    double (*clock)(void* user_data);

    // Sleep for given number of milliseconds
    void (*sleep_ms)(uint32_t ms, void* user_data);

    // User data passed to all time functions
    void* user_data;
} BtlTimeHandles;

// ============================================================================
// I/O Function Handles
//
// These allow custom I/O for embedded systems, GUIs, logging systems, etc.
// ============================================================================

typedef struct {
    // Print text to standard output
    void (*print)(const char* text, void* user_data);

    // Print text to error output
    void (*error)(const char* text, void* user_data);

    // Read a file by path. Returns malloc'd buffer (caller frees) or NULL.
    // If NULL, the default fopen/fread implementation is used.
    // out_size receives the file size (may be NULL if caller doesn't need it).
    char* (*read_file)(const char* path, size_t* out_size, void* user_data);

    // User data passed to all I/O functions
    void* user_data;
} BtlIOHandles;

// ============================================================================
// BtlPlatform - Complete Platform Configuration
//
// Contains all platform-specific function handles. Pass to BTLConfig when
// creating a runtime to customize platform behavior.
// ============================================================================

typedef struct {
    BtlMemoryHandles mem;       // Memory allocation functions
    BtlThreadHandles thread;    // Threading primitives
    BtlTimeHandles time;        // Time functions
    BtlIOHandles io;            // I/O functions
} BtlPlatform;

// ============================================================================
// Default Platform Implementations
//
// These provide standard implementations using system libraries (malloc,
// pthreads, stdio, etc.). Call btl_platform_default() to get a platform
// struct with all defaults filled in.
// ============================================================================

// Get default platform using standard system libraries
BtlPlatform btl_platform_default(void);

// Individual default initializers (useful for partial customization)
BtlMemoryHandles btl_memory_handles_default(void);
BtlThreadHandles btl_thread_handles_default(void);
BtlTimeHandles btl_time_handles_default(void);
BtlIOHandles btl_io_handles_default(void);

// ============================================================================
// Stub/No-op Implementations
//
// These provide no-op implementations for single-threaded or minimal
// environments where certain features aren't needed.
// ============================================================================

// Get stub threading handles (no-op, for single-threaded environments)
BtlThreadHandles btl_thread_handles_stub(void);

#endif // btl_platform_h
