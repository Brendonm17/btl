// Platform abstraction. All function handles can be overridden so BTL can
// run on Windows, Linux, macOS, embedded systems, WASM, etc. without
// hard-coding pthread/stdio/malloc.

#ifndef btl_platform_h
#define btl_platform_h

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef void* BtlThreadHandle;
typedef void* BtlMutexHandle;
typedef void* BtlCondHandle;

typedef struct {
    void* (*alloc)(size_t size, void* user_data);
    void* (*realloc)(void* ptr, size_t old_size, size_t new_size, void* user_data);
    void (*free)(void* ptr, size_t size, void* user_data);
    void* user_data;
} BtlMemoryHandles;

typedef void* (*BtlThreadFunc)(void* arg);

typedef struct {
    BtlThreadHandle (*thread_create)(BtlThreadFunc func, void* arg, void* user_data);
    void (*thread_join)(BtlThreadHandle thread, void* user_data);

    BtlMutexHandle (*mutex_create)(void* user_data);
    void (*mutex_destroy)(BtlMutexHandle mutex, void* user_data);
    void (*mutex_lock)(BtlMutexHandle mutex, void* user_data);
    void (*mutex_unlock)(BtlMutexHandle mutex, void* user_data);

    BtlCondHandle (*cond_create)(void* user_data);
    void (*cond_destroy)(BtlCondHandle cond, void* user_data);
    void (*cond_wait)(BtlCondHandle cond, BtlMutexHandle mutex, void* user_data);
    void (*cond_signal)(BtlCondHandle cond, void* user_data);
    void (*cond_broadcast)(BtlCondHandle cond, void* user_data);

    void* user_data;
} BtlThreadHandles;

typedef struct {
    double (*clock)(void* user_data);
    void (*sleep_ms)(uint32_t ms, void* user_data);
    void* user_data;
} BtlTimeHandles;

typedef struct {
    void (*print)(const char* text, void* user_data);
    void (*error)(const char* text, void* user_data);

    // Returns a malloc'd buffer (caller frees) or NULL on failure.
    // out_size receives the file size; may be NULL.
    // If this handle is NULL, the default fopen/fread path is used.
    char* (*read_file)(const char* path, size_t* out_size, void* user_data);

    void* user_data;
} BtlIOHandles;

typedef struct {
    BtlMemoryHandles mem;
    BtlThreadHandles thread;
    BtlTimeHandles time;
    BtlIOHandles io;
} BtlPlatform;

BtlPlatform btl_platform_default(void);

BtlMemoryHandles btl_memory_handles_default(void);
BtlThreadHandles btl_thread_handles_default(void);
BtlTimeHandles btl_time_handles_default(void);
BtlIOHandles btl_io_handles_default(void);

// No-op threading handles for single-threaded environments.
BtlThreadHandles btl_thread_handles_stub(void);

#endif
