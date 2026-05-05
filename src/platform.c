// Default platform handles using standard system libraries.

// Must come before any includes.
#if !defined(_WIN32) && !defined(_WIN64)
#define _POSIX_C_SOURCE 200809L
#endif

#include "platform.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(_WIN32) || defined(_WIN64)
    #define BTL_PLATFORM_WINDOWS 1
    #include <windows.h>
#elif defined(__APPLE__) || defined(__linux__) || defined(__unix__)
    #define BTL_PLATFORM_POSIX 1
    #include <pthread.h>
    #include <unistd.h>
    #include <time.h>
    #include <sys/time.h>
#else
    #define BTL_PLATFORM_UNKNOWN 1
#endif

// ============================================================================
// Default Memory Implementation (uses malloc/realloc/free)
// ============================================================================

static void* default_mem_alloc(size_t size, void* user_data) {
    (void)user_data;
    return malloc(size);
}

static void* default_mem_realloc(void* ptr, size_t old_size, size_t new_size, void* user_data) {
    (void)user_data;
    (void)old_size;
    if (new_size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, new_size);
}

static void default_mem_free(void* ptr, size_t size, void* user_data) {
    (void)user_data;
    (void)size;
    free(ptr);
}

BtlMemoryHandles btl_memory_handles_default(void) {
    return (BtlMemoryHandles){
        .alloc = default_mem_alloc,
        .realloc = default_mem_realloc,
        .free = default_mem_free,
        .user_data = NULL
    };
}

// ============================================================================
// Default I/O Implementation (uses stdio)
// ============================================================================

static void default_io_print(const char* text, void* user_data) {
    (void)user_data;
    fputs(text, stdout);
    fflush(stdout);
}

static void default_io_error(const char* text, void* user_data) {
    (void)user_data;
    fputs(text, stderr);
    fflush(stderr);
}

BtlIOHandles btl_io_handles_default(void) {
    return (BtlIOHandles){
        .print = default_io_print,
        .error = default_io_error,
        .user_data = NULL
    };
}

// ============================================================================
// Default Time Implementation
// ============================================================================

static double default_time_clock(void* user_data) {
    (void)user_data;
#if BTL_PLATFORM_POSIX
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
#elif BTL_PLATFORM_WINDOWS
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) {
        QueryPerformanceFrequency(&freq);
    }
    LARGE_INTEGER counter;
    QueryPerformanceCounter(&counter);
    return (double)counter.QuadPart / (double)freq.QuadPart;
#else
    return (double)clock() / CLOCKS_PER_SEC;
#endif
}

static void default_time_sleep_ms(uint32_t ms, void* user_data) {
    (void)user_data;
#if BTL_PLATFORM_POSIX
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000L;
    nanosleep(&ts, NULL);
#elif BTL_PLATFORM_WINDOWS
    Sleep(ms);
#else
    // Busy wait fallback
    double start = default_time_clock(NULL);
    while ((default_time_clock(NULL) - start) * 1000 < ms) {
        // spin
    }
#endif
}

BtlTimeHandles btl_time_handles_default(void) {
    return (BtlTimeHandles){
        .clock = default_time_clock,
        .sleep_ms = default_time_sleep_ms,
        .user_data = NULL
    };
}

// ============================================================================
// Default Threading Implementation (POSIX pthreads)
// ============================================================================

#if BTL_PLATFORM_POSIX

static BtlThreadHandle default_thread_create(BtlThreadFunc func, void* arg, void* user_data) {
    (void)user_data;
    pthread_t* thread = malloc(sizeof(pthread_t));
    if (thread == NULL) return NULL;
    if (pthread_create(thread, NULL, func, arg) != 0) {
        free(thread);
        return NULL;
    }
    return (BtlThreadHandle)thread;
}

static void default_thread_join(BtlThreadHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_t* thread = (pthread_t*)handle;
    pthread_join(*thread, NULL);
    free(thread);
}

static BtlMutexHandle default_mutex_create(void* user_data) {
    (void)user_data;
    pthread_mutex_t* mutex = malloc(sizeof(pthread_mutex_t));
    if (mutex == NULL) return NULL;
    if (pthread_mutex_init(mutex, NULL) != 0) {
        free(mutex);
        return NULL;
    }
    return (BtlMutexHandle)mutex;
}

static void default_mutex_destroy(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_mutex_t* mutex = (pthread_mutex_t*)handle;
    pthread_mutex_destroy(mutex);
    free(mutex);
}

static void default_mutex_lock(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_mutex_lock((pthread_mutex_t*)handle);
}

static void default_mutex_unlock(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_mutex_unlock((pthread_mutex_t*)handle);
}

static BtlCondHandle default_cond_create(void* user_data) {
    (void)user_data;
    pthread_cond_t* cond = malloc(sizeof(pthread_cond_t));
    if (cond == NULL) return NULL;
    if (pthread_cond_init(cond, NULL) != 0) {
        free(cond);
        return NULL;
    }
    return (BtlCondHandle)cond;
}

static void default_cond_destroy(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_cond_t* cond = (pthread_cond_t*)handle;
    pthread_cond_destroy(cond);
    free(cond);
}

static void default_cond_wait(BtlCondHandle cond_handle, BtlMutexHandle mutex_handle, void* user_data) {
    (void)user_data;
    if (cond_handle == NULL || mutex_handle == NULL) return;
    pthread_cond_wait((pthread_cond_t*)cond_handle, (pthread_mutex_t*)mutex_handle);
}

static void default_cond_signal(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_cond_signal((pthread_cond_t*)handle);
}

static void default_cond_broadcast(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    pthread_cond_broadcast((pthread_cond_t*)handle);
}

BtlThreadHandles btl_thread_handles_default(void) {
    return (BtlThreadHandles){
        .thread_create = default_thread_create,
        .thread_join = default_thread_join,
        .mutex_create = default_mutex_create,
        .mutex_destroy = default_mutex_destroy,
        .mutex_lock = default_mutex_lock,
        .mutex_unlock = default_mutex_unlock,
        .cond_create = default_cond_create,
        .cond_destroy = default_cond_destroy,
        .cond_wait = default_cond_wait,
        .cond_signal = default_cond_signal,
        .cond_broadcast = default_cond_broadcast,
        .user_data = NULL
    };
}

#elif BTL_PLATFORM_WINDOWS

// Windows threading implementation using Win32 API

// Wrapper to bridge BtlThreadFunc (cdecl, returns void*) to LPTHREAD_START_ROUTINE (stdcall, returns DWORD)
typedef struct {
    BtlThreadFunc func;
    void* arg;
} WinThreadTrampoline;

static DWORD WINAPI win_thread_trampoline(LPVOID param) {
    WinThreadTrampoline ctx = *(WinThreadTrampoline*)param;
    free(param);
    ctx.func(ctx.arg);
    return 0;
}

static BtlThreadHandle default_thread_create(BtlThreadFunc func, void* arg, void* user_data) {
    (void)user_data;
    WinThreadTrampoline* ctx = malloc(sizeof(WinThreadTrampoline));
    if (ctx == NULL) return NULL;
    ctx->func = func;
    ctx->arg = arg;
    HANDLE thread = CreateThread(NULL, 0, win_thread_trampoline, ctx, 0, NULL);
    if (thread == NULL) {
        free(ctx);
        return NULL;
    }
    return (BtlThreadHandle)thread;
}

static void default_thread_join(BtlThreadHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    WaitForSingleObject((HANDLE)handle, INFINITE);
    CloseHandle((HANDLE)handle);
}

static BtlMutexHandle default_mutex_create(void* user_data) {
    (void)user_data;
    CRITICAL_SECTION* cs = malloc(sizeof(CRITICAL_SECTION));
    if (cs == NULL) return NULL;
    InitializeCriticalSection(cs);
    return (BtlMutexHandle)cs;
}

static void default_mutex_destroy(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    DeleteCriticalSection((CRITICAL_SECTION*)handle);
    free(handle);
}

static void default_mutex_lock(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    EnterCriticalSection((CRITICAL_SECTION*)handle);
}

static void default_mutex_unlock(BtlMutexHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    LeaveCriticalSection((CRITICAL_SECTION*)handle);
}

static BtlCondHandle default_cond_create(void* user_data) {
    (void)user_data;
    CONDITION_VARIABLE* cv = malloc(sizeof(CONDITION_VARIABLE));
    if (cv == NULL) return NULL;
    InitializeConditionVariable(cv);
    return (BtlCondHandle)cv;
}

static void default_cond_destroy(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    // Windows condition variables don't need explicit destruction
    free(handle);
}

static void default_cond_wait(BtlCondHandle cond_handle, BtlMutexHandle mutex_handle, void* user_data) {
    (void)user_data;
    if (cond_handle == NULL || mutex_handle == NULL) return;
    SleepConditionVariableCS((CONDITION_VARIABLE*)cond_handle,
                              (CRITICAL_SECTION*)mutex_handle, INFINITE);
}

static void default_cond_signal(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    WakeConditionVariable((CONDITION_VARIABLE*)handle);
}

static void default_cond_broadcast(BtlCondHandle handle, void* user_data) {
    (void)user_data;
    if (handle == NULL) return;
    WakeAllConditionVariable((CONDITION_VARIABLE*)handle);
}

BtlThreadHandles btl_thread_handles_default(void) {
    return (BtlThreadHandles){
        .thread_create = default_thread_create,
        .thread_join = default_thread_join,
        .mutex_create = default_mutex_create,
        .mutex_destroy = default_mutex_destroy,
        .mutex_lock = default_mutex_lock,
        .mutex_unlock = default_mutex_unlock,
        .cond_create = default_cond_create,
        .cond_destroy = default_cond_destroy,
        .cond_wait = default_cond_wait,
        .cond_signal = default_cond_signal,
        .cond_broadcast = default_cond_broadcast,
        .user_data = NULL
    };
}

#else

// Stub implementation for unknown platforms
BtlThreadHandles btl_thread_handles_default(void) {
    return btl_thread_handles_stub();
}

#endif

// ============================================================================
// Stub Threading Implementation (no-op, for single-threaded environments)
// ============================================================================

static BtlThreadHandle stub_thread_create(BtlThreadFunc func, void* arg, void* user_data) {
    (void)func; (void)arg; (void)user_data;
    return NULL;  // Threading not supported
}

static void stub_thread_join(BtlThreadHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static BtlMutexHandle stub_mutex_create(void* user_data) {
    (void)user_data;
    return (BtlMutexHandle)(uintptr_t)1;  // Non-null dummy handle
}

static void stub_mutex_destroy(BtlMutexHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static void stub_mutex_lock(BtlMutexHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static void stub_mutex_unlock(BtlMutexHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static BtlCondHandle stub_cond_create(void* user_data) {
    (void)user_data;
    return (BtlCondHandle)(uintptr_t)1;  // Non-null dummy handle
}

static void stub_cond_destroy(BtlCondHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static void stub_cond_wait(BtlCondHandle cond, BtlMutexHandle mutex, void* user_data) {
    (void)cond; (void)mutex; (void)user_data;
}

static void stub_cond_signal(BtlCondHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

static void stub_cond_broadcast(BtlCondHandle handle, void* user_data) {
    (void)handle; (void)user_data;
}

BtlThreadHandles btl_thread_handles_stub(void) {
    return (BtlThreadHandles){
        .thread_create = stub_thread_create,
        .thread_join = stub_thread_join,
        .mutex_create = stub_mutex_create,
        .mutex_destroy = stub_mutex_destroy,
        .mutex_lock = stub_mutex_lock,
        .mutex_unlock = stub_mutex_unlock,
        .cond_create = stub_cond_create,
        .cond_destroy = stub_cond_destroy,
        .cond_wait = stub_cond_wait,
        .cond_signal = stub_cond_signal,
        .cond_broadcast = stub_cond_broadcast,
        .user_data = NULL
    };
}

// ============================================================================
// Complete Platform Default
// ============================================================================

BtlPlatform btl_platform_default(void) {
    return (BtlPlatform){
        .mem = btl_memory_handles_default(),
        .thread = btl_thread_handles_default(),
        .time = btl_time_handles_default(),
        .io = btl_io_handles_default()
    };
}
