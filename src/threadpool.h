#ifndef btl_threadpool_h
#define btl_threadpool_h

#include <stdbool.h>
#include "platform.h"

// Forward declaration
struct BTLRuntime;

typedef struct BtlTask {
    void (*function)(void* arg);
    void* arg;
    struct BtlTask* next;
} BtlTask;

// Thread pool structure using platform-abstracted handles
struct ThreadPool {
    BtlThreadHandle* threads;       // Array of thread handles
    int threadCount;
    BtlTask* head;
    BtlTask* tail;
    BtlMutexHandle mutex;           // Platform mutex handle
    BtlCondHandle cond;             // Platform condition variable handle
    volatile bool shutdown;
    BtlThreadHandles* threadHandles; // Platform threading functions
    BtlMemoryHandles* memHandles;    // Platform memory functions
};

typedef struct ThreadPool ThreadPool;

// Initialize thread pool with platform handles from runtime
void btl_threadpool_init(ThreadPool* pool, int numThreads, struct BTLRuntime* runtime);

// Submit a task to the thread pool
void btl_threadpool_submit(ThreadPool* pool, void (*function)(void*), void* arg);

// Shutdown and cleanup the thread pool
void btl_threadpool_shutdown(ThreadPool* pool);

#endif
