// ============================================================================
// threadpool.c - BTL Thread Pool Implementation
//
// Uses platform-abstracted threading primitives for portability.
// ============================================================================

#include "threadpool.h"
#include "runtime.h"

static void* workerThread(void* arg) {
    ThreadPool* pool = (ThreadPool*)arg;
    BtlThreadHandles* th = pool->threadHandles;
    BtlMemoryHandles* mem = pool->memHandles;

    while (1) {
        th->mutex_lock(pool->mutex, th->user_data);

        while (pool->head == NULL && !pool->shutdown) {
            th->cond_wait(pool->cond, pool->mutex, th->user_data);
        }

        if (pool->shutdown && pool->head == NULL) {
            th->mutex_unlock(pool->mutex, th->user_data);
            break;
        }

        // Dequeue task
        BtlTask* task = pool->head;
        if (task != NULL) {
            pool->head = task->next;
            if (pool->head == NULL) {
                pool->tail = NULL;
            }
        }

        th->mutex_unlock(pool->mutex, th->user_data);

        // Execute task
        if (task != NULL) {
            task->function(task->arg);
            mem->free(task, sizeof(BtlTask), mem->user_data);
        }
    }

    return NULL;
}

void btl_threadpool_init(ThreadPool* pool, int numThreads, BTLRuntime* runtime) {
    BtlThreadHandles* th = &runtime->config.platform.thread;
    BtlMemoryHandles* mem = &runtime->config.platform.mem;

    pool->threadCount = numThreads;
    pool->threadHandles = th;
    pool->memHandles = mem;
    pool->head = NULL;
    pool->tail = NULL;
    pool->shutdown = false;

    // Allocate thread handle array using platform memory
    pool->threads = mem->alloc(sizeof(BtlThreadHandle) * numThreads, mem->user_data);

    // Create mutex and condition variable using platform handles
    pool->mutex = th->mutex_create(th->user_data);
    pool->cond = th->cond_create(th->user_data);

    // Create worker threads using platform handles
    for (int i = 0; i < numThreads; i++) {
        pool->threads[i] = th->thread_create(workerThread, pool, th->user_data);
    }
}

void btl_threadpool_submit(ThreadPool* pool, void (*function)(void*), void* arg) {
    BtlThreadHandles* th = pool->threadHandles;
    BtlMemoryHandles* mem = pool->memHandles;

    BtlTask* task = mem->alloc(sizeof(BtlTask), mem->user_data);
    task->function = function;
    task->arg = arg;
    task->next = NULL;

    th->mutex_lock(pool->mutex, th->user_data);

    if (pool->tail == NULL) {
        pool->head = task;
        pool->tail = task;
    } else {
        pool->tail->next = task;
        pool->tail = task;
    }

    th->cond_signal(pool->cond, th->user_data);
    th->mutex_unlock(pool->mutex, th->user_data);
}

void btl_threadpool_shutdown(ThreadPool* pool) {
    BtlThreadHandles* th = pool->threadHandles;
    BtlMemoryHandles* mem = pool->memHandles;

    th->mutex_lock(pool->mutex, th->user_data);
    pool->shutdown = true;
    th->cond_broadcast(pool->cond, th->user_data);
    th->mutex_unlock(pool->mutex, th->user_data);

    // Join all worker threads
    for (int i = 0; i < pool->threadCount; i++) {
        th->thread_join(pool->threads[i], th->user_data);
    }

    // Free remaining tasks
    BtlTask* task = pool->head;
    while (task != NULL) {
        BtlTask* next = task->next;
        mem->free(task, sizeof(BtlTask), mem->user_data);
        task = next;
    }

    // Free thread array
    mem->free(pool->threads, sizeof(BtlThreadHandle) * pool->threadCount, mem->user_data);

    // Destroy mutex and condition variable
    th->mutex_destroy(pool->mutex, th->user_data);
    th->cond_destroy(pool->cond, th->user_data);
}
