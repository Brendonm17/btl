#include <stdlib.h>
#include "threadpool.h"

static void* workerThread(void* arg) {
    ThreadPool* pool = (ThreadPool*) arg;

    while (1) {
        pthread_mutex_lock(&pool->mutex);

        while (pool->head == NULL && !pool->shutdown) {
            pthread_cond_wait(&pool->cond, &pool->mutex);
        }

        if (pool->shutdown && pool->head == NULL) {
            pthread_mutex_unlock(&pool->mutex);
            break;
        }

        // Dequeue task
        Task* task = pool->head;
        if (task != NULL) {
            pool->head = task->next;
            if (pool->head == NULL) {
                pool->tail = NULL;
            }
        }

        pthread_mutex_unlock(&pool->mutex);

        // Execute task
        if (task != NULL) {
            task->function(task->arg);
            free(task);
        }
    }

    return NULL;
}

void threadPoolInit(ThreadPool* pool, int numThreads) {
    pool->threadCount = numThreads;
    pool->threads = malloc(sizeof(pthread_t) * numThreads);
    pool->head = NULL;
    pool->tail = NULL;
    pool->shutdown = false;

    pthread_mutex_init(&pool->mutex, NULL);
    pthread_cond_init(&pool->cond, NULL);

    for (int i = 0; i < numThreads; i++) {
        pthread_create(&pool->threads[i], NULL, workerThread, pool);
    }
}

void threadPoolSubmit(ThreadPool* pool, void (*function)(void*), void* arg) {
    Task* task = malloc(sizeof(Task));
    task->function = function;
    task->arg = arg;
    task->next = NULL;

    pthread_mutex_lock(&pool->mutex);

    if (pool->tail == NULL) {
        pool->head = task;
        pool->tail = task;
    } else {
        pool->tail->next = task;
        pool->tail = task;
    }

    pthread_cond_signal(&pool->cond);
    pthread_mutex_unlock(&pool->mutex);
}

void threadPoolShutdown(ThreadPool* pool) {
    pthread_mutex_lock(&pool->mutex);
    pool->shutdown = true;
    pthread_cond_broadcast(&pool->cond);
    pthread_mutex_unlock(&pool->mutex);

    for (int i = 0; i < pool->threadCount; i++) {
        pthread_join(pool->threads[i], NULL);
    }

    // Free remaining tasks
    Task* task = pool->head;
    while (task != NULL) {
        Task* next = task->next;
        free(task);
        task = next;
    }

    free(pool->threads);
    pthread_mutex_destroy(&pool->mutex);
    pthread_cond_destroy(&pool->cond);
}