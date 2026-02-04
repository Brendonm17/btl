#ifndef btl_threadpool_h
#define btl_threadpool_h

#include <pthread.h>
#include <stdbool.h>

typedef struct Task {
    void (*function)(void* arg);
    void* arg;
    struct Task* next;
} Task;

// Use a named struct so it can be forward-declared elsewhere
struct ThreadPool {
    pthread_t* threads;
    int threadCount;
    Task* head;
    Task* tail;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool shutdown;
};

typedef struct ThreadPool ThreadPool;

void threadPoolInit(ThreadPool* pool, int numThreads);
void threadPoolSubmit(ThreadPool* pool, void (*function)(void*), void* arg);
void threadPoolShutdown(ThreadPool* pool);

#endif