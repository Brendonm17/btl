#include "runtime.h"
#include "threadpool.h"
#include "vm.h"
#include "memory.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

// ============================================================================
// Version
// ============================================================================

#define BTL_VERSION_MAJOR 0
#define BTL_VERSION_MINOR 1
#define BTL_VERSION_PATCH 0

const char* btl_version(void) {
    static char version[32];
    snprintf(version, sizeof(version), "%d.%d.%d",
        BTL_VERSION_MAJOR, BTL_VERSION_MINOR, BTL_VERSION_PATCH);
    return version;
}

// ============================================================================
// CPU Detection
// ============================================================================

int btl_get_cpu_count(void) {
#ifdef _WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return (int) sysinfo.dwNumberOfProcessors;
#elif defined(_SC_NPROCESSORS_ONLN)
    int count = (int) sysconf(_SC_NPROCESSORS_ONLN);
    return count > 0 ? count : 1;
#else
    return 1;
#endif
}

// ============================================================================
// Default Configuration
// ============================================================================

BTLConfig btl_config_default(void) {
    return (BTLConfig) {
        .thread_count = 0,
            .initial_heap_size = 1024 * 1024,
            .max_heap_size = 0,
            .nursery_size = 0,
            .gc_threshold = 0,
            .gc_grow_factor = 0,
            .user_data = NULL,
            .alloc = NULL,
            .realloc = NULL,
            .free = NULL,
            .print = NULL,
            .error = NULL,
    };
}

// ============================================================================
// Runtime Creation / Destruction
// ============================================================================

BTLRuntime* btl_runtime_new(const BTLConfig* config) {
    BTLConfig cfg = config ? *config : btl_config_default();

    if (cfg.thread_count == 0) {
        cfg.thread_count = btl_get_cpu_count();
    }

    // Allocate runtime structure using callbacks (or system malloc)
    BTLRuntime* runtime;
    if (cfg.alloc != NULL) {
        runtime = cfg.alloc(sizeof(BTLRuntime), cfg.user_data);
    } else {
        runtime = malloc(sizeof(BTLRuntime));
    }

    if (runtime == NULL) {
        return NULL;
    }

    memset(runtime, 0, sizeof(BTLRuntime));
    runtime->config = cfg;
    runtime->user_data = cfg.user_data;

    // Allocate thread pool using runtime allocator
    runtime->pool = btl_runtime_alloc(runtime, NULL, 0, sizeof(ThreadPool));
    if (runtime->pool == NULL) {
        btl_runtime_alloc(runtime, runtime, sizeof(BTLRuntime), 0);
        return NULL;
    }

    threadPoolInit(runtime->pool, cfg.thread_count);
    runtime->pool_initialized = true;

    // Allocate VM using runtime allocator
    runtime->vm = btl_runtime_alloc(runtime, NULL, 0, sizeof(VM));
    if (runtime->vm == NULL) {
        threadPoolShutdown(runtime->pool);
        btl_runtime_alloc(runtime, runtime->pool, sizeof(ThreadPool), 0);
        btl_runtime_alloc(runtime, runtime, sizeof(BTLRuntime), 0);
        return NULL;
    }

    // CRITICAL: Link VM to runtime BEFORE initVM
    runtime->vm->runtime = runtime;

    initVM(runtime->vm);
    runtime->initialized = true;

    return runtime;
}

void btl_runtime_free(BTLRuntime* runtime) {
    if (runtime == NULL) return;

    // Free VM
    if (runtime->vm != NULL) {
        freeVM(runtime->vm, true);
        btl_runtime_alloc(runtime, runtime->vm, sizeof(VM), 0);
        runtime->vm = NULL;
    }

    // Shutdown thread pool
    if (runtime->pool_initialized && runtime->pool != NULL) {
        threadPoolShutdown(runtime->pool);
        btl_runtime_alloc(runtime, runtime->pool, sizeof(ThreadPool), 0);
        runtime->pool = NULL;
    }

    // Free runtime itself - must use raw callback since runtime is going away
    if (runtime->config.free != NULL) {
        runtime->config.free(runtime, sizeof(BTLRuntime), runtime->config.user_data);
    } else {
        free(runtime);
    }
}

// ============================================================================
// Execution
// ============================================================================

BTLResult btl_runtime_exec(BTLRuntime* runtime, const char* source) {
    if (runtime == NULL || !runtime->initialized || source == NULL) {
        return BTL_RUNTIME_ERROR;
    }

    InterpretResult result = interpret(runtime->vm, runtime->vm->rootModule, source);

    switch (result) {
    case INTERPRET_OK:
        return BTL_OK;
    case INTERPRET_COMPILE_ERROR:
        return BTL_COMPILE_ERROR;
    case INTERPRET_RUNTIME_ERROR:
        return BTL_RUNTIME_ERROR;
    default:
        return BTL_RUNTIME_ERROR;
    }
}

BTLResult btl_runtime_exec_file(BTLRuntime* runtime, const char* path) {
    if (runtime == NULL || path == NULL) {
        return BTL_RUNTIME_ERROR;
    }

    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        btl_runtime_error(runtime, "Could not open file.\n");
        return BTL_RUNTIME_ERROR;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    char* source = btl_runtime_alloc(runtime, NULL, 0, size + 1);
    if (source == NULL) {
        fclose(file);
        return BTL_OUT_OF_MEMORY;
    }

    size_t bytesRead = fread(source, 1, size, file);
    fclose(file);

    if (bytesRead != size) {
        btl_runtime_alloc(runtime, source, size + 1, 0);
        return BTL_RUNTIME_ERROR;
    }

    source[size] = '\0';

    BTLResult result = btl_runtime_exec(runtime, source);

    btl_runtime_alloc(runtime, source, size + 1, 0);

    return result;
}

// ============================================================================
// Accessors
// ============================================================================

ThreadPool* btl_runtime_get_pool(BTLRuntime* runtime) {
    return runtime ? runtime->pool : NULL;
}

int btl_runtime_thread_count(BTLRuntime* runtime) {
    if (runtime == NULL || runtime->pool == NULL) return 0;
    return runtime->pool->threadCount;
}

bool btl_runtime_get_result(BTLRuntime* runtime, void* out_value) {
    if (runtime == NULL || runtime->vm == NULL) return false;
    (void) out_value;
    return true;
}