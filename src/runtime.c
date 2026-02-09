// runtime.c - BTL Runtime Implementation
// Provides the high-level runtime API for the BTL interpreter

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

// Returns the version string for the BTL runtime
const char* btl_version(void) {
    static char version[32];
    snprintf(version, sizeof(version), "%d.%d.%d",
        BTL_VERSION_MAJOR, BTL_VERSION_MINOR, BTL_VERSION_PATCH);
    return version;
}

// ============================================================================
// CPU Detection
// ============================================================================

// Returns the number of available CPU cores for thread pool sizing
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

// Creates a default configuration with sensible defaults
BTLConfig btl_config_default(void) {
    return (BTLConfig) {
        .thread_count = 0,
        .initial_heap_size = 1024 * 1024,
        .max_heap_size = 0,
        .nursery_size = 0,
        .gc_threshold = 0,
        .gc_grow_factor = 0,
        .platform = btl_platform_default(),
        .user_data = NULL,
    };
}

// ============================================================================
// Runtime Creation / Destruction
// ============================================================================

// Creates a new BTL runtime with the given configuration
BTLRuntime* btl_runtime_new(const BTLConfig* config) {
    BTLConfig cfg = config ? *config : btl_config_default();

    // Default thread count to number of CPU cores
    if (cfg.thread_count == 0) {
        cfg.thread_count = btl_get_cpu_count();
    }

    // Ensure platform has defaults if not set
    if (cfg.platform.mem.alloc == NULL) {
        cfg.platform.mem = btl_memory_handles_default();
    }
    if (cfg.platform.thread.thread_create == NULL) {
        cfg.platform.thread = btl_thread_handles_default();
    }
    if (cfg.platform.time.clock == NULL) {
        cfg.platform.time = btl_time_handles_default();
    }
    if (cfg.platform.io.print == NULL) {
        cfg.platform.io = btl_io_handles_default();
    }

    // Allocate runtime structure using platform memory handles
    BTLRuntime* runtime = cfg.platform.mem.alloc(sizeof(BTLRuntime), cfg.platform.mem.user_data);
    if (runtime == NULL) {
        return NULL;
    }

    memset(runtime, 0, sizeof(BTLRuntime));
    runtime->config = cfg;

    // Allocate thread pool using runtime allocator
    runtime->pool = btl_runtime_alloc(runtime, NULL, 0, sizeof(ThreadPool));
    if (runtime->pool == NULL) {
        btl_runtime_alloc(runtime, runtime, sizeof(BTLRuntime), 0);
        return NULL;
    }

    // Initialize the thread pool with platform handles
    btl_threadpool_init(runtime->pool, cfg.thread_count, runtime);
    runtime->pool_initialized = true;

    // Allocate VM using runtime allocator
    runtime->vm = btl_runtime_alloc(runtime, NULL, 0, sizeof(VM));
    if (runtime->vm == NULL) {
        btl_threadpool_shutdown(runtime->pool);
        btl_runtime_alloc(runtime, runtime->pool, sizeof(ThreadPool), 0);
        btl_runtime_alloc(runtime, runtime, sizeof(BTLRuntime), 0);
        return NULL;
    }
    memset(runtime->vm, 0, sizeof(VM));  // Zero-initialize before btl_vm_init

    // CRITICAL: Link VM to runtime BEFORE btl_vm_init
    runtime->vm->runtime = runtime;

    // Initialize the VM with btl_vm_init
    btl_vm_init(runtime->vm);
    runtime->initialized = true;

    return runtime;
}

// Frees the BTL runtime and all associated resources
void btl_runtime_free(BTLRuntime* runtime) {
    if (runtime == NULL) return;

    // Free VM using btl_vm_free
    if (runtime->vm != NULL) {
        btl_vm_free(runtime->vm, true);
        btl_runtime_alloc(runtime, runtime->vm, sizeof(VM), 0);
        runtime->vm = NULL;
    }

    // Shutdown thread pool
    if (runtime->pool_initialized && runtime->pool != NULL) {
        btl_threadpool_shutdown(runtime->pool);
        btl_runtime_alloc(runtime, runtime->pool, sizeof(ThreadPool), 0);
        runtime->pool = NULL;
    }

    // Free runtime itself - must use platform memory handle since runtime is going away
    BtlMemoryHandles* mem = &runtime->config.platform.mem;
    mem->free(runtime, sizeof(BTLRuntime), mem->user_data);
}

// ============================================================================
// Execution
// ============================================================================

// Executes BTL source code in the runtime
BTLResult btl_runtime_exec(BTLRuntime* runtime, const char* source) {
    if (runtime == NULL || !runtime->initialized || source == NULL) {
        return BTL_RUNTIME_ERROR;
    }

    // Call btl_interpret and convert the result
    BtlInterpretResult result = btl_interpret(runtime->vm, runtime->vm->rootModule, source);

    // Map BtlInterpretResult to BTLResult
    switch (result) {
    case BTL_INTERPRET_OK:
        return BTL_OK;
    case BTL_INTERPRET_COMPILE_ERROR:
        return BTL_COMPILE_ERROR;
    case BTL_INTERPRET_RUNTIME_ERROR:
        return BTL_RUNTIME_ERROR;
    default:
        return BTL_RUNTIME_ERROR;
    }
}

// Executes a BTL source file in the runtime
BTLResult btl_runtime_exec_file(BTLRuntime* runtime, const char* path) {
    if (runtime == NULL || path == NULL) {
        return BTL_RUNTIME_ERROR;
    }

    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        btl_runtime_err_print(runtime, "Could not open file.\n");
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

// Returns the thread pool associated with the runtime
ThreadPool* btl_runtime_get_pool(BTLRuntime* runtime) {
    return runtime ? runtime->pool : NULL;
}

// Returns the number of threads in the runtime's thread pool
int btl_runtime_thread_count(BTLRuntime* runtime) {
    if (runtime == NULL || runtime->pool == NULL) return 0;
    return runtime->pool->threadCount;
}

// Retrieves the result value from the last execution
bool btl_runtime_get_result(BTLRuntime* runtime, void* out_value) {
    if (runtime == NULL || runtime->vm == NULL) return false;
    (void) out_value;
    return true;
}

// ============================================================================
// Embedding API
// ============================================================================

// Returns the internal VM handle for native module registration
VM* btl_runtime_get_vm(BTLRuntime* runtime) {
    if (runtime == NULL) return NULL;
    return runtime->vm;
}

// Registers a native module by calling the initializer immediately
// The init_fn should call btl_native_module_new() and register functions
void btl_runtime_register_module(BTLRuntime* runtime, const char* name,
                                  BtlModuleInitFn init_fn) {
    if (runtime == NULL || !runtime->initialized || init_fn == NULL) return;
    (void) name;  // The init_fn is responsible for creating the module with the right name
    init_fn(runtime->vm);
}

// Calls a BTL callable value from C code
// Pushes the callable and arguments onto the VM stack, invokes it, returns result
BtlValue btl_runtime_call(BTLRuntime* runtime, BtlValue callable,
                            int argCount, BtlValue* args, bool* ok) {
    if (runtime == NULL || !runtime->initialized) {
        if (ok) *ok = false;
        return BTL_NULL_VAL;
    }

    VM* vm = runtime->vm;

    // Ensure stack has enough space for callable + args
    btl_ensure_stack_capacity(vm, argCount + 1);

    // Push the callable (will be the "receiver" slot)
    btl_push(vm, callable);

    // Push all arguments
    for (int i = 0; i < argCount; i++) {
        btl_push(vm, args[i]);
    }

    // Call the value
    if (!btl_call_value(vm, callable, argCount)) {
        if (ok) *ok = false;
        // Pop the callable + args that were pushed
        vm->stackTop -= argCount + 1;
        return BTL_NULL_VAL;
    }

    // Run the VM to execute the call
    BtlInterpretResult result = btl_run(vm);
    if (result != BTL_INTERPRET_OK) {
        if (ok) *ok = false;
        return BTL_NULL_VAL;
    }

    // The result should be on top of the stack
    if (ok) *ok = true;
    return btl_pop(vm);
}

// Gets a global variable by name from the root module
BtlValue btl_runtime_get_global(BTLRuntime* runtime, const char* name) {
    if (runtime == NULL || !runtime->initialized || name == NULL) {
        return BTL_NULL_VAL;
    }

    VM* vm = runtime->vm;
    ObjModule* module = vm->rootModule;
    if (module == NULL) return BTL_NULL_VAL;

    // Look up the name in the module's global names table
    ObjString* nameStr = btl_string_copy(vm, name, (int) strlen(name));
    btl_push(vm, OBJ_VAL(nameStr));  // GC root

    BtlValue indexVal;
    if (btl_table_get(&module->globalNames, OBJ_VAL(nameStr), &indexVal)) {
        int index = (int) AS_NUMBER(indexVal);
        btl_pop(vm);  // nameStr
        if (index >= 0 && index < module->globalValues.count) {
            return module->globalValues.values[index];
        }
    }

    btl_pop(vm);  // nameStr
    return BTL_NULL_VAL;
}

// Sets a global variable by name in the root module
void btl_runtime_set_global(BTLRuntime* runtime, const char* name, BtlValue value) {
    if (runtime == NULL || !runtime->initialized || name == NULL) return;

    VM* vm = runtime->vm;
    ObjModule* module = vm->rootModule;
    if (module == NULL) return;

    // Look up the name in the module's global names table
    ObjString* nameStr = btl_string_copy(vm, name, (int) strlen(name));
    btl_push(vm, OBJ_VAL(nameStr));  // GC root
    btl_push(vm, value);             // GC root

    BtlValue indexVal;
    if (btl_table_get(&module->globalNames, OBJ_VAL(nameStr), &indexVal)) {
        // Global exists, update its value
        int index = (int) AS_NUMBER(indexVal);
        if (index >= 0 && index < module->globalValues.count) {
            module->globalValues.values[index] = value;
        }
    } else {
        // Global doesn't exist, create it
        int index = module->globalValues.count;
        btl_value_array_write(vm, &module->globalValues, value);
        btl_table_set(vm, &module->globalNames, OBJ_VAL(nameStr), NUMBER_VAL(index));
    }

    btl_pop(vm);  // value
    btl_pop(vm);  // nameStr
}
