#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

static void repl(BTLRuntime* runtime) {
    char line[1024];
    for (;;) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        btl_runtime_exec(runtime, line);
    }
}

static void runFile(BTLRuntime* runtime, const char* path) {
    BTLResult result = btl_runtime_exec_file(runtime, path);

    if (result == BTL_COMPILE_ERROR) exit(65);
    if (result == BTL_RUNTIME_ERROR) exit(70);
}

static void printUsage(const char* program) {
    fprintf(stderr, "BTL %s\n", btl_version());
    fprintf(stderr, "Usage: %s [options] [script]\n", program);
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -t, --threads N    Set thread pool size (default: auto)\n");
    fprintf(stderr, "  -h, --help         Show this help\n");
    fprintf(stderr, "  -v, --version      Show version\n");
}

int main(int argc, const char* argv []) {
    // Parse command line options
    BTLConfig config = btl_config_default();

    /*
    // Override specific settings
    config.nursery_size = 512 * 1024;           // 512KB nursery instead of 256KB
    config.initial_heap_size = 2 * 1024 * 1024; // 2MB initial heap
    config.max_heap_size = 100 * 1024 * 1024;   // 100MB max heap
    config.gc_threshold = 512 * 1024;           // First GC at 512KB
    config.gc_grow_factor = 1.5f;               // Grow heap by 1.5x after GC

    // Custom allocators (e.g., for embedding in a game engine)
    config.user_data = my_allocator_context;
    config.alloc = my_alloc;
    config.realloc = my_realloc;
    config.free = my_free;

    // Custom I/O (e.g., redirect to a log file or GUI console)
    config.print = my_print_handler;
    config.error = my_error_handler;
    */

    setSystemArgs(argc, argv);
    const char* script = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-t") == 0 || strcmp(argv[i], "--threads") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: --threads requires a number\n");
                return 1;
            }
            config.thread_count = atoi(argv[++i]);
            if (config.thread_count == 0) {
                fprintf(stderr, "Error: invalid thread count\n");
                return 1;
            }
        } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            printUsage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--version") == 0) {
            printf("BTL %s\n", btl_version());
            return 0;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            printUsage(argv[0]);
            return 1;
        } else {
            script = argv[i];
        }
    }

    // Create runtime
    BTLRuntime* runtime = btl_runtime_new(&config);
    if (!runtime) {
        fprintf(stderr, "Error: failed to create BTL runtime\n");
        return 1;
    }

    //printf("BTL %s (using %d threads)\n", btl_version(), btl_runtime_thread_count(runtime));

    // Run script or REPL
    if (script) {
        runFile(runtime, script);
    } else {
        repl(runtime);
    }

    // Cleanup
    btl_runtime_free(runtime);
    return 0;
}