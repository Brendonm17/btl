// ============================================================================
// main.c - BTL Entry Point
//
// This is the main entry point for the BTL interpreter. It handles:
// - Command line argument parsing
// - REPL (Read-Eval-Print Loop) mode for interactive use
// - File execution mode for running scripts
// ============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

// ----------------------------------------------------------------------------
// REPL - Interactive interpreter loop
//
// Reads lines from stdin and executes them until EOF.
// ----------------------------------------------------------------------------
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

// ----------------------------------------------------------------------------
// runFile - Execute a BTL script from a file
//
// Exits with code 65 for compile errors, 70 for runtime errors.
// ----------------------------------------------------------------------------
static void runFile(BTLRuntime* runtime, const char* path) {
    BTLResult result = btl_runtime_exec_file(runtime, path);

    if (result == BTL_COMPILE_ERROR) exit(65);
    if (result == BTL_RUNTIME_ERROR) exit(70);
}

// ----------------------------------------------------------------------------
// printUsage - Display help message
// ----------------------------------------------------------------------------
static void printUsage(const char* program) {
    fprintf(stderr, "BTL %s\n", btl_version());
    fprintf(stderr, "Usage: %s [options] [script]\n", program);
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -t, --threads N    Set thread pool size (default: auto)\n");
    fprintf(stderr, "  -h, --help         Show this help\n");
    fprintf(stderr, "  -v, --version      Show version\n");
}

// ----------------------------------------------------------------------------
// main - Entry point
//
// Parses command line arguments, creates runtime, runs script or REPL.
// ----------------------------------------------------------------------------
int main(int argc, const char* argv[]) {
    // Initialize default configuration
    BTLConfig config = btl_config_default();

    // Configuration can be customized here:
    // config.nursery_size = 512 * 1024;           // 512KB nursery
    // config.initial_heap_size = 2 * 1024 * 1024; // 2MB initial heap
    // config.max_heap_size = 100 * 1024 * 1024;   // 100MB max heap
    // config.gc_threshold = 512 * 1024;           // First GC at 512KB
    // config.gc_grow_factor = 1.5f;               // Heap growth factor
    // config.user_data = custom_context;          // Custom allocator context
    // config.alloc = custom_alloc;                // Custom allocation
    // config.realloc = custom_realloc;            // Custom reallocation
    // config.free = custom_free;                  // Custom deallocation
    // config.print = custom_print;                // Custom print output
    // config.error = custom_error;                // Custom error output

    // Make command line args available to scripts via system.args
    btl_set_system_args(argc, argv);
    const char* script = NULL;

    // Parse command line options
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

    // Create runtime with configuration
    BTLRuntime* runtime = btl_runtime_new(&config);
    if (!runtime) {
        fprintf(stderr, "Error: failed to create BTL runtime\n");
        return 1;
    }

    // Run script file or interactive REPL
    if (script) {
        runFile(runtime, script);
    } else {
        repl(runtime);
    }

    // Cleanup
    btl_runtime_free(runtime);
    return 0;
}
