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

// Exits with code 65 for compile errors, 70 for runtime errors.
static void runFile(BTLRuntime* runtime, const char* path) {
    BTLResult result = btl_runtime_exec_file(runtime, path);

    if (result == BTL_COMPILE_ERROR) {
        btl_runtime_free(runtime);
        exit(65);
    }
    if (result == BTL_RUNTIME_ERROR) {
        btl_runtime_free(runtime);
        exit(70);
    }
}

static void printUsage(const char* program) {
    fprintf(stderr, "BTL %s\n", btl_version());
    fprintf(stderr, "Usage: %s [options] [script]\n", program);
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -t, --threads N    Set thread pool size (default: auto)\n");
    fprintf(stderr, "  -h, --help         Show this help\n");
    fprintf(stderr, "  -v, --version      Show version\n");
}

int main(int argc, const char* argv[]) {
    BTLConfig config = btl_config_default();

    btl_set_system_args(argc, argv);
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

    BTLRuntime* runtime = btl_runtime_new(&config);
    if (!runtime) {
        fprintf(stderr, "Error: failed to create BTL runtime\n");
        return 1;
    }

    if (script) {
        runFile(runtime, script);
    } else {
        repl(runtime);
    }

    btl_runtime_free(runtime);
    return 0;
}
