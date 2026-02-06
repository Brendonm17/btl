// ============================================================================
// compiled_main.c - Entry Point for Transpiled BTL Binary
//
// This replaces main.c for the compiled output. It reads the original
// source file (needed for compile()), initializes the VM, and calls
// the transpiled entry point btl_compiled_run().
// ============================================================================

#include "compiled.h"
#include "../src/runtime.h"

#include <stdio.h>
#include <stdlib.h>

// Declared in the generated .c file
extern BtlInterpretResult btl_compiled_run(VM* vm, ObjModule* module, const char* source);

// ----------------------------------------------------------------------------
// File I/O
// ----------------------------------------------------------------------------

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error: Could not open file \"%s\".\n", path);
        return NULL;
    }
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*) malloc(fileSize + 1);
    if (buffer == NULL) {
        fclose(file);
        return NULL;
    }
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

// ----------------------------------------------------------------------------
// Main Entry Point
// ----------------------------------------------------------------------------

int main(int argc, char* argv []) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <script.btl>\n", argv[0]);
        return 1;
    }

    char* source = readFile(argv[1]);
    if (source == NULL) return 1;

    btl_set_system_args(argc, (const char**) argv);

    BTLConfig cfg = btl_config_default();
    BTLRuntime* rt = btl_runtime_new(&cfg);
    if (!rt) {
        fprintf(stderr, "Error: failed to create BTL runtime\n");
        return 1;
    }
    BtlInterpretResult result = btl_compiled_run(rt->vm, rt->vm->rootModule, source);

    free(source);
    btl_runtime_free(rt);

    if (result == BTL_INTERPRET_COMPILE_ERROR) return 65;
    if (result == BTL_INTERPRET_RUNTIME_ERROR) return 70;
    return 0;
}
