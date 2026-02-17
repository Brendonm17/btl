// ============================================================================
// transpiler_main.c - BTL Transpiler Command-line Driver
//
// Usage:  ./transpiler <input.btl> <output.c>
//
// 1. Initializes a BTL VM
// 2. Compiles the source to bytecode (using the existing compiler)
// 3. Walks all functions and emits optimized C
// ============================================================================

#include "transpiler.h"
#include "../src/vm.h"
#include "../src/compiler.h"
#include "../src/object.h"
#include "../src/memory.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
        fprintf(stderr, "Error: Not enough memory to read \"%s\".\n", path);
        fclose(file);
        return NULL;
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Error: Could not read file \"%s\".\n", path);
        free(buffer);
        fclose(file);
        return NULL;
    }

    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

// ----------------------------------------------------------------------------
// Main Entry Point
// ----------------------------------------------------------------------------

int main(int argc, char* argv []) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <input.btl> <output.c>\n", argv[0]);
        fprintf(stderr, "\nTranspiles a BTL source file to optimized C.\n");
        fprintf(stderr, "Options:\n");
        fprintf(stderr, "  --comments    Include bytecode offset comments\n");
        fprintf(stderr, "  --lines       Include source line info\n");
        fprintf(stderr, "  --checks      Include bounds/type checks\n");
        return 1;
    }

    const char* inputPath = argv[1];
    const char* outputPath = argv[2];

    // Parse optional flags
    bool comments = false;
    bool lines = false;
    bool checks = false;
    for (int i = 3; i < argc; i++) {
        if (strcmp(argv[i], "--comments") == 0) comments = true;
        else if (strcmp(argv[i], "--lines") == 0) lines = true;
        else if (strcmp(argv[i], "--checks") == 0) checks = true;
        else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        }
    }

    // Read source file
    char* source = readFile(inputPath);
    if (source == NULL) return 1;

    // Initialize VM and compile to bytecode
    // Zero-initialize before btl_vm_init (required on MSVC where stack memory is uninitialized)
    VM vm;
    memset(&vm, 0, sizeof(VM));
    btl_vm_init(&vm);

    ObjModule* module = btl_module_new(&vm, btl_string_copy(&vm, "<main>", 6));
    vm.rootModule = module;

    ObjFunction* mainFn = btl_compile(&vm, module, source);
    if (mainFn == NULL) {
        fprintf(stderr, "Error: Compilation failed.\n");
        free(source);
        btl_vm_free(&vm, true);
        return 1;
    }

    // Create transpiler and emit C
    BtlTranspilerConfig config = {
        .emit_comments = comments,
        .emit_line_info = lines,
        .bounds_checks = checks,
        .output_path = outputPath,
    };

    BtlTranspiler* t = btl_transpiler_new(config, &vm);
    if (t == NULL) {
        fprintf(stderr, "Error: Could not create output file \"%s\".\n", outputPath);
        free(source);
        btl_vm_free(&vm, true);
        return 1;
    }

    fprintf(stderr, "Transpiling %s -> %s\n", inputPath, outputPath);

    if (!btl_transpiler_emit_program(t, mainFn, module)) {
        fprintf(stderr, "Error: Transpilation failed.\n");
        btl_transpiler_free(t);
        free(source);
        btl_vm_free(&vm, true);
        return 1;
    }

    btl_transpiler_free(t);
    free(source);
    btl_vm_free(&vm, true);

    fprintf(stderr, "Done.\n");
    return 0;
}
