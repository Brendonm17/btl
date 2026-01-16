#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "vm.h"

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*) malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, 1, fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';
    fclose(file);
    return buffer;
}

int main(int argc, const char* argv []) {
    struct VM vm;
    initVM(&vm);

    InterpretResult result = INTERPRET_OK;

    if (argc == 1) {
        // REPL
        char line[1024];
        while (printf("> "), fgets(line, sizeof(line), stdin)) {
            result = interpret(&vm, vm.rootModule, line);
        }
    } else if (argc == 2) {
        // Run File
        char* source = readFile(argv[1]);
        result = interpret(&vm, vm.rootModule, source);
        free(source);
    } else {
        fprintf(stderr, "Usage: btl [path]\n");
        exit(64);
    }

    // Clean up
    freeVM(&vm);

    // CRITICAL for tests: Signal errors to the OS
    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);

    return 0;
}