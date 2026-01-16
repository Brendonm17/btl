#include <stdio.h>
#include <stdlib.h>
#include "vm.h"

int main(int argc, const char* argv []) {
    struct VM vm; initVM(&vm);
    if (argc == 1) {
        char line[1024];
        while (printf("> "), fgets(line, sizeof(line), stdin)) interpret(&vm, vm.rootModule, line);
    } else if (argc == 2) {
        FILE* file = fopen(argv[1], "rb"); if (!file) return 74;
        fseek(file, 0L, SEEK_END); size_t size = ftell(file); rewind(file);
        char* src = malloc(size + 1); size_t r = fread(src, 1, size, file); src[r] = '\0'; fclose(file);
        interpret(&vm, vm.rootModule, src); free(src);
    }
    freeVM(&vm); return 0;
}