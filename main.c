#include <stdio.h>
#include <stdlib.h>
#include "vm.h"

int main(int argc, const char* argv []) {
    VM vm; initVM(&vm);
    if (argc == 1) {
        char line[1024];
        while (printf("> "), fgets(line, sizeof(line), stdin))
            interpret(&vm, line);
    } else if (argc == 2) {
        FILE* f = fopen(argv[1], "rb");
        fseek(f, 0, SEEK_END);
        long size = ftell(f);
        rewind(f);
        char* src = malloc(size + 1);
        fread(src, 1, size, f);
        src[size] = '\0';
        fclose(f);
        interpret(&vm, src);
        free(src);
    }
    freeVM(&vm);
    return 0;
}