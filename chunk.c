#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0; chunk->capacity = 0; chunk->code = NULL; chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void freeChunk(struct VM* vm, Chunk* chunk) {
    FREE_ARRAY(vm, uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(vm, int, chunk->lines, chunk->capacity);
    freeValueArray(vm, &chunk->constants);
    initChunk(chunk);
}

void writeChunk(struct VM* vm, Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int old = chunk->capacity; chunk->capacity = GROW_CAPACITY(old);
        chunk->code = GROW_ARRAY(vm, uint8_t, chunk->code, old, chunk->capacity);
        chunk->lines = GROW_ARRAY(vm, int, chunk->lines, old, chunk->capacity);
    }
    chunk->code[chunk->count] = byte; chunk->lines[chunk->count] = line; chunk->count++;
}

int addConstant(struct VM* vm, Chunk* chunk, Value value) {
    push(vm, value); writeValueArray(vm, &chunk->constants, value); pop(vm);
    return chunk->constants.count - 1;
}