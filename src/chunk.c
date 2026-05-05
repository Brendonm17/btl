#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

void btl_chunk_init(BtlChunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    btl_value_array_init(&chunk->constants);
}

void btl_chunk_free(struct VM* vm, BtlChunk* chunk) {
    BTL_FREE_ARRAY(vm, uint8_t, chunk->code, chunk->capacity);
    BTL_FREE_ARRAY(vm, int, chunk->lines, chunk->capacity);
    btl_value_array_free(vm, &chunk->constants);
    btl_chunk_init(chunk);
}

void btl_chunk_write(struct VM* vm, BtlChunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = BTL_GROW_CAPACITY(oldCapacity);
        chunk->code = BTL_GROW_ARRAY(vm, uint8_t, chunk->code,
                                      oldCapacity, chunk->capacity);
        chunk->lines = BTL_GROW_ARRAY(vm, int, chunk->lines,
                                       oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

// Push value first so it's protected from GC during the array growth.
int btl_chunk_add_constant(struct VM* vm, BtlChunk* chunk, BtlValue value) {
    btl_push(vm, value);
    btl_value_array_write(vm, &chunk->constants, value);
    btl_pop(vm);
    return chunk->constants.count - 1;
}
