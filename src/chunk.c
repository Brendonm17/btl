// ============================================================================
// chunk.c - BTL Bytecode Chunk Implementation
//
// This file implements operations on bytecode chunks including initialization,
// memory management, writing bytes, and managing the constant pool.
// ============================================================================

#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

// ----------------------------------------------------------------------------
// btl_chunk_init
//
// Initialize a chunk to empty state. Must be called before using the chunk.
// Sets all pointers to NULL and counts to zero.
//
// Parameters:
//   chunk - The chunk to initialize
// ----------------------------------------------------------------------------
void btl_chunk_init(BtlChunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    btl_value_array_init(&chunk->constants);
}

// ----------------------------------------------------------------------------
// btl_chunk_free
//
// Free all memory used by a chunk and reset it to empty state.
// This frees the bytecode array, line info array, and constant pool.
//
// Parameters:
//   vm    - Virtual machine (for memory deallocation)
//   chunk - The chunk to free
// ----------------------------------------------------------------------------
void btl_chunk_free(struct VM* vm, BtlChunk* chunk) {
    BTL_FREE_ARRAY(vm, uint8_t, chunk->code, chunk->capacity);
    BTL_FREE_ARRAY(vm, int, chunk->lines, chunk->capacity);
    btl_value_array_free(vm, &chunk->constants);
    btl_chunk_init(chunk);
}

// ----------------------------------------------------------------------------
// btl_chunk_write
//
// Write a single byte to the chunk along with its source line number.
// If the chunk is full, it will be grown automatically. The line number
// is stored in a parallel array for error reporting and debugging.
//
// Parameters:
//   vm    - Virtual machine (for memory allocation)
//   chunk - The chunk to write to
//   byte  - The byte to write (opcode or operand)
//   line  - Source line number for this byte
// ----------------------------------------------------------------------------
void btl_chunk_write(struct VM* vm, BtlChunk* chunk, uint8_t byte, int line) {
    // Check if we need to grow the arrays
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = BTL_GROW_CAPACITY(oldCapacity);
        chunk->code = BTL_GROW_ARRAY(vm, uint8_t, chunk->code,
                                      oldCapacity, chunk->capacity);
        chunk->lines = BTL_GROW_ARRAY(vm, int, chunk->lines,
                                       oldCapacity, chunk->capacity);
    }

    // Store the byte and line number
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

// ----------------------------------------------------------------------------
// btl_chunk_add_constant
//
// Add a constant value to the chunk's constant pool. Returns the index
// where the constant was stored.
//
// The value is temporarily pushed onto the VM stack during allocation
// to protect it from garbage collection (the constant pool array might
// be reallocated, which could trigger GC).
//
// Parameters:
//   vm    - Virtual machine (for memory allocation and GC protection)
//   chunk - The chunk whose constant pool to add to
//   value - The constant value to add
//
// Returns:
//   Index of the constant in the pool (for use in bytecode operands)
// ----------------------------------------------------------------------------
int btl_chunk_add_constant(struct VM* vm, BtlChunk* chunk, BtlValue value) {
    // Push value to protect from GC during reallocation
    btl_push(vm, value);
    btl_value_array_write(vm, &chunk->constants, value);
    btl_pop(vm);
    return chunk->constants.count - 1;
}
