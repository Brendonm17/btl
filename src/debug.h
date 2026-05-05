#ifndef btl_debug_h
#define btl_debug_h

#include "chunk.h"
#include "runtime.h"

void btl_disassemble_chunk(BTLRuntime* runtime, BtlChunk* chunk, const char* name);

// Returns the offset of the next instruction.
int btl_disassemble_instruction(BTLRuntime* runtime, BtlChunk* chunk, int offset);

#endif
