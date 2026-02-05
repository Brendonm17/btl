#ifndef btl_debug_h
#define btl_debug_h

#include "chunk.h"
#include "runtime.h"

void disassembleChunk(BTLRuntime* runtime, Chunk* chunk, const char* name);
int disassembleInstruction(BTLRuntime* runtime, Chunk* chunk, int offset);

#endif