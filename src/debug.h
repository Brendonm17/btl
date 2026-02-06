// BTL Debug Module
// Provides bytecode disassembly and debugging utilities for the BTL virtual machine.

#ifndef btl_debug_h
#define btl_debug_h

#include "chunk.h"
#include "runtime.h"

// Disassembles an entire chunk of bytecode, printing each instruction.
// Parameters:
//   runtime - The BTL runtime context for output handling
//   chunk   - The bytecode chunk to disassemble
//   name    - A descriptive name for the chunk (displayed in header)
void btl_disassemble_chunk(BTLRuntime* runtime, BtlChunk* chunk, const char* name);

// Disassembles a single instruction at the given offset.
// Parameters:
//   runtime - The BTL runtime context for output handling
//   chunk   - The bytecode chunk containing the instruction
//   offset  - The byte offset of the instruction to disassemble
// Returns:
//   The offset of the next instruction
int btl_disassemble_instruction(BTLRuntime* runtime, BtlChunk* chunk, int offset);

#endif
