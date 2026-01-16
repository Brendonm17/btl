#include <stdio.h>
#include "debug.h"
#include "object.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    fprintf(stderr, "== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}
static int simple(const char* n, int o) {
    fprintf(stderr, "%s\n", n); return o + 1;
}
static int byte(const char* n, Chunk* c, int o) {
    fprintf(stderr, "%-16s %4d\n", n, c->code[o + 1]); return o + 2;
}
static int constant(const char* n, Chunk* c, int o) {
    fprintf(stderr, "%-16s %4d '", n, c->code[o + 1]);
    printValueStderr(c->constants.values[c->code[o + 1]]);
    fprintf(stderr, "'\n"); return o + 2;
}
static int jump(const char* n, int s, Chunk* c, int o) {
    uint16_t j = (uint16_t) (c->code[o + 1] << 8) | c->code[o + 2];
    fprintf(stderr, "%-16s %4d -> %d\n", n, o, o + 3 + s * j); return o + 3;
}
int disassembleInstruction(Chunk* chunk, int offset) {
    fprintf(stderr, "%04d ", offset);
    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
    case OP_CONSTANT: return constant("OP_CONSTANT", chunk, offset);
    case OP_NIL: return simple("OP_NIL", offset);
    case OP_TRUE: return simple("OP_TRUE", offset);
    case OP_FALSE: return simple("OP_FALSE", offset);
    case OP_POP: return simple("OP_POP", offset);
    case OP_GET_LOCAL: return byte("OP_GET_LOCAL", chunk, offset);
    case OP_SET_LOCAL: return byte("OP_SET_LOCAL", chunk, offset);
    case OP_GET_GLOBAL: return byte("OP_GET_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL: return byte("OP_DEFINE_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL: return byte("OP_SET_GLOBAL", chunk, offset);
    case OP_GET_UPVALUE: return byte("OP_GET_UPVALUE", chunk, offset);
    case OP_SET_UPVALUE: return byte("OP_SET_UPVALUE", chunk, offset);
    case OP_GET_PROPERTY: return constant("OP_GET_PROPERTY", chunk, offset);
    case OP_SET_PROPERTY: return constant("OP_SET_PROPERTY", chunk, offset);
    case OP_GET_SUPER: return constant("OP_GET_SUPER", chunk, offset);
    case OP_EQUAL: return simple("OP_EQUAL", offset);
    case OP_GREATER: return simple("OP_GREATER", offset);
    case OP_LESS: return simple("OP_LESS", offset);
    case OP_ADD: return simple("OP_ADD", offset);
    case OP_SUBTRACT: return simple("OP_SUBTRACT", offset);
    case OP_MULTIPLY: return simple("OP_MULTIPLY", offset);
    case OP_DIVIDE: return simple("OP_DIVIDE", offset);
    case OP_NOT: return simple("OP_NOT", offset);
    case OP_NEGATE: return simple("OP_NEGATE", offset);
    case OP_PRINT: return simple("OP_PRINT", offset);
    case OP_JUMP: return jump("OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE: return jump("OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_LOOP: return jump("OP_LOOP", -1, chunk, offset);
    case OP_CALL: return byte("OP_CALL", chunk, offset);
    case OP_INVOKE: return constant("OP_INVOKE", chunk, offset);
    case OP_SUPER_INVOKE: return constant("OP_SUPER_INVOKE", chunk, offset);
    case OP_CLOSURE: {
        offset++; uint8_t con = chunk->code[offset++];
        fprintf(stderr, "%-16s %4d ", "OP_CLOSURE", con);
        printValueStderr(chunk->constants.values[con]);
        fprintf(stderr, "\n");
        ObjFunction* f = AS_FUNCTION(chunk->constants.values[con]);
        for (int j = 0; j < f->upvalueCount; j++) {
            int isL = chunk->code[offset++]; int idx = chunk->code[offset++];
            fprintf(stderr, "%04d      |                     %s %d\n", offset - 2, isL ? "local" : "upvalue", idx);
        }
        return offset;
    }
    case OP_CLOSE_UPVALUE: return simple("OP_CLOSE_UPVALUE", offset);
    case OP_RETURN: return simple("OP_RETURN", offset);
    case OP_CLASS: return constant("OP_CLASS", chunk, offset);
    case OP_INHERIT: return simple("OP_INHERIT", offset);
    case OP_METHOD: return constant("OP_METHOD", chunk, offset);
    case OP_BUILD_LIST: return byte("OP_BUILD_LIST", chunk, offset);
    case OP_INDEX_GET: return simple("OP_INDEX_GET", offset);
    case OP_INDEX_SET: return simple("OP_INDEX_SET", offset);
    case OP_IMPORT: return constant("OP_IMPORT", chunk, offset);
    default: return offset + 1;
    }
}