#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "object.h"
#include "value.h"

// Helper to print values to stderr so they don't pollute stdout during tests
static void printValueStderr(Value value) {
    if (IS_BOOL(value)) {
        fprintf(stderr, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        fprintf(stderr, "null");
    } else if (IS_NUMBER(value)) {
        fprintf(stderr, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            fprintf(stderr, "%s", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION:
            fprintf(stderr, "<func %s>", AS_FUNCTION(value)->name ? AS_FUNCTION(value)->name->chars : "script");
            break;
        case OBJ_CLASS:
            fprintf(stderr, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            fprintf(stderr, "<%s instance>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_BOUND_METHOD:
            fprintf(stderr, "<method %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case OBJ_CLOSURE:
            fprintf(stderr, "<func %s>", AS_CLOSURE(value)->function->name ? AS_CLOSURE(value)->function->name->chars : "script");
            break;
        case OBJ_NATIVE:
            fprintf(stderr, "<native func>");
            break;
        case OBJ_LIST: {
            ObjList* list = AS_LIST(value);
            fprintf(stderr, "<list>[");
            for (int i = 0; i < list->items.count; i++) {
                printValueStderr(list->items.values[i]);
                if (i < list->items.count - 1) {
                    fprintf(stderr, ", ");
                }
            }
            fprintf(stderr, "]");
            break;
        }
        default:
            fprintf(stderr, "<obj>");
            break;
        }
    }
}

void disassembleChunk(Chunk* chunk, const char* name) {
    fprintf(stderr, "== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int simpleInstruction(const char* name, int offset) {
    fprintf(stderr, "%s\n", name);
    return offset + 1;
}

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    fprintf(stderr, "%-16s %4d\n", name, slot);
    return offset + 2;
}

static int shortInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t slot = (uint16_t) (chunk->code[offset + 1] |
        (chunk->code[offset + 2] << 8));
    fprintf(stderr, "%-16s %4d\n", name, slot);
    return offset + 3;
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jump = (uint16_t) (chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    fprintf(stderr, "%-16s %4d -> %d\n", name, offset,
        offset + 3 + sign * jump);
    return offset + 3;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    fprintf(stderr, "%-16s %4d '", name, constant);
    printValueStderr(chunk->constants.values[constant]);
    fprintf(stderr, "'\n");
    return offset + 2;
}

static int constantLongInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t constant = (uint16_t) (chunk->code[offset + 1] |
        (chunk->code[offset + 2] << 8));
    fprintf(stderr, "%-16s %4d '", name, constant);
    printValueStderr(chunk->constants.values[constant]);
    fprintf(stderr, "'\n");
    return offset + 3;
}

// NEW: For indexed invoke opcodes
static int indexedInvokeInstruction(const char* name, Chunk* chunk, int offset, int argCount) {
    uint8_t methodIndex = chunk->code[offset + 1];
    fprintf(stderr, "%-16s (%d args) index=%4d\n", name, argCount, methodIndex);
    return offset + 2;
}

// NEW: For OP_INVOKE and OP_INVOKE_LONG
static int invokeIndexedInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    fprintf(stderr, "%-16s (%d args) index=%4d\n", name, argCount, methodIndex);
    return offset + 3;
}

static int invokeIndexedLongInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] |
        (chunk->code[offset + 2] << 8));
    uint8_t argCount = chunk->code[offset + 3];
    fprintf(stderr, "%-16s (%d args) index=%4d\n", name, argCount, methodIndex);
    return offset + 4;
}

// NEW: For OP_METHOD
static int methodIndexedInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t arity = chunk->code[offset + 2];
    fprintf(stderr, "%-16s index=%4d arity=%d\n", name, methodIndex, arity);
    return offset + 3;
}

static int methodIndexedLongInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] |
        (chunk->code[offset + 2] << 8));
    uint8_t arity = chunk->code[offset + 3];
    fprintf(stderr, "%-16s index=%4d arity=%d\n", name, methodIndex, arity);
    return offset + 4;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    fprintf(stderr, "%04d ", offset);
    if (offset > 0 &&
        chunk->lines[offset] == chunk->lines[offset - 1]) {
        fprintf(stderr, "   | ");
    } else {
        fprintf(stderr, "%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
    case OP_CONSTANT: return constantInstruction("OP_CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG: return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_NIL: return simpleInstruction("OP_NIL", offset);
    case OP_TRUE: return simpleInstruction("OP_TRUE", offset);
    case OP_FALSE: return simpleInstruction("OP_FALSE", offset);
    case OP_0: return simpleInstruction("OP_0", offset);
    case OP_1: return simpleInstruction("OP_1", offset);
    case OP_2: return simpleInstruction("OP_2", offset);
    case OP_POP: return simpleInstruction("OP_POP", offset);
    case OP_POP_N: return byteInstruction("OP_POP_N", chunk, offset);
    case OP_GET_LOCAL: return byteInstruction("OP_GET_LOCAL", chunk, offset);
    case OP_GET_LOCAL_0: return simpleInstruction("OP_GET_LOCAL_0", offset);
    case OP_GET_LOCAL_1: return simpleInstruction("OP_GET_LOCAL_1", offset);
    case OP_GET_LOCAL_2: return simpleInstruction("OP_GET_LOCAL_2", offset);
    case OP_GET_LOCAL_3: return simpleInstruction("OP_GET_LOCAL_3", offset);
    case OP_GET_LOCAL_4: return simpleInstruction("OP_GET_LOCAL_4", offset);
    case OP_GET_LOCAL_5: return simpleInstruction("OP_GET_LOCAL_5", offset);
    case OP_GET_LOCAL_6: return simpleInstruction("OP_GET_LOCAL_6", offset);
    case OP_GET_LOCAL_7: return simpleInstruction("OP_GET_LOCAL_7", offset);
    case OP_SET_LOCAL: return byteInstruction("OP_SET_LOCAL", chunk, offset);
    case OP_SET_LOCAL_0: return simpleInstruction("OP_SET_LOCAL_0", offset);
    case OP_SET_LOCAL_1: return simpleInstruction("OP_SET_LOCAL_1", offset);
    case OP_SET_LOCAL_2: return simpleInstruction("OP_SET_LOCAL_2", offset);
    case OP_SET_LOCAL_3: return simpleInstruction("OP_SET_LOCAL_3", offset);
    case OP_SET_LOCAL_4: return simpleInstruction("OP_SET_LOCAL_4", offset);
    case OP_SET_LOCAL_5: return simpleInstruction("OP_SET_LOCAL_5", offset);
    case OP_SET_LOCAL_6: return simpleInstruction("OP_SET_LOCAL_6", offset);
    case OP_SET_LOCAL_7: return simpleInstruction("OP_SET_LOCAL_7", offset);
    case OP_SET_LOCAL_0_POP: return simpleInstruction("OP_SET_LOCAL_0_POP", offset);
    case OP_SET_LOCAL_1_POP: return simpleInstruction("OP_SET_LOCAL_1_POP", offset);
    case OP_SET_LOCAL_2_POP: return simpleInstruction("OP_SET_LOCAL_2_POP", offset);
    case OP_SET_LOCAL_3_POP: return simpleInstruction("OP_SET_LOCAL_3_POP", offset);
    case OP_SET_LOCAL_4_POP: return simpleInstruction("OP_SET_LOCAL_4_POP", offset);
    case OP_SET_LOCAL_5_POP: return simpleInstruction("OP_SET_LOCAL_5_POP", offset);
    case OP_SET_LOCAL_6_POP: return simpleInstruction("OP_SET_LOCAL_6_POP", offset);
    case OP_SET_LOCAL_7_POP: return simpleInstruction("OP_SET_LOCAL_7_POP", offset);
    case OP_INC_LOCAL_POP: return byteInstruction("OP_INC_LOCAL_POP", chunk, offset);
    case OP_INC_LOCAL: return byteInstruction("OP_INC_LOCAL", chunk, offset);
    case OP_GET_GLOBAL: return byteInstruction("OP_GET_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL_LONG: return shortInstruction("OP_GET_GLOBAL_LONG", chunk, offset);
    case OP_DEFINE_GLOBAL: return byteInstruction("OP_DEFINE_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL_LONG: return shortInstruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
    case OP_SET_GLOBAL: return byteInstruction("OP_SET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL_LONG: return shortInstruction("OP_SET_GLOBAL_LONG", chunk, offset);
    case OP_GET_UPVALUE: return byteInstruction("OP_GET_UPVALUE", chunk, offset);
    case OP_GET_UPVALUE_OPEN:   return byteInstruction("OP_GET_UPVALUE_OPEN", chunk, offset);
    case OP_GET_UPVALUE_CLOSED: return byteInstruction("OP_GET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_IMMUTABLE: return byteInstruction("OP_GET_UPVALUE_IMMUTABLE", chunk, offset);
    case OP_SET_UPVALUE: return byteInstruction("OP_SET_UPVALUE", chunk, offset);
    case OP_SET_UPVALUE_OPEN: return byteInstruction("OP_SET_UPVALUE_OPEN", chunk, offset);
    case OP_SET_UPVALUE_CLOSED: return byteInstruction("OP_SET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_0: return simpleInstruction("OP_GET_UPVALUE_0", offset);
    case OP_GET_UPVALUE_OPEN_0: return simpleInstruction("OP_GET_UPVALUE_OPEN_0", offset);
    case OP_GET_UPVALUE_CLOSED_0: return simpleInstruction("OP_GET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_IMMUTABLE_0: return simpleInstruction("OP_GET_UPVALUE_IMMUTABLE_0", offset);
    case OP_SET_UPVALUE_0: return simpleInstruction("OP_SET_UPVALUE_0", offset);
    case OP_SET_UPVALUE_OPEN_0: return simpleInstruction("OP_SET_UPVALUE_OPEN_0", offset);
    case OP_SET_UPVALUE_CLOSED_0: return simpleInstruction("OP_SET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_1: return simpleInstruction("OP_GET_UPVALUE_1", offset);
    case OP_GET_UPVALUE_OPEN_1: return simpleInstruction("OP_GET_UPVALUE_OPEN_1", offset);
    case OP_GET_UPVALUE_CLOSED_1: return simpleInstruction("OP_GET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_IMMUTABLE_1: return simpleInstruction("OP_GET_UPVALUE_IMMUTABLE_1", offset);
    case OP_SET_UPVALUE_1: return simpleInstruction("OP_SET_UPVALUE_1", offset);
    case OP_SET_UPVALUE_OPEN_1: return simpleInstruction("OP_SET_UPVALUE_OPEN_1", offset);
    case OP_SET_UPVALUE_CLOSED_1: return simpleInstruction("OP_SET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_2: return simpleInstruction("OP_GET_UPVALUE_2", offset);
    case OP_GET_UPVALUE_OPEN_2: return simpleInstruction("OP_GET_UPVALUE_OPEN_2", offset);
    case OP_GET_UPVALUE_CLOSED_2: return simpleInstruction("OP_GET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_IMMUTABLE_2: return simpleInstruction("OP_GET_UPVALUE_IMMUTABLE_2", offset);
    case OP_SET_UPVALUE_2: return simpleInstruction("OP_SET_UPVALUE_2", offset);
    case OP_SET_UPVALUE_OPEN_2: return simpleInstruction("OP_SET_UPVALUE_OPEN_2", offset);
    case OP_SET_UPVALUE_CLOSED_2: return simpleInstruction("OP_SET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_3: return simpleInstruction("OP_GET_UPVALUE_3", offset);
    case OP_GET_UPVALUE_OPEN_3: return simpleInstruction("OP_GET_UPVALUE_OPEN_3", offset);
    case OP_GET_UPVALUE_CLOSED_3: return simpleInstruction("OP_GET_UPVALUE_CLOSED_3", offset);
    case OP_GET_UPVALUE_IMMUTABLE_3: return simpleInstruction("OP_GET_UPVALUE_IMMUTABLE_3", offset);
    case OP_SET_UPVALUE_3: return simpleInstruction("OP_SET_UPVALUE_3", offset);
    case OP_SET_UPVALUE_OPEN_3: return simpleInstruction("OP_SET_UPVALUE_OPEN_3", offset);
    case OP_SET_UPVALUE_CLOSED_3: return simpleInstruction("OP_SET_UPVALUE_CLOSED_3", offset);
    case OP_FIELD: return constantInstruction("OP_FIELD", chunk, offset);
    case OP_GET_FIELD_THIS: return byteInstruction("OP_GET_FIELD_THIS", chunk, offset);
    case OP_SET_FIELD_THIS: return byteInstruction("OP_SET_FIELD_THIS", chunk, offset);
    case OP_GET_PROPERTY: return constantInstruction("OP_GET_PROPERTY", chunk, offset);
    case OP_GET_PROPERTY_LONG: return constantLongInstruction("OP_GET_PROPERTY_LONG", chunk, offset);
    case OP_SET_PROPERTY: return constantInstruction("OP_SET_PROPERTY", chunk, offset);
    case OP_SET_PROPERTY_LONG: return constantLongInstruction("OP_SET_PROPERTY_LONG", chunk, offset);
    case OP_GET_SUPER: return constantInstruction("OP_GET_SUPER", chunk, offset);
    case OP_GET_SUPER_LONG: return constantLongInstruction("OP_GET_SUPER_LONG", chunk, offset);
    case OP_EQUAL: return simpleInstruction("OP_EQUAL", offset);
    case OP_GREATER: return simpleInstruction("OP_GREATER", offset);
    case OP_LESS: return simpleInstruction("OP_LESS", offset);
    case OP_ADD: return simpleInstruction("OP_ADD", offset);
    case OP_SUBTRACT: return simpleInstruction("OP_SUBTRACT", offset);
    case OP_MULTIPLY: return simpleInstruction("OP_MULTIPLY", offset);
    case OP_DIVIDE: return simpleInstruction("OP_DIVIDE", offset);
    case OP_MODULO: return simpleInstruction("OP_MODULO", offset);
    case OP_NOT: return simpleInstruction("OP_NOT", offset);
    case OP_NEGATE: return simpleInstruction("OP_NEGATE", offset);
    case OP_PRINT: return simpleInstruction("OP_PRINT", offset);
    case OP_JUMP: return jumpInstruction("OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE: return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_POP_JUMP_IF_FALSE: return jumpInstruction("OP_POP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_JUMP_IF_TRUE: return jumpInstruction("OP_JUMP_IF_TRUE", 1, chunk, offset);
    case OP_POP_JUMP_IF_TRUE: return jumpInstruction("OP_POP_JUMP_IF_TRUE", 1, chunk, offset);
    case OP_JUMP_IF_NOT_EQUAL: return jumpInstruction("OP_JUMP_IF_NOT_EQUAL", 1, chunk, offset);
    case OP_JUMP_IF_EQUAL: return jumpInstruction("OP_JUMP_IF_EQUAL", 1, chunk, offset);
    case OP_JUMP_IF_NOT_GREATER: return jumpInstruction("OP_JUMP_IF_NOT_GREATER", 1, chunk, offset);
    case OP_JUMP_IF_NOT_LESS: return jumpInstruction("OP_JUMP_IF_NOT_LESS", 1, chunk, offset);
    case OP_LOOP: return jumpInstruction("OP_LOOP", -1, chunk, offset);
    case OP_CALL_0: return simpleInstruction("OP_CALL_0", offset);
    case OP_CALL_1: return simpleInstruction("OP_CALL_1", offset);
    case OP_CALL_2: return simpleInstruction("OP_CALL_2", offset);
    case OP_CALL_3: return simpleInstruction("OP_CALL_3", offset);
    case OP_CALL_4: return simpleInstruction("OP_CALL_4", offset);
    case OP_CALL_5: return simpleInstruction("OP_CALL_5", offset);
    case OP_CALL_6: return simpleInstruction("OP_CALL_6", offset);
    case OP_CALL_7: return simpleInstruction("OP_CALL_7", offset);
    case OP_CALL_8: return simpleInstruction("OP_CALL_8", offset);
    case OP_CALL: return byteInstruction("OP_CALL", chunk, offset);
    case OP_TAIL_CALL_0: return simpleInstruction("OP_TAIL_CALL_0", offset);
    case OP_TAIL_CALL_1: return simpleInstruction("OP_TAIL_CALL_1", offset);
    case OP_TAIL_CALL_2: return simpleInstruction("OP_TAIL_CALL_2", offset);
    case OP_TAIL_CALL_3: return simpleInstruction("OP_TAIL_CALL_3", offset);
    case OP_TAIL_CALL_4: return simpleInstruction("OP_TAIL_CALL_4", offset);
    case OP_TAIL_CALL_5: return simpleInstruction("OP_TAIL_CALL_5", offset);
    case OP_TAIL_CALL_6: return simpleInstruction("OP_TAIL_CALL_6", offset);
    case OP_TAIL_CALL_7: return simpleInstruction("OP_TAIL_CALL_7", offset);
    case OP_TAIL_CALL_8: return simpleInstruction("OP_TAIL_CALL_8", offset);
    case OP_TAIL_CALL: return byteInstruction("OP_TAIL_CALL", chunk, offset);

        // OPTIMIZED: Indexed invoke opcodes (replaces old string-based OP_INVOKE)
    case OP_INVOKE_0: return indexedInvokeInstruction("OP_INVOKE_IDX_0", chunk, offset, 0);
    case OP_INVOKE_1: return indexedInvokeInstruction("OP_INVOKE_IDX_1", chunk, offset, 1);
    case OP_INVOKE_2: return indexedInvokeInstruction("OP_INVOKE_IDX_2", chunk, offset, 2);
    case OP_INVOKE_3: return indexedInvokeInstruction("OP_INVOKE_IDX_3", chunk, offset, 3);
    case OP_INVOKE_4: return indexedInvokeInstruction("OP_INVOKE_IDX_4", chunk, offset, 4);
    case OP_INVOKE_5: return indexedInvokeInstruction("OP_INVOKE_IDX_5", chunk, offset, 5);
    case OP_INVOKE_6: return indexedInvokeInstruction("OP_INVOKE_IDX_6", chunk, offset, 6);
    case OP_INVOKE_7: return indexedInvokeInstruction("OP_INVOKE_IDX_7", chunk, offset, 7);
    case OP_INVOKE_8: return indexedInvokeInstruction("OP_INVOKE_IDX_8", chunk, offset, 8);
    case OP_INVOKE: return invokeIndexedInstruction("OP_INVOKE_IDX", chunk, offset);
    case OP_INVOKE_LONG: return invokeIndexedLongInstruction("OP_INVOKE_IDX_L", chunk, offset);

    case OP_TAIL_INVOKE_0: return indexedInvokeInstruction("OP_T_INVOKE_IDX_0", chunk, offset, 0);
    case OP_TAIL_INVOKE_1: return indexedInvokeInstruction("OP_T_INVOKE_IDX_1", chunk, offset, 1);
    case OP_TAIL_INVOKE_2: return indexedInvokeInstruction("OP_T_INVOKE_IDX_2", chunk, offset, 2);
    case OP_TAIL_INVOKE_3: return indexedInvokeInstruction("OP_T_INVOKE_IDX_3", chunk, offset, 3);
    case OP_TAIL_INVOKE_4: return indexedInvokeInstruction("OP_T_INVOKE_IDX_4", chunk, offset, 4);
    case OP_TAIL_INVOKE_5: return indexedInvokeInstruction("OP_T_INVOKE_IDX_5", chunk, offset, 5);
    case OP_TAIL_INVOKE_6: return indexedInvokeInstruction("OP_T_INVOKE_IDX_6", chunk, offset, 6);
    case OP_TAIL_INVOKE_7: return indexedInvokeInstruction("OP_T_INVOKE_IDX_7", chunk, offset, 7);
    case OP_TAIL_INVOKE_8: return indexedInvokeInstruction("OP_T_INVOKE_IDX_8", chunk, offset, 8);
    case OP_TAIL_INVOKE: return invokeIndexedInstruction("OP_T_INVOKE_IDX", chunk, offset);
    case OP_TAIL_INVOKE_LONG: return invokeIndexedLongInstruction("OP_T_INVOKE_IDX_L", chunk, offset);

    case OP_SUPER_INVOKE_0: return indexedInvokeInstruction("OP_S_INVOKE_IDX_0", chunk, offset, 0);
    case OP_SUPER_INVOKE_1: return indexedInvokeInstruction("OP_S_INVOKE_IDX_1", chunk, offset, 1);
    case OP_SUPER_INVOKE_2: return indexedInvokeInstruction("OP_S_INVOKE_IDX_2", chunk, offset, 2);
    case OP_SUPER_INVOKE_3: return indexedInvokeInstruction("OP_S_INVOKE_IDX_3", chunk, offset, 3);
    case OP_SUPER_INVOKE_4: return indexedInvokeInstruction("OP_S_INVOKE_IDX_4", chunk, offset, 4);
    case OP_SUPER_INVOKE_5: return indexedInvokeInstruction("OP_S_INVOKE_IDX_5", chunk, offset, 5);
    case OP_SUPER_INVOKE_6: return indexedInvokeInstruction("OP_S_INVOKE_IDX_6", chunk, offset, 6);
    case OP_SUPER_INVOKE_7: return indexedInvokeInstruction("OP_S_INVOKE_IDX_7", chunk, offset, 7);
    case OP_SUPER_INVOKE_8: return indexedInvokeInstruction("OP_S_INVOKE_IDX_8", chunk, offset, 8);
    case OP_SUPER_INVOKE: return invokeIndexedInstruction("OP_S_INVOKE_IDX", chunk, offset);
    case OP_SUPER_INVOKE_LONG: return invokeIndexedLongInstruction("OP_S_INVOKE_IDX_L", chunk, offset);

    case OP_TAIL_SUPER_INVOKE_0: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_0", chunk, offset, 0);
    case OP_TAIL_SUPER_INVOKE_1: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_1", chunk, offset, 1);
    case OP_TAIL_SUPER_INVOKE_2: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_2", chunk, offset, 2);
    case OP_TAIL_SUPER_INVOKE_3: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_3", chunk, offset, 3);
    case OP_TAIL_SUPER_INVOKE_4: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_4", chunk, offset, 4);
    case OP_TAIL_SUPER_INVOKE_5: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_5", chunk, offset, 5);
    case OP_TAIL_SUPER_INVOKE_6: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_6", chunk, offset, 6);
    case OP_TAIL_SUPER_INVOKE_7: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_7", chunk, offset, 7);
    case OP_TAIL_SUPER_INVOKE_8: return indexedInvokeInstruction("OP_TS_INVOKE_IDX_8", chunk, offset, 8);
    case OP_TAIL_SUPER_INVOKE: return invokeIndexedInstruction("OP_TS_INVOKE_IDX", chunk, offset);
    case OP_TAIL_SUPER_INVOKE_LONG: return invokeIndexedLongInstruction("OP_TS_INVOKE_IDX_L", chunk, offset);

    case OP_CLOSURE: {
        uint8_t constant = chunk->code[offset + 1];
        fprintf(stderr, "%-16s %4d ", "OP_CLOSURE", constant);
        printValueStderr(chunk->constants.values[constant]);
        fprintf(stderr, "\n");
        ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
        int newOffset = offset + 2;
        for (int i = 0; i < function->upvalueCount; i++) {
            uint8_t isLocal = chunk->code[newOffset++];
            uint8_t index = chunk->code[newOffset++];
            uint8_t isMut = chunk->code[newOffset++];
            fprintf(stderr, "%04d      |                     %s %d (%s)\n",
                newOffset - 3, isLocal ? "local" : "upvalue", index,
                isMut ? "mutable" : "immutable");
        }
        return newOffset;
    }
    case OP_CLOSURE_LONG: {
        uint16_t constant = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
        fprintf(stderr, "%-16s %4d ", "OP_CLOSURE_LONG", constant);
        printValueStderr(chunk->constants.values[constant]);
        fprintf(stderr, "\n");
        ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
        int newOffset = offset + 3;
        for (int i = 0; i < function->upvalueCount; i++) {
            uint8_t isLocal = chunk->code[newOffset++];
            uint8_t index = chunk->code[newOffset++];
            uint8_t isMut = chunk->code[newOffset++];
            fprintf(stderr, "%04d      |                     %s %d (%s)\n",
                newOffset - 3, isLocal ? "local" : "upvalue", index,
                isMut ? "mutable" : "immutable");
        }
        return newOffset;
    }
    case OP_CLOSE_UPVALUE: return simpleInstruction("OP_CLOSE_UPVALUE", offset);
    case OP_RETURN: return simpleInstruction("OP_RETURN", offset);
    case OP_CLASS: return constantInstruction("OP_CLASS", chunk, offset);
    case OP_CLASS_LONG: return constantLongInstruction("OP_CLASS_LONG", chunk, offset);
    case OP_INHERIT: return simpleInstruction("OP_INHERIT", offset);

        // OPTIMIZED: Indexed method definition (replaces old OP_METHOD)
    case OP_METHOD: return methodIndexedInstruction("OP_METHOD_IDX", chunk, offset);
    case OP_METHOD_LONG: return methodIndexedLongInstruction("OP_METHOD_IDX_L", chunk, offset);

    case OP_BUILD_LIST: return byteInstruction("OP_BUILD_LIST", chunk, offset);
    case OP_INDEX_GET: return simpleInstruction("OP_INDEX_GET", offset);
    case OP_INDEX_SET: return simpleInstruction("OP_INDEX_SET", offset);
    case OP_IMPORT: return constantInstruction("OP_IMPORT", chunk, offset);
    case OP_IMPORT_LONG: return constantLongInstruction("OP_IMPORT_LONG", chunk, offset);
    default:
        fprintf(stderr, "Unknown opcode %d\n", instruction);
        return offset + 1;
    }
}