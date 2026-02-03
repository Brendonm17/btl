#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "object.h"
#include "value.h"

static void printValueStderr(Value value) {
    if (IS_BOOL(value)) {
        fprintf(stderr, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        fprintf(stderr, "null");
    } else if (IS_NUMBER(value)) {
        fprintf(stderr, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            fprintf(stderr, "'%s'", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            fprintf(stderr, "<fn %s>", fn->name ? fn->name->chars : "script");
            break;
        }
        case OBJ_CLASS:
            fprintf(stderr, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            fprintf(stderr, "<%s>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_BOUND_METHOD:
            fprintf(stderr, "<bound %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case OBJ_CLOSURE: {
            ObjClosure* cl = AS_CLOSURE(value);
            fprintf(stderr, "<fn %s>", cl->function->name ? cl->function->name->chars : "script");
            break;
        }
        case OBJ_NATIVE:
            fprintf(stderr, "<native>");
            break;
        case OBJ_LIST:
            fprintf(stderr, "<list[%d]>", AS_LIST(value)->items.count);
            break;
        case OBJ_TABLE:
            fprintf(stderr, "<table>");
            break;
        case OBJ_MODULE:
            fprintf(stderr, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case OBJ_UPVALUE:
            fprintf(stderr, "<upvalue>");
            break;
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

// ============================================================================
// Instruction Printers
// ============================================================================

static int simple(const char* name, int offset) {
    fprintf(stderr, "%s\n", name);
    return offset + 1;
}

static int byte(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    fprintf(stderr, "%-24s %d\n", name, slot);
    return offset + 2;
}

static int shortOp(const char* name, Chunk* chunk, int offset) {
    uint16_t slot = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    fprintf(stderr, "%-24s %d\n", name, slot);
    return offset + 3;
}

static int jump(const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jmp = (uint16_t) (chunk->code[offset + 1] << 8 | chunk->code[offset + 2]);
    fprintf(stderr, "%-24s %d -> %d\n", name, offset, offset + 3 + sign * jmp);
    return offset + 3;
}

static int constant(const char* name, Chunk* chunk, int offset) {
    uint8_t idx = chunk->code[offset + 1];
    fprintf(stderr, "%-24s [%d] ", name, idx);
    printValueStderr(chunk->constants.values[idx]);
    fprintf(stderr, "\n");
    return offset + 2;
}

static int constantLong(const char* name, Chunk* chunk, int offset) {
    uint16_t idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    fprintf(stderr, "%-24s [%d] ", name, idx);
    printValueStderr(chunk->constants.values[idx]);
    fprintf(stderr, "\n");
    return offset + 3;
}

// OP_INVOKE_0 - OP_INVOKE_8: [methodIndex]
static int invokeIndexed(const char* name, int argCount, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    fprintf(stderr, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 2;
}

// OP_INVOKE: [methodIndex] [argCount]
static int invoke(const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    fprintf(stderr, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 3;
}

// OP_INVOKE_LONG: [methodIndex:16] [argCount]
static int invokeLong(const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t argCount = chunk->code[offset + 3];
    fprintf(stderr, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 4;
}

// OP_INVOKE_IC: [nameIdx] [argCount] [icSlot]
static int invokeIC(const char* name, Chunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    uint8_t icSlot = chunk->code[offset + 3];
    fprintf(stderr, "%-24s '%s' args=%d ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, argCount, icSlot);
    return offset + 4;
}

// OP_GET/SET_PROPERTY_IC: [nameIdx] [icSlot]
static int propertyIC(const char* name, Chunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t icSlot = chunk->code[offset + 2];
    fprintf(stderr, "%-24s '%s' ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, icSlot);
    return offset + 3;
}

// OP_METHOD: [methodIndex] [arity]
static int method(const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t arity = chunk->code[offset + 2];
    fprintf(stderr, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 3;
}

// OP_METHOD_LONG: [methodIndex:16] [arity]
static int methodLong(const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t arity = chunk->code[offset + 3];
    fprintf(stderr, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 4;
}

// OP_CLOSURE / OP_CLOSURE_LONG
static int closure(const char* name, Chunk* chunk, int offset, bool isLong) {
    int idx;
    int base;
    if (isLong) {
        idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
        base = offset + 3;
    } else {
        idx = chunk->code[offset + 1];
        base = offset + 2;
    }

    fprintf(stderr, "%-24s [%d] ", name, idx);
    printValueStderr(chunk->constants.values[idx]);
    fprintf(stderr, "\n");

    ObjFunction* fn = AS_FUNCTION(chunk->constants.values[idx]);
    for (int i = 0; i < fn->upvalueCount; i++) {
        uint8_t isLocal = chunk->code[base++];
        uint8_t index = chunk->code[base++];
        uint8_t isMut = chunk->code[base++];
        fprintf(stderr, "     |                          %s[%d] %s\n",
            isLocal ? "local" : "upval", index, isMut ? "mut" : "imm");
    }
    return base;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    fprintf(stderr, "%04d ", offset);

    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        fprintf(stderr, "   | ");
    } else {
        fprintf(stderr, "%4d ", chunk->lines[offset]);
    }

    uint8_t op = chunk->code[offset];

    switch (op) {
        // Constants
    case OP_CONSTANT:           return constant("CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:      return constantLong("CONSTANT_LONG", chunk, offset);
    case OP_NULL:                return simple("NULL", offset);
    case OP_TRUE:               return simple("TRUE", offset);
    case OP_FALSE:              return simple("FALSE", offset);
    case OP_0:                  return simple("PUSH_0", offset);
    case OP_1:                  return simple("PUSH_1", offset);
    case OP_2:                  return simple("PUSH_2", offset);

        // Stack
    case OP_POP:                return simple("POP", offset);
    case OP_POP_N:              return byte("POP_N", chunk, offset);
    case OP_DUP:                return simple("DUP", offset);
    case OP_SWAP:               return simple("SWAP", offset);

        // Locals
    case OP_GET_LOCAL:          return byte("GET_LOCAL", chunk, offset);
    case OP_GET_LOCAL_0:        return simple("GET_LOCAL_0", offset);
    case OP_GET_LOCAL_1:        return simple("GET_LOCAL_1", offset);
    case OP_GET_LOCAL_2:        return simple("GET_LOCAL_2", offset);
    case OP_GET_LOCAL_3:        return simple("GET_LOCAL_3", offset);
    case OP_GET_LOCAL_4:        return simple("GET_LOCAL_4", offset);
    case OP_GET_LOCAL_5:        return simple("GET_LOCAL_5", offset);
    case OP_GET_LOCAL_6:        return simple("GET_LOCAL_6", offset);
    case OP_GET_LOCAL_7:        return simple("GET_LOCAL_7", offset);
    case OP_SET_LOCAL:          return byte("SET_LOCAL", chunk, offset);
    case OP_SET_LOCAL_0:        return simple("SET_LOCAL_0", offset);
    case OP_SET_LOCAL_1:        return simple("SET_LOCAL_1", offset);
    case OP_SET_LOCAL_2:        return simple("SET_LOCAL_2", offset);
    case OP_SET_LOCAL_3:        return simple("SET_LOCAL_3", offset);
    case OP_SET_LOCAL_4:        return simple("SET_LOCAL_4", offset);
    case OP_SET_LOCAL_5:        return simple("SET_LOCAL_5", offset);
    case OP_SET_LOCAL_6:        return simple("SET_LOCAL_6", offset);
    case OP_SET_LOCAL_7:        return simple("SET_LOCAL_7", offset);
    case OP_SET_LOCAL_0_POP:    return simple("SET_LOCAL_0_POP", offset);
    case OP_SET_LOCAL_1_POP:    return simple("SET_LOCAL_1_POP", offset);
    case OP_SET_LOCAL_2_POP:    return simple("SET_LOCAL_2_POP", offset);
    case OP_SET_LOCAL_3_POP:    return simple("SET_LOCAL_3_POP", offset);
    case OP_SET_LOCAL_4_POP:    return simple("SET_LOCAL_4_POP", offset);
    case OP_SET_LOCAL_5_POP:    return simple("SET_LOCAL_5_POP", offset);
    case OP_SET_LOCAL_6_POP:    return simple("SET_LOCAL_6_POP", offset);
    case OP_SET_LOCAL_7_POP:    return simple("SET_LOCAL_7_POP", offset);

        // Increment
    case OP_INC_LOCAL_POP:      return byte("INC_LOCAL_POP", chunk, offset);
    case OP_INC_LOCAL:          return byte("INC_LOCAL", chunk, offset);
    case OP_INCREMENT:          return simple("INCREMENT", offset);
    case OP_DECREMENT:          return simple("DECREMENT", offset);

        // Globals
    case OP_GET_GLOBAL:         return byte("GET_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL_LONG:    return shortOp("GET_GLOBAL_LONG", chunk, offset);
    case OP_DEFINE_GLOBAL:      return byte("DEF_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL_LONG: return shortOp("DEF_GLOBAL_LONG", chunk, offset);
    case OP_SET_GLOBAL:         return byte("SET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL_LONG:    return shortOp("SET_GLOBAL_LONG", chunk, offset);

        // Upvalues
    case OP_GET_UPVALUE:            return byte("GET_UPVALUE", chunk, offset);
    case OP_GET_UPVALUE_OPEN:       return byte("GET_UPVALUE_OPEN", chunk, offset);
    case OP_GET_UPVALUE_CLOSED:     return byte("GET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_IMMUTABLE:  return byte("GET_UPVALUE_IMM", chunk, offset);
    case OP_SET_UPVALUE:            return byte("SET_UPVALUE", chunk, offset);
    case OP_SET_UPVALUE_OPEN:       return byte("SET_UPVALUE_OPEN", chunk, offset);
    case OP_SET_UPVALUE_CLOSED:     return byte("SET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_0:          return simple("GET_UPVALUE_0", offset);
    case OP_GET_UPVALUE_OPEN_0:     return simple("GET_UPVALUE_OPEN_0", offset);
    case OP_GET_UPVALUE_CLOSED_0:   return simple("GET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_IMMUTABLE_0:return simple("GET_UPVALUE_IMM_0", offset);
    case OP_SET_UPVALUE_0:          return simple("SET_UPVALUE_0", offset);
    case OP_SET_UPVALUE_OPEN_0:     return simple("SET_UPVALUE_OPEN_0", offset);
    case OP_SET_UPVALUE_CLOSED_0:   return simple("SET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_1:          return simple("GET_UPVALUE_1", offset);
    case OP_GET_UPVALUE_OPEN_1:     return simple("GET_UPVALUE_OPEN_1", offset);
    case OP_GET_UPVALUE_CLOSED_1:   return simple("GET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_IMMUTABLE_1:return simple("GET_UPVALUE_IMM_1", offset);
    case OP_SET_UPVALUE_1:          return simple("SET_UPVALUE_1", offset);
    case OP_SET_UPVALUE_OPEN_1:     return simple("SET_UPVALUE_OPEN_1", offset);
    case OP_SET_UPVALUE_CLOSED_1:   return simple("SET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_2:          return simple("GET_UPVALUE_2", offset);
    case OP_GET_UPVALUE_OPEN_2:     return simple("GET_UPVALUE_OPEN_2", offset);
    case OP_GET_UPVALUE_CLOSED_2:   return simple("GET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_IMMUTABLE_2:return simple("GET_UPVALUE_IMM_2", offset);
    case OP_SET_UPVALUE_2:          return simple("SET_UPVALUE_2", offset);
    case OP_SET_UPVALUE_OPEN_2:     return simple("SET_UPVALUE_OPEN_2", offset);
    case OP_SET_UPVALUE_CLOSED_2:   return simple("SET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_3:          return simple("GET_UPVALUE_3", offset);
    case OP_GET_UPVALUE_OPEN_3:     return simple("GET_UPVALUE_OPEN_3", offset);
    case OP_GET_UPVALUE_CLOSED_3:   return simple("GET_UPVALUE_CLOSED_3", offset);
    case OP_GET_UPVALUE_IMMUTABLE_3:return simple("GET_UPVALUE_IMM_3", offset);
    case OP_SET_UPVALUE_3:          return simple("SET_UPVALUE_3", offset);
    case OP_SET_UPVALUE_OPEN_3:     return simple("SET_UPVALUE_OPEN_3", offset);
    case OP_SET_UPVALUE_CLOSED_3:   return simple("SET_UPVALUE_CLOSED_3", offset);

        // Fields
    case OP_FIELD:              return constant("FIELD", chunk, offset);
    case OP_GET_FIELD_THIS:     return byte("GET_FIELD_THIS", chunk, offset);
    case OP_SET_FIELD_THIS:     return byte("SET_FIELD_THIS", chunk, offset);

        // Property IC
    case OP_GET_PROPERTY_IC:    return propertyIC("GET_PROP_IC", chunk, offset);
    case OP_SET_PROPERTY_IC:    return propertyIC("SET_PROP_IC", chunk, offset);

        // Super
    case OP_GET_SUPER:          return constant("GET_SUPER", chunk, offset);
    case OP_GET_SUPER_LONG:     return constantLong("GET_SUPER_LONG", chunk, offset);

        // Arithmetic
    case OP_EQUAL:              return simple("EQUAL", offset);
    case OP_GREATER:            return simple("GREATER", offset);
    case OP_LESS:               return simple("LESS", offset);
    case OP_ADD:                return simple("ADD", offset);
    case OP_SUBTRACT:           return simple("SUB", offset);
    case OP_MULTIPLY:           return simple("MUL", offset);
    case OP_DIVIDE:             return simple("DIV", offset);
    case OP_MODULO:             return simple("MOD", offset);
    case OP_NOT:                return simple("NOT", offset);
    case OP_NEGATE:             return simple("NEG", offset);
        // Jumps
    case OP_JUMP:               return jump("JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:      return jump("JUMP_IF_FALSE", 1, chunk, offset);
    case OP_POP_JUMP_IF_FALSE:  return jump("POP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_JUMP_IF_TRUE:       return jump("JUMP_IF_TRUE", 1, chunk, offset);
    case OP_POP_JUMP_IF_TRUE:   return jump("POP_JUMP_IF_TRUE", 1, chunk, offset);
    case OP_JUMP_IF_NOT_EQUAL:  return jump("JUMP_IF_NEQ", 1, chunk, offset);
    case OP_JUMP_IF_EQUAL:      return jump("JUMP_IF_EQ", 1, chunk, offset);
    case OP_JUMP_IF_NOT_GREATER:return jump("JUMP_IF_NGT", 1, chunk, offset);
    case OP_JUMP_IF_NOT_LESS:   return jump("JUMP_IF_NLT", 1, chunk, offset);
    case OP_LOOP:               return jump("LOOP", -1, chunk, offset);

        // Calls
    case OP_CALL_0:             return simple("CALL_0", offset);
    case OP_CALL_1:             return simple("CALL_1", offset);
    case OP_CALL_2:             return simple("CALL_2", offset);
    case OP_CALL_3:             return simple("CALL_3", offset);
    case OP_CALL_4:             return simple("CALL_4", offset);
    case OP_CALL_5:             return simple("CALL_5", offset);
    case OP_CALL_6:             return simple("CALL_6", offset);
    case OP_CALL_7:             return simple("CALL_7", offset);
    case OP_CALL_8:             return simple("CALL_8", offset);
    case OP_CALL:               return byte("CALL", chunk, offset);

        // Tail Calls
    case OP_TAIL_CALL_0:        return simple("TAIL_CALL_0", offset);
    case OP_TAIL_CALL_1:        return simple("TAIL_CALL_1", offset);
    case OP_TAIL_CALL_2:        return simple("TAIL_CALL_2", offset);
    case OP_TAIL_CALL_3:        return simple("TAIL_CALL_3", offset);
    case OP_TAIL_CALL_4:        return simple("TAIL_CALL_4", offset);
    case OP_TAIL_CALL_5:        return simple("TAIL_CALL_5", offset);
    case OP_TAIL_CALL_6:        return simple("TAIL_CALL_6", offset);
    case OP_TAIL_CALL_7:        return simple("TAIL_CALL_7", offset);
    case OP_TAIL_CALL_8:        return simple("TAIL_CALL_8", offset);
    case OP_TAIL_CALL:          return byte("TAIL_CALL", chunk, offset);

        // Invoke (indexed)
    case OP_INVOKE_0:           return invokeIndexed("INVOKE_0", 0, chunk, offset);
    case OP_INVOKE_1:           return invokeIndexed("INVOKE_1", 1, chunk, offset);
    case OP_INVOKE_2:           return invokeIndexed("INVOKE_2", 2, chunk, offset);
    case OP_INVOKE_3:           return invokeIndexed("INVOKE_3", 3, chunk, offset);
    case OP_INVOKE_4:           return invokeIndexed("INVOKE_4", 4, chunk, offset);
    case OP_INVOKE_5:           return invokeIndexed("INVOKE_5", 5, chunk, offset);
    case OP_INVOKE_6:           return invokeIndexed("INVOKE_6", 6, chunk, offset);
    case OP_INVOKE_7:           return invokeIndexed("INVOKE_7", 7, chunk, offset);
    case OP_INVOKE_8:           return invokeIndexed("INVOKE_8", 8, chunk, offset);
    case OP_INVOKE:             return invoke("INVOKE", chunk, offset);
    case OP_INVOKE_LONG:        return invokeLong("INVOKE_LONG", chunk, offset);

        // Invoke IC
    case OP_INVOKE_IC:          return invokeIC("INVOKE_IC", chunk, offset);

        // Tail Invoke (indexed)
    case OP_TAIL_INVOKE_0:      return invokeIndexed("TAIL_INVOKE_0", 0, chunk, offset);
    case OP_TAIL_INVOKE_1:      return invokeIndexed("TAIL_INVOKE_1", 1, chunk, offset);
    case OP_TAIL_INVOKE_2:      return invokeIndexed("TAIL_INVOKE_2", 2, chunk, offset);
    case OP_TAIL_INVOKE_3:      return invokeIndexed("TAIL_INVOKE_3", 3, chunk, offset);
    case OP_TAIL_INVOKE_4:      return invokeIndexed("TAIL_INVOKE_4", 4, chunk, offset);
    case OP_TAIL_INVOKE_5:      return invokeIndexed("TAIL_INVOKE_5", 5, chunk, offset);
    case OP_TAIL_INVOKE_6:      return invokeIndexed("TAIL_INVOKE_6", 6, chunk, offset);
    case OP_TAIL_INVOKE_7:      return invokeIndexed("TAIL_INVOKE_7", 7, chunk, offset);
    case OP_TAIL_INVOKE_8:      return invokeIndexed("TAIL_INVOKE_8", 8, chunk, offset);
    case OP_TAIL_INVOKE:        return invoke("TAIL_INVOKE", chunk, offset);
    case OP_TAIL_INVOKE_LONG:   return invokeLong("TAIL_INVOKE_LONG", chunk, offset);

        // Tail Invoke IC
    case OP_TAIL_INVOKE_IC:     return invokeIC("TAIL_INVOKE_IC", chunk, offset);

        // Super Invoke (indexed)
    case OP_SUPER_INVOKE_0:     return invokeIndexed("SUPER_INVOKE_0", 0, chunk, offset);
    case OP_SUPER_INVOKE_1:     return invokeIndexed("SUPER_INVOKE_1", 1, chunk, offset);
    case OP_SUPER_INVOKE_2:     return invokeIndexed("SUPER_INVOKE_2", 2, chunk, offset);
    case OP_SUPER_INVOKE_3:     return invokeIndexed("SUPER_INVOKE_3", 3, chunk, offset);
    case OP_SUPER_INVOKE_4:     return invokeIndexed("SUPER_INVOKE_4", 4, chunk, offset);
    case OP_SUPER_INVOKE_5:     return invokeIndexed("SUPER_INVOKE_5", 5, chunk, offset);
    case OP_SUPER_INVOKE_6:     return invokeIndexed("SUPER_INVOKE_6", 6, chunk, offset);
    case OP_SUPER_INVOKE_7:     return invokeIndexed("SUPER_INVOKE_7", 7, chunk, offset);
    case OP_SUPER_INVOKE_8:     return invokeIndexed("SUPER_INVOKE_8", 8, chunk, offset);
    case OP_SUPER_INVOKE:       return invoke("SUPER_INVOKE", chunk, offset);
    case OP_SUPER_INVOKE_LONG:  return invokeLong("SUPER_INVOKE_LONG", chunk, offset);

        // Tail Super Invoke (indexed)
    case OP_TAIL_SUPER_INVOKE_0:return invokeIndexed("TAIL_SUPER_INVOKE_0", 0, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_1:return invokeIndexed("TAIL_SUPER_INVOKE_1", 1, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_2:return invokeIndexed("TAIL_SUPER_INVOKE_2", 2, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_3:return invokeIndexed("TAIL_SUPER_INVOKE_3", 3, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_4:return invokeIndexed("TAIL_SUPER_INVOKE_4", 4, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_5:return invokeIndexed("TAIL_SUPER_INVOKE_5", 5, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_6:return invokeIndexed("TAIL_SUPER_INVOKE_6", 6, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_7:return invokeIndexed("TAIL_SUPER_INVOKE_7", 7, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_8:return invokeIndexed("TAIL_SUPER_INVOKE_8", 8, chunk, offset);
    case OP_TAIL_SUPER_INVOKE:  return invoke("TAIL_SUPER_INVOKE", chunk, offset);
    case OP_TAIL_SUPER_INVOKE_LONG: return invokeLong("TAIL_SUPER_INVOKE_LONG", chunk, offset);

        // Closures
    case OP_CLOSURE:            return closure("CLOSURE", chunk, offset, false);
    case OP_CLOSURE_LONG:       return closure("CLOSURE_LONG", chunk, offset, true);
    case OP_CLOSE_UPVALUE:      return simple("CLOSE_UPVALUE", offset);

        // Return
    case OP_RETURN:             return simple("RETURN", offset);

        // Classes
    case OP_CLASS:              return constant("CLASS", chunk, offset);
    case OP_CLASS_LONG:         return constantLong("CLASS_LONG", chunk, offset);
    case OP_INHERIT:            return simple("INHERIT", offset);
    case OP_METHOD:             return method("METHOD", chunk, offset);
    case OP_METHOD_LONG:        return methodLong("METHOD_LONG", chunk, offset);

        // Collections
    case OP_BUILD_LIST:         return byte("BUILD_LIST", chunk, offset);
    case OP_BUILD_TABLE:        return byte("BUILD_TABLE", chunk, offset);
    case OP_INDEX_GET:          return simple("INDEX_GET", offset);
    case OP_INDEX_SET:          return simple("INDEX_SET", offset);

        // Import
    case OP_IMPORT:             return constant("IMPORT", chunk, offset);
    case OP_IMPORT_LONG:        return constantLong("IMPORT_LONG", chunk, offset);

    default:
        fprintf(stderr, "UNKNOWN_OP %d\n", op);
        return offset + 1;
    }
}