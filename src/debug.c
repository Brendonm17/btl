#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "debug.h"
#include "object.h"
#include "value.h"

// Buffer size for formatting debug output strings
#define DEBUG_BUFFER_SIZE 1024

// Prints formatted debug output using the runtime's configured print function.
// Falls back to stderr if no print function is configured.
static void btl_debug_print(BTLRuntime* runtime, const char* format, ...) {
    char buffer[DEBUG_BUFFER_SIZE];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, DEBUG_BUFFER_SIZE, format, args);
    va_end(args);

    if (runtime) {
        BtlIOHandles* io = &runtime->config.platform.io;
        io->print(buffer, io->user_data);
    } else {
        fprintf(stderr, "%s", buffer);
    }
}

// Prints a BTL value in a human-readable debug format.
// Handles all value types including objects, functions, classes, etc.
static void btl_print_value_debug(BTLRuntime* runtime, BtlValue value) {
    if (IS_BOOL(value)) {
        btl_debug_print(runtime, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        btl_debug_print(runtime, "null");
    } else if (IS_NUMBER(value)) {
        btl_debug_print(runtime, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case BTL_OBJ_STRING:
            btl_debug_print(runtime, "'%s'", AS_CSTRING(value));
            break;
        case BTL_OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            btl_debug_print(runtime, "<fn %s>", fn->name ? fn->name->chars : "script");
            break;
        }
        case BTL_OBJ_CLASS:
            btl_debug_print(runtime, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case BTL_OBJ_INSTANCE:
            btl_debug_print(runtime, "<%s>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case BTL_OBJ_BOUND_METHOD:
            btl_debug_print(runtime, "<bound %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case BTL_OBJ_CLOSURE: {
            ObjClosure* cl = AS_CLOSURE(value);
            btl_debug_print(runtime, "<fn %s>", cl->function->name ? cl->function->name->chars : "script");
            break;
        }
        case BTL_OBJ_NATIVE:
            btl_debug_print(runtime, "<native>");
            break;
        case BTL_OBJ_LIST:
            btl_debug_print(runtime, "<list[%d]>", AS_LIST(value)->items.count);
            break;
        case BTL_OBJ_TABLE:
            btl_debug_print(runtime, "<table>");
            break;
        case BTL_OBJ_MODULE:
            btl_debug_print(runtime, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case BTL_OBJ_UPVALUE:
            btl_debug_print(runtime, "<upvalue>");
            break;
        case BTL_OBJ_FUTURE: {
            ObjFuture* future = AS_FUTURE(value);
            BtlFutureState state = btl_future_get_state(future);
            switch (state) {
            case BTL_FUTURE_PENDING: btl_debug_print(runtime, "<future:pending>"); break;
            case BTL_FUTURE_READY:   btl_debug_print(runtime, "<future:ready>"); break;
            case BTL_FUTURE_ERROR:   btl_debug_print(runtime, "<future:error>"); break;
            }
            break;
        }
        case BTL_OBJ_ACTOR: {
            ObjActor* actor = AS_ACTOR(value);
            if (actor->alive) {
                btl_debug_print(runtime, "<actor:%s>", actor->klass->name->chars);
            } else {
                btl_debug_print(runtime, "<actor:dead>");
            }
            break;
        }
        default:
            btl_debug_print(runtime, "<obj>");
            break;
        }
    }
}

// Disassembles an entire chunk of bytecode.
void btl_disassemble_chunk(BTLRuntime* runtime, BtlChunk* chunk, const char* name) {
    btl_debug_print(runtime, "== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = btl_disassemble_instruction(runtime, chunk, offset);
    }
}

// ============================================================================
// Instruction Printers
// ============================================================================

// Prints a simple instruction with no operands.
static int btl_simple(BTLRuntime* runtime, const char* name, int offset) {
    btl_debug_print(runtime, "%s\n", name);
    return offset + 1;
}

// Prints an instruction with a single byte operand.
static int btl_byte(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    btl_debug_print(runtime, "%-24s %d\n", name, slot);
    return offset + 2;
}

// Prints an instruction with a 16-bit (short) operand.
static int btl_short_op(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint16_t slot = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    btl_debug_print(runtime, "%-24s %d\n", name, slot);
    return offset + 3;
}

// Prints a jump instruction with its target address.
// sign: 1 for forward jumps, -1 for backward jumps (loops)
static int btl_jump(BTLRuntime* runtime, const char* name, int sign, BtlChunk* chunk, int offset) {
    uint16_t jmp = (uint16_t) (chunk->code[offset + 1] << 8 | chunk->code[offset + 2]);
    btl_debug_print(runtime, "%-24s %d -> %d\n", name, offset, offset + 3 + sign * jmp);
    return offset + 3;
}

// Prints a constant instruction with its index and value.
static int btl_constant(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t idx = chunk->code[offset + 1];
    btl_debug_print(runtime, "%-24s [%d] ", name, idx);
    btl_print_value_debug(runtime, chunk->constants.values[idx]);
    btl_debug_print(runtime, "\n");
    return offset + 2;
}

// Prints a long constant instruction with a 16-bit index.
static int btl_constant_long(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint16_t idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    btl_debug_print(runtime, "%-24s [%d] ", name, idx);
    btl_print_value_debug(runtime, chunk->constants.values[idx]);
    btl_debug_print(runtime, "\n");
    return offset + 3;
}

// Prints an indexed invoke instruction (OP_INVOKE_0 through OP_INVOKE_8).
// Format: [methodIndex]
static int btl_invoke_indexed(BTLRuntime* runtime, const char* name, int argCount, BtlChunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    btl_debug_print(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 2;
}

// Prints a general invoke instruction.
// Format: [methodIndex] [argCount]
static int btl_invoke(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    btl_debug_print(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 3;
}

// Prints a long invoke instruction with 16-bit method index.
// Format: [methodIndex:16] [argCount]
static int btl_invoke_long(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t argCount = chunk->code[offset + 3];
    btl_debug_print(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 4;
}

// Prints an invoke instruction with inline cache.
// Format: [nameIdx] [argCount] [icSlot]
static int btl_invoke_ic(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    uint8_t icSlot = chunk->code[offset + 3];
    btl_debug_print(runtime, "%-24s '%s' args=%d ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, argCount, icSlot);
    return offset + 4;
}

// Prints a property access instruction with inline cache.
// Format: [nameIdx] [icSlot]
static int btl_property_ic(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t icSlot = chunk->code[offset + 2];
    btl_debug_print(runtime, "%-24s '%s' ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, icSlot);
    return offset + 3;
}

// Prints a method definition instruction.
// Format: [methodIndex] [arity]
static int btl_method(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t arity = chunk->code[offset + 2];
    btl_debug_print(runtime, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 3;
}

// Prints a long method definition instruction with 16-bit index.
// Format: [methodIndex:16] [arity]
static int btl_method_long(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t arity = chunk->code[offset + 3];
    btl_debug_print(runtime, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 4;
}

// Prints a closure instruction with its upvalue information.
// isLong: true for OP_CLOSURE_LONG (16-bit index), false for OP_CLOSURE (8-bit index)
static int btl_closure(BTLRuntime* runtime, const char* name, BtlChunk* chunk, int offset, bool isLong) {
    int idx;
    int base;
    if (isLong) {
        idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
        base = offset + 3;
    } else {
        idx = chunk->code[offset + 1];
        base = offset + 2;
    }

    btl_debug_print(runtime, "%-24s [%d] ", name, idx);
    btl_print_value_debug(runtime, chunk->constants.values[idx]);
    btl_debug_print(runtime, "\n");

    ObjFunction* fn = AS_FUNCTION(chunk->constants.values[idx]);
    for (int i = 0; i < fn->upvalueCount; i++) {
        uint8_t isLocal = chunk->code[base++];
        uint8_t index = chunk->code[base++];
        uint8_t isMut = chunk->code[base++];
        btl_debug_print(runtime, "     |                          %s[%d] %s\n",
            isLocal ? "local" : "upval", index, isMut ? "mut" : "imm");
    }
    return base;
}

// Disassembles a single instruction at the given offset.
int btl_disassemble_instruction(BTLRuntime* runtime, BtlChunk* chunk, int offset) {
    // Print instruction offset
    btl_debug_print(runtime, "%04d ", offset);

    // Print line number or continuation marker
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        btl_debug_print(runtime, "   | ");
    } else {
        btl_debug_print(runtime, "%4d ", chunk->lines[offset]);
    }

    uint8_t op = chunk->code[offset];

    switch (op) {
        // Constants
    case BTL_OP_CONSTANT:           return btl_constant(runtime, "CONSTANT", chunk, offset);
    case BTL_OP_CONSTANT_LONG:      return btl_constant_long(runtime, "CONSTANT_LONG", chunk, offset);
    case BTL_OP_NULL:               return btl_simple(runtime, "NULL", offset);
    case BTL_OP_TRUE:               return btl_simple(runtime, "TRUE", offset);
    case BTL_OP_FALSE:              return btl_simple(runtime, "FALSE", offset);
    case BTL_OP_0:                  return btl_simple(runtime, "PUSH_0", offset);
    case BTL_OP_1:                  return btl_simple(runtime, "PUSH_1", offset);
    case BTL_OP_2:                  return btl_simple(runtime, "PUSH_2", offset);
    case BTL_OP_INT_0:              return btl_simple(runtime, "PUSH_INT_0", offset);
    case BTL_OP_INT_1:              return btl_simple(runtime, "PUSH_INT_1", offset);
    case BTL_OP_INT_2:              return btl_simple(runtime, "PUSH_INT_2", offset);

        // Stack
    case BTL_OP_POP:                return btl_simple(runtime, "POP", offset);
    case BTL_OP_POP_N:              return btl_byte(runtime, "POP_N", chunk, offset);
    case BTL_OP_DUP:                return btl_simple(runtime, "DUP", offset);
    case BTL_OP_SWAP:               return btl_simple(runtime, "SWAP", offset);

        // Locals
    case BTL_OP_GET_LOCAL:          return btl_byte(runtime, "GET_LOCAL", chunk, offset);
    case BTL_OP_GET_LOCAL_0:        return btl_simple(runtime, "GET_LOCAL_0", offset);
    case BTL_OP_GET_LOCAL_1:        return btl_simple(runtime, "GET_LOCAL_1", offset);
    case BTL_OP_GET_LOCAL_2:        return btl_simple(runtime, "GET_LOCAL_2", offset);
    case BTL_OP_GET_LOCAL_3:        return btl_simple(runtime, "GET_LOCAL_3", offset);
    case BTL_OP_GET_LOCAL_4:        return btl_simple(runtime, "GET_LOCAL_4", offset);
    case BTL_OP_GET_LOCAL_5:        return btl_simple(runtime, "GET_LOCAL_5", offset);
    case BTL_OP_GET_LOCAL_6:        return btl_simple(runtime, "GET_LOCAL_6", offset);
    case BTL_OP_GET_LOCAL_7:        return btl_simple(runtime, "GET_LOCAL_7", offset);
    case BTL_OP_SET_LOCAL:          return btl_byte(runtime, "SET_LOCAL", chunk, offset);
    case BTL_OP_SET_LOCAL_0:        return btl_simple(runtime, "SET_LOCAL_0", offset);
    case BTL_OP_SET_LOCAL_1:        return btl_simple(runtime, "SET_LOCAL_1", offset);
    case BTL_OP_SET_LOCAL_2:        return btl_simple(runtime, "SET_LOCAL_2", offset);
    case BTL_OP_SET_LOCAL_3:        return btl_simple(runtime, "SET_LOCAL_3", offset);
    case BTL_OP_SET_LOCAL_4:        return btl_simple(runtime, "SET_LOCAL_4", offset);
    case BTL_OP_SET_LOCAL_5:        return btl_simple(runtime, "SET_LOCAL_5", offset);
    case BTL_OP_SET_LOCAL_6:        return btl_simple(runtime, "SET_LOCAL_6", offset);
    case BTL_OP_SET_LOCAL_7:        return btl_simple(runtime, "SET_LOCAL_7", offset);
    case BTL_OP_SET_LOCAL_0_POP:    return btl_simple(runtime, "SET_LOCAL_0_POP", offset);
    case BTL_OP_SET_LOCAL_1_POP:    return btl_simple(runtime, "SET_LOCAL_1_POP", offset);
    case BTL_OP_SET_LOCAL_2_POP:    return btl_simple(runtime, "SET_LOCAL_2_POP", offset);
    case BTL_OP_SET_LOCAL_3_POP:    return btl_simple(runtime, "SET_LOCAL_3_POP", offset);
    case BTL_OP_SET_LOCAL_4_POP:    return btl_simple(runtime, "SET_LOCAL_4_POP", offset);
    case BTL_OP_SET_LOCAL_5_POP:    return btl_simple(runtime, "SET_LOCAL_5_POP", offset);
    case BTL_OP_SET_LOCAL_6_POP:    return btl_simple(runtime, "SET_LOCAL_6_POP", offset);
    case BTL_OP_SET_LOCAL_7_POP:    return btl_simple(runtime, "SET_LOCAL_7_POP", offset);

        // Increment
    case BTL_OP_INC_LOCAL_POP:      return btl_byte(runtime, "INC_LOCAL_POP", chunk, offset);
    case BTL_OP_INC_LOCAL:          return btl_byte(runtime, "INC_LOCAL", chunk, offset);
    case BTL_OP_INCREMENT:          return btl_simple(runtime, "INCREMENT", offset);
    case BTL_OP_DECREMENT:          return btl_simple(runtime, "DECREMENT", offset);

        // Globals
    case BTL_OP_GET_GLOBAL:         return btl_byte(runtime, "GET_GLOBAL", chunk, offset);
    case BTL_OP_GET_GLOBAL_LONG:    return btl_short_op(runtime, "GET_GLOBAL_LONG", chunk, offset);
    case BTL_OP_DEFINE_GLOBAL:      return btl_byte(runtime, "DEF_GLOBAL", chunk, offset);
    case BTL_OP_DEFINE_GLOBAL_LONG: return btl_short_op(runtime, "DEF_GLOBAL_LONG", chunk, offset);
    case BTL_OP_SET_GLOBAL:         return btl_byte(runtime, "SET_GLOBAL", chunk, offset);
    case BTL_OP_SET_GLOBAL_LONG:    return btl_short_op(runtime, "SET_GLOBAL_LONG", chunk, offset);

        // Upvalues
    case BTL_OP_GET_UPVALUE:            return btl_byte(runtime, "GET_UPVALUE", chunk, offset);
    case BTL_OP_GET_UPVALUE_OPEN:       return btl_byte(runtime, "GET_UPVALUE_OPEN", chunk, offset);
    case BTL_OP_GET_UPVALUE_CLOSED:     return btl_byte(runtime, "GET_UPVALUE_CLOSED", chunk, offset);
    case BTL_OP_GET_UPVALUE_IMMUTABLE:  return btl_byte(runtime, "GET_UPVALUE_IMM", chunk, offset);
    case BTL_OP_SET_UPVALUE:            return btl_byte(runtime, "SET_UPVALUE", chunk, offset);
    case BTL_OP_SET_UPVALUE_OPEN:       return btl_byte(runtime, "SET_UPVALUE_OPEN", chunk, offset);
    case BTL_OP_SET_UPVALUE_CLOSED:     return btl_byte(runtime, "SET_UPVALUE_CLOSED", chunk, offset);
    case BTL_OP_GET_UPVALUE_0:          return btl_simple(runtime, "GET_UPVALUE_0", offset);
    case BTL_OP_GET_UPVALUE_OPEN_0:     return btl_simple(runtime, "GET_UPVALUE_OPEN_0", offset);
    case BTL_OP_GET_UPVALUE_CLOSED_0:   return btl_simple(runtime, "GET_UPVALUE_CLOSED_0", offset);
    case BTL_OP_GET_UPVALUE_IMMUTABLE_0:return btl_simple(runtime, "GET_UPVALUE_IMM_0", offset);
    case BTL_OP_SET_UPVALUE_0:          return btl_simple(runtime, "SET_UPVALUE_0", offset);
    case BTL_OP_SET_UPVALUE_OPEN_0:     return btl_simple(runtime, "SET_UPVALUE_OPEN_0", offset);
    case BTL_OP_SET_UPVALUE_CLOSED_0:   return btl_simple(runtime, "SET_UPVALUE_CLOSED_0", offset);
    case BTL_OP_GET_UPVALUE_1:          return btl_simple(runtime, "GET_UPVALUE_1", offset);
    case BTL_OP_GET_UPVALUE_OPEN_1:     return btl_simple(runtime, "GET_UPVALUE_OPEN_1", offset);
    case BTL_OP_GET_UPVALUE_CLOSED_1:   return btl_simple(runtime, "GET_UPVALUE_CLOSED_1", offset);
    case BTL_OP_GET_UPVALUE_IMMUTABLE_1:return btl_simple(runtime, "GET_UPVALUE_IMM_1", offset);
    case BTL_OP_SET_UPVALUE_1:          return btl_simple(runtime, "SET_UPVALUE_1", offset);
    case BTL_OP_SET_UPVALUE_OPEN_1:     return btl_simple(runtime, "SET_UPVALUE_OPEN_1", offset);
    case BTL_OP_SET_UPVALUE_CLOSED_1:   return btl_simple(runtime, "SET_UPVALUE_CLOSED_1", offset);
    case BTL_OP_GET_UPVALUE_2:          return btl_simple(runtime, "GET_UPVALUE_2", offset);
    case BTL_OP_GET_UPVALUE_OPEN_2:     return btl_simple(runtime, "GET_UPVALUE_OPEN_2", offset);
    case BTL_OP_GET_UPVALUE_CLOSED_2:   return btl_simple(runtime, "GET_UPVALUE_CLOSED_2", offset);
    case BTL_OP_GET_UPVALUE_IMMUTABLE_2:return btl_simple(runtime, "GET_UPVALUE_IMM_2", offset);
    case BTL_OP_SET_UPVALUE_2:          return btl_simple(runtime, "SET_UPVALUE_2", offset);
    case BTL_OP_SET_UPVALUE_OPEN_2:     return btl_simple(runtime, "SET_UPVALUE_OPEN_2", offset);
    case BTL_OP_SET_UPVALUE_CLOSED_2:   return btl_simple(runtime, "SET_UPVALUE_CLOSED_2", offset);
    case BTL_OP_GET_UPVALUE_3:          return btl_simple(runtime, "GET_UPVALUE_3", offset);
    case BTL_OP_GET_UPVALUE_OPEN_3:     return btl_simple(runtime, "GET_UPVALUE_OPEN_3", offset);
    case BTL_OP_GET_UPVALUE_CLOSED_3:   return btl_simple(runtime, "GET_UPVALUE_CLOSED_3", offset);
    case BTL_OP_GET_UPVALUE_IMMUTABLE_3:return btl_simple(runtime, "GET_UPVALUE_IMM_3", offset);
    case BTL_OP_SET_UPVALUE_3:          return btl_simple(runtime, "SET_UPVALUE_3", offset);
    case BTL_OP_SET_UPVALUE_OPEN_3:     return btl_simple(runtime, "SET_UPVALUE_OPEN_3", offset);
    case BTL_OP_SET_UPVALUE_CLOSED_3:   return btl_simple(runtime, "SET_UPVALUE_CLOSED_3", offset);

        // Fields
    case BTL_OP_FIELD:              return btl_constant(runtime, "FIELD", chunk, offset);
    case BTL_OP_GET_FIELD_THIS:     return btl_byte(runtime, "GET_FIELD_THIS", chunk, offset);
    case BTL_OP_SET_FIELD_THIS:     return btl_byte(runtime, "SET_FIELD_THIS", chunk, offset);

        // Property IC
    case BTL_OP_GET_PROPERTY_IC:    return btl_property_ic(runtime, "GET_PROP_IC", chunk, offset);
    case BTL_OP_SET_PROPERTY_IC:    return btl_property_ic(runtime, "SET_PROP_IC", chunk, offset);

        // Super
    case BTL_OP_GET_SUPER:          return btl_constant(runtime, "GET_SUPER", chunk, offset);
    case BTL_OP_GET_SUPER_LONG:     return btl_constant_long(runtime, "GET_SUPER_LONG", chunk, offset);

        // Arithmetic
    case BTL_OP_EQUAL:              return btl_simple(runtime, "EQUAL", offset);
    case BTL_OP_GREATER:            return btl_simple(runtime, "GREATER", offset);
    case BTL_OP_LESS:               return btl_simple(runtime, "LESS", offset);
    case BTL_OP_ADD:                return btl_simple(runtime, "ADD", offset);
    case BTL_OP_SUBTRACT:           return btl_simple(runtime, "SUB", offset);
    case BTL_OP_MULTIPLY:           return btl_simple(runtime, "MUL", offset);
    case BTL_OP_DIVIDE:             return btl_simple(runtime, "DIV", offset);
    case BTL_OP_MODULO:             return btl_simple(runtime, "MOD", offset);
    case BTL_OP_NOT:                return btl_simple(runtime, "NOT", offset);
    case BTL_OP_NEGATE:             return btl_simple(runtime, "NEG", offset);

        // Jumps
    case BTL_OP_JUMP:               return btl_jump(runtime, "JUMP", 1, chunk, offset);
    case BTL_OP_JUMP_IF_FALSE:      return btl_jump(runtime, "JUMP_IF_FALSE", 1, chunk, offset);
    case BTL_OP_POP_JUMP_IF_FALSE:  return btl_jump(runtime, "POP_JUMP_IF_FALSE", 1, chunk, offset);
    case BTL_OP_JUMP_IF_TRUE:       return btl_jump(runtime, "JUMP_IF_TRUE", 1, chunk, offset);
    case BTL_OP_POP_JUMP_IF_TRUE:   return btl_jump(runtime, "POP_JUMP_IF_TRUE", 1, chunk, offset);
    case BTL_OP_JUMP_IF_NOT_EQUAL:  return btl_jump(runtime, "JUMP_IF_NEQ", 1, chunk, offset);
    case BTL_OP_JUMP_IF_EQUAL:      return btl_jump(runtime, "JUMP_IF_EQ", 1, chunk, offset);
    case BTL_OP_JUMP_IF_NOT_GREATER:return btl_jump(runtime, "JUMP_IF_NGT", 1, chunk, offset);
    case BTL_OP_JUMP_IF_NOT_LESS:   return btl_jump(runtime, "JUMP_IF_NLT", 1, chunk, offset);
    case BTL_OP_LOOP:               return btl_jump(runtime, "LOOP", -1, chunk, offset);

        // Calls
    case BTL_OP_CALL_0:             return btl_simple(runtime, "CALL_0", offset);
    case BTL_OP_CALL_1:             return btl_simple(runtime, "CALL_1", offset);
    case BTL_OP_CALL_2:             return btl_simple(runtime, "CALL_2", offset);
    case BTL_OP_CALL_3:             return btl_simple(runtime, "CALL_3", offset);
    case BTL_OP_CALL_4:             return btl_simple(runtime, "CALL_4", offset);
    case BTL_OP_CALL_5:             return btl_simple(runtime, "CALL_5", offset);
    case BTL_OP_CALL_6:             return btl_simple(runtime, "CALL_6", offset);
    case BTL_OP_CALL_7:             return btl_simple(runtime, "CALL_7", offset);
    case BTL_OP_CALL_8:             return btl_simple(runtime, "CALL_8", offset);
    case BTL_OP_CALL:               return btl_byte(runtime, "CALL", chunk, offset);

        // Tail Calls
    case BTL_OP_TAIL_CALL_0:        return btl_simple(runtime, "TAIL_CALL_0", offset);
    case BTL_OP_TAIL_CALL_1:        return btl_simple(runtime, "TAIL_CALL_1", offset);
    case BTL_OP_TAIL_CALL_2:        return btl_simple(runtime, "TAIL_CALL_2", offset);
    case BTL_OP_TAIL_CALL_3:        return btl_simple(runtime, "TAIL_CALL_3", offset);
    case BTL_OP_TAIL_CALL_4:        return btl_simple(runtime, "TAIL_CALL_4", offset);
    case BTL_OP_TAIL_CALL_5:        return btl_simple(runtime, "TAIL_CALL_5", offset);
    case BTL_OP_TAIL_CALL_6:        return btl_simple(runtime, "TAIL_CALL_6", offset);
    case BTL_OP_TAIL_CALL_7:        return btl_simple(runtime, "TAIL_CALL_7", offset);
    case BTL_OP_TAIL_CALL_8:        return btl_simple(runtime, "TAIL_CALL_8", offset);
    case BTL_OP_TAIL_CALL:          return btl_byte(runtime, "TAIL_CALL", chunk, offset);

        // Invoke (indexed)
    case BTL_OP_INVOKE_0:           return btl_invoke_indexed(runtime, "INVOKE_0", 0, chunk, offset);
    case BTL_OP_INVOKE_1:           return btl_invoke_indexed(runtime, "INVOKE_1", 1, chunk, offset);
    case BTL_OP_INVOKE_2:           return btl_invoke_indexed(runtime, "INVOKE_2", 2, chunk, offset);
    case BTL_OP_INVOKE_3:           return btl_invoke_indexed(runtime, "INVOKE_3", 3, chunk, offset);
    case BTL_OP_INVOKE_4:           return btl_invoke_indexed(runtime, "INVOKE_4", 4, chunk, offset);
    case BTL_OP_INVOKE_5:           return btl_invoke_indexed(runtime, "INVOKE_5", 5, chunk, offset);
    case BTL_OP_INVOKE_6:           return btl_invoke_indexed(runtime, "INVOKE_6", 6, chunk, offset);
    case BTL_OP_INVOKE_7:           return btl_invoke_indexed(runtime, "INVOKE_7", 7, chunk, offset);
    case BTL_OP_INVOKE_8:           return btl_invoke_indexed(runtime, "INVOKE_8", 8, chunk, offset);
    case BTL_OP_INVOKE:             return btl_invoke(runtime, "INVOKE", chunk, offset);
    case BTL_OP_INVOKE_LONG:        return btl_invoke_long(runtime, "INVOKE_LONG", chunk, offset);

        // Invoke IC
    case BTL_OP_INVOKE_IC:          return btl_invoke_ic(runtime, "INVOKE_IC", chunk, offset);

        // Tail Invoke (indexed)
    case BTL_OP_TAIL_INVOKE_0:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_0", 0, chunk, offset);
    case BTL_OP_TAIL_INVOKE_1:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_1", 1, chunk, offset);
    case BTL_OP_TAIL_INVOKE_2:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_2", 2, chunk, offset);
    case BTL_OP_TAIL_INVOKE_3:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_3", 3, chunk, offset);
    case BTL_OP_TAIL_INVOKE_4:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_4", 4, chunk, offset);
    case BTL_OP_TAIL_INVOKE_5:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_5", 5, chunk, offset);
    case BTL_OP_TAIL_INVOKE_6:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_6", 6, chunk, offset);
    case BTL_OP_TAIL_INVOKE_7:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_7", 7, chunk, offset);
    case BTL_OP_TAIL_INVOKE_8:      return btl_invoke_indexed(runtime, "TAIL_INVOKE_8", 8, chunk, offset);
    case BTL_OP_TAIL_INVOKE:        return btl_invoke(runtime, "TAIL_INVOKE", chunk, offset);
    case BTL_OP_TAIL_INVOKE_LONG:   return btl_invoke_long(runtime, "TAIL_INVOKE_LONG", chunk, offset);

        // Tail Invoke IC
    case BTL_OP_TAIL_INVOKE_IC:     return btl_invoke_ic(runtime, "TAIL_INVOKE_IC", chunk, offset);

        // Super Invoke (indexed)
    case BTL_OP_SUPER_INVOKE_0:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_0", 0, chunk, offset);
    case BTL_OP_SUPER_INVOKE_1:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_1", 1, chunk, offset);
    case BTL_OP_SUPER_INVOKE_2:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_2", 2, chunk, offset);
    case BTL_OP_SUPER_INVOKE_3:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_3", 3, chunk, offset);
    case BTL_OP_SUPER_INVOKE_4:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_4", 4, chunk, offset);
    case BTL_OP_SUPER_INVOKE_5:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_5", 5, chunk, offset);
    case BTL_OP_SUPER_INVOKE_6:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_6", 6, chunk, offset);
    case BTL_OP_SUPER_INVOKE_7:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_7", 7, chunk, offset);
    case BTL_OP_SUPER_INVOKE_8:     return btl_invoke_indexed(runtime, "SUPER_INVOKE_8", 8, chunk, offset);
    case BTL_OP_SUPER_INVOKE:       return btl_invoke(runtime, "SUPER_INVOKE", chunk, offset);
    case BTL_OP_SUPER_INVOKE_LONG:  return btl_invoke_long(runtime, "SUPER_INVOKE_LONG", chunk, offset);

        // Tail Super Invoke (indexed)
    case BTL_OP_TAIL_SUPER_INVOKE_0:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_0", 0, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_1:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_1", 1, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_2:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_2", 2, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_3:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_3", 3, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_4:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_4", 4, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_5:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_5", 5, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_6:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_6", 6, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_7:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_7", 7, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_8:return btl_invoke_indexed(runtime, "TAIL_SUPER_INVOKE_8", 8, chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE:  return btl_invoke(runtime, "TAIL_SUPER_INVOKE", chunk, offset);
    case BTL_OP_TAIL_SUPER_INVOKE_LONG: return btl_invoke_long(runtime, "TAIL_SUPER_INVOKE_LONG", chunk, offset);

        // Closures
    case BTL_OP_CLOSURE:            return btl_closure(runtime, "CLOSURE", chunk, offset, false);
    case BTL_OP_CLOSURE_LONG:       return btl_closure(runtime, "CLOSURE_LONG", chunk, offset, true);
    case BTL_OP_CLOSE_UPVALUE:      return btl_simple(runtime, "CLOSE_UPVALUE", offset);

        // Return
    case BTL_OP_RETURN:             return btl_simple(runtime, "RETURN", offset);

        // Classes
    case BTL_OP_CLASS:              return btl_constant(runtime, "CLASS", chunk, offset);
    case BTL_OP_CLASS_LONG:         return btl_constant_long(runtime, "CLASS_LONG", chunk, offset);
    case BTL_OP_INHERIT:            return btl_simple(runtime, "INHERIT", offset);
    case BTL_OP_METHOD:             return btl_method(runtime, "METHOD", chunk, offset);
    case BTL_OP_METHOD_LONG:        return btl_method_long(runtime, "METHOD_LONG", chunk, offset);

        // Collections
    case BTL_OP_BUILD_LIST:         return btl_byte(runtime, "BUILD_LIST", chunk, offset);
    case BTL_OP_BUILD_TABLE:        return btl_byte(runtime, "BUILD_TABLE", chunk, offset);
    case BTL_OP_INDEX_GET:          return btl_simple(runtime, "INDEX_GET", offset);
    case BTL_OP_INDEX_SET:          return btl_simple(runtime, "INDEX_SET", offset);

        // Import
    case BTL_OP_IMPORT:             return btl_constant(runtime, "IMPORT", chunk, offset);
    case BTL_OP_IMPORT_LONG:        return btl_constant_long(runtime, "IMPORT_LONG", chunk, offset);

        // Actor operations
    case BTL_OP_DO_NEW:             return btl_byte(runtime, "OP_DO_NEW", chunk, offset);
    case BTL_OP_DO_INVOKE:          return btl_invoke(runtime, "OP_DO_INVOKE", chunk, offset);

        // Iterator operations
    case BTL_OP_ITER_INIT:          return btl_simple(runtime, "OP_ITER_INIT", offset);
    case BTL_OP_ITER_NEXT: {
        uint8_t slot = chunk->code[offset + 1];
        uint16_t jmp = (uint16_t) (chunk->code[offset + 2] << 8 | chunk->code[offset + 3]);
        btl_debug_print(runtime, "%-24s slot=%d -> %d\n", "OP_ITER_NEXT", slot, offset + 4 + (int)jmp);
        return offset + 4;
    }

    default:
        btl_debug_print(runtime, "UNKNOWN_OP %d\n", op);
        return offset + 1;
    }
}
