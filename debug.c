#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "debug.h"
#include "object.h"
#include "value.h"

// ============================================================================
// Debug Output Helpers
// ============================================================================

// Buffer for formatting debug output
#define DEBUG_BUFFER_SIZE 1024

static void debugPrint(BTLRuntime* runtime, const char* format, ...) {
    char buffer[DEBUG_BUFFER_SIZE];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, DEBUG_BUFFER_SIZE, format, args);
    va_end(args);

    if (runtime && runtime->config.print) {
        runtime->config.print(buffer, runtime->config.user_data);
    } else {
        fprintf(stderr, "%s", buffer);
    }
}

static void printValueDebug(BTLRuntime* runtime, Value value) {
    if (IS_BOOL(value)) {
        debugPrint(runtime, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NULL(value)) {
        debugPrint(runtime, "null");
    } else if (IS_NUMBER(value)) {
        debugPrint(runtime, "%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            debugPrint(runtime, "'%s'", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            debugPrint(runtime, "<fn %s>", fn->name ? fn->name->chars : "script");
            break;
        }
        case OBJ_CLASS:
            debugPrint(runtime, "<class %s>", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            debugPrint(runtime, "<%s>", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_BOUND_METHOD:
            debugPrint(runtime, "<bound %s>", AS_BOUND_METHOD(value)->method->function->name->chars);
            break;
        case OBJ_CLOSURE: {
            ObjClosure* cl = AS_CLOSURE(value);
            debugPrint(runtime, "<fn %s>", cl->function->name ? cl->function->name->chars : "script");
            break;
        }
        case OBJ_NATIVE:
            debugPrint(runtime, "<native>");
            break;
        case OBJ_LIST:
            debugPrint(runtime, "<list[%d]>", AS_LIST(value)->items.count);
            break;
        case OBJ_TABLE:
            debugPrint(runtime, "<table>");
            break;
        case OBJ_MODULE:
            debugPrint(runtime, "<module %s>", AS_MODULE(value)->name->chars);
            break;
        case OBJ_UPVALUE:
            debugPrint(runtime, "<upvalue>");
            break;
        case OBJ_FUTURE: {
            ObjFuture* future = AS_FUTURE(value);
            FutureState state = futureGetState(future);
            switch (state) {
            case FUTURE_PENDING: debugPrint(runtime, "<future:pending>"); break;
            case FUTURE_READY:   debugPrint(runtime, "<future:ready>"); break;
            case FUTURE_ERROR:   debugPrint(runtime, "<future:error>"); break;
            }
            break;
        }
        case OBJ_ACTOR: {
            ObjActor* actor = AS_ACTOR(value);
            if (actor->alive) {
                debugPrint(runtime, "<actor:%s>", actor->klass->name->chars);
            } else {
                debugPrint(runtime, "<actor:dead>");
            }
            break;
        }
        default:
            debugPrint(runtime, "<obj>");
            break;
        }
    }
}

void disassembleChunk(BTLRuntime* runtime, Chunk* chunk, const char* name) {
    debugPrint(runtime, "== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(runtime, chunk, offset);
    }
}

// ============================================================================
// Instruction Printers
// ============================================================================

static int simple(BTLRuntime* runtime, const char* name, int offset) {
    debugPrint(runtime, "%s\n", name);
    return offset + 1;
}

static int byte_(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    debugPrint(runtime, "%-24s %d\n", name, slot);
    return offset + 2;
}

static int shortOp(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint16_t slot = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    debugPrint(runtime, "%-24s %d\n", name, slot);
    return offset + 3;
}

static int jump(BTLRuntime* runtime, const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jmp = (uint16_t) (chunk->code[offset + 1] << 8 | chunk->code[offset + 2]);
    debugPrint(runtime, "%-24s %d -> %d\n", name, offset, offset + 3 + sign * jmp);
    return offset + 3;
}

static int constant(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t idx = chunk->code[offset + 1];
    debugPrint(runtime, "%-24s [%d] ", name, idx);
    printValueDebug(runtime, chunk->constants.values[idx]);
    debugPrint(runtime, "\n");
    return offset + 2;
}

static int constantLong(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint16_t idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    debugPrint(runtime, "%-24s [%d] ", name, idx);
    printValueDebug(runtime, chunk->constants.values[idx]);
    debugPrint(runtime, "\n");
    return offset + 3;
}

// OP_INVOKE_0 - OP_INVOKE_8: [methodIndex]
static int invokeIndexed(BTLRuntime* runtime, const char* name, int argCount, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    debugPrint(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 2;
}

// OP_INVOKE: [methodIndex] [argCount]
static int invoke(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    debugPrint(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 3;
}

// OP_INVOKE_LONG: [methodIndex:16] [argCount]
static int invokeLong(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t argCount = chunk->code[offset + 3];
    debugPrint(runtime, "%-24s idx=%d args=%d\n", name, methodIndex, argCount);
    return offset + 4;
}

// OP_INVOKE_IC: [nameIdx] [argCount] [icSlot]
static int invokeIC(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];
    uint8_t icSlot = chunk->code[offset + 3];
    debugPrint(runtime, "%-24s '%s' args=%d ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, argCount, icSlot);
    return offset + 4;
}

// OP_GET/SET_PROPERTY_IC: [nameIdx] [icSlot]
static int propertyIC(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t nameIdx = chunk->code[offset + 1];
    uint8_t icSlot = chunk->code[offset + 2];
    debugPrint(runtime, "%-24s '%s' ic=%d\n", name,
        AS_STRING(chunk->constants.values[nameIdx])->chars, icSlot);
    return offset + 3;
}

// OP_METHOD: [methodIndex] [arity]
static int method(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint8_t methodIndex = chunk->code[offset + 1];
    uint8_t arity = chunk->code[offset + 2];
    debugPrint(runtime, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 3;
}

// OP_METHOD_LONG: [methodIndex:16] [arity]
static int methodLong(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset) {
    uint16_t methodIndex = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
    uint8_t arity = chunk->code[offset + 3];
    debugPrint(runtime, "%-24s idx=%d arity=%d\n", name, methodIndex, arity);
    return offset + 4;
}

// OP_CLOSURE / OP_CLOSURE_LONG
static int closure(BTLRuntime* runtime, const char* name, Chunk* chunk, int offset, bool isLong) {
    int idx;
    int base;
    if (isLong) {
        idx = (uint16_t) (chunk->code[offset + 1] | (chunk->code[offset + 2] << 8));
        base = offset + 3;
    } else {
        idx = chunk->code[offset + 1];
        base = offset + 2;
    }

    debugPrint(runtime, "%-24s [%d] ", name, idx);
    printValueDebug(runtime, chunk->constants.values[idx]);
    debugPrint(runtime, "\n");

    ObjFunction* fn = AS_FUNCTION(chunk->constants.values[idx]);
    for (int i = 0; i < fn->upvalueCount; i++) {
        uint8_t isLocal = chunk->code[base++];
        uint8_t index = chunk->code[base++];
        uint8_t isMut = chunk->code[base++];
        debugPrint(runtime, "     |                          %s[%d] %s\n",
            isLocal ? "local" : "upval", index, isMut ? "mut" : "imm");
    }
    return base;
}

int disassembleInstruction(BTLRuntime* runtime, Chunk* chunk, int offset) {
    debugPrint(runtime, "%04d ", offset);

    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        debugPrint(runtime, "   | ");
    } else {
        debugPrint(runtime, "%4d ", chunk->lines[offset]);
    }

    uint8_t op = chunk->code[offset];

    switch (op) {
        // Constants
    case OP_CONSTANT:           return constant(runtime, "CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:      return constantLong(runtime, "CONSTANT_LONG", chunk, offset);
    case OP_NULL:               return simple(runtime, "NULL", offset);
    case OP_TRUE:               return simple(runtime, "TRUE", offset);
    case OP_FALSE:              return simple(runtime, "FALSE", offset);
    case OP_0:                  return simple(runtime, "PUSH_0", offset);
    case OP_1:                  return simple(runtime, "PUSH_1", offset);
    case OP_2:                  return simple(runtime, "PUSH_2", offset);

        // Stack
    case OP_POP:                return simple(runtime, "POP", offset);
    case OP_POP_N:              return byte_(runtime, "POP_N", chunk, offset);
    case OP_DUP:                return simple(runtime, "DUP", offset);
    case OP_SWAP:               return simple(runtime, "SWAP", offset);

        // Locals
    case OP_GET_LOCAL:          return byte_(runtime, "GET_LOCAL", chunk, offset);
    case OP_GET_LOCAL_0:        return simple(runtime, "GET_LOCAL_0", offset);
    case OP_GET_LOCAL_1:        return simple(runtime, "GET_LOCAL_1", offset);
    case OP_GET_LOCAL_2:        return simple(runtime, "GET_LOCAL_2", offset);
    case OP_GET_LOCAL_3:        return simple(runtime, "GET_LOCAL_3", offset);
    case OP_GET_LOCAL_4:        return simple(runtime, "GET_LOCAL_4", offset);
    case OP_GET_LOCAL_5:        return simple(runtime, "GET_LOCAL_5", offset);
    case OP_GET_LOCAL_6:        return simple(runtime, "GET_LOCAL_6", offset);
    case OP_GET_LOCAL_7:        return simple(runtime, "GET_LOCAL_7", offset);
    case OP_SET_LOCAL:          return byte_(runtime, "SET_LOCAL", chunk, offset);
    case OP_SET_LOCAL_0:        return simple(runtime, "SET_LOCAL_0", offset);
    case OP_SET_LOCAL_1:        return simple(runtime, "SET_LOCAL_1", offset);
    case OP_SET_LOCAL_2:        return simple(runtime, "SET_LOCAL_2", offset);
    case OP_SET_LOCAL_3:        return simple(runtime, "SET_LOCAL_3", offset);
    case OP_SET_LOCAL_4:        return simple(runtime, "SET_LOCAL_4", offset);
    case OP_SET_LOCAL_5:        return simple(runtime, "SET_LOCAL_5", offset);
    case OP_SET_LOCAL_6:        return simple(runtime, "SET_LOCAL_6", offset);
    case OP_SET_LOCAL_7:        return simple(runtime, "SET_LOCAL_7", offset);
    case OP_SET_LOCAL_0_POP:    return simple(runtime, "SET_LOCAL_0_POP", offset);
    case OP_SET_LOCAL_1_POP:    return simple(runtime, "SET_LOCAL_1_POP", offset);
    case OP_SET_LOCAL_2_POP:    return simple(runtime, "SET_LOCAL_2_POP", offset);
    case OP_SET_LOCAL_3_POP:    return simple(runtime, "SET_LOCAL_3_POP", offset);
    case OP_SET_LOCAL_4_POP:    return simple(runtime, "SET_LOCAL_4_POP", offset);
    case OP_SET_LOCAL_5_POP:    return simple(runtime, "SET_LOCAL_5_POP", offset);
    case OP_SET_LOCAL_6_POP:    return simple(runtime, "SET_LOCAL_6_POP", offset);
    case OP_SET_LOCAL_7_POP:    return simple(runtime, "SET_LOCAL_7_POP", offset);

        // Increment
    case OP_INC_LOCAL_POP:      return byte_(runtime, "INC_LOCAL_POP", chunk, offset);
    case OP_INC_LOCAL:          return byte_(runtime, "INC_LOCAL", chunk, offset);
    case OP_INCREMENT:          return simple(runtime, "INCREMENT", offset);
    case OP_DECREMENT:          return simple(runtime, "DECREMENT", offset);

        // Globals
    case OP_GET_GLOBAL:         return byte_(runtime, "GET_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL_LONG:    return shortOp(runtime, "GET_GLOBAL_LONG", chunk, offset);
    case OP_DEFINE_GLOBAL:      return byte_(runtime, "DEF_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL_LONG: return shortOp(runtime, "DEF_GLOBAL_LONG", chunk, offset);
    case OP_SET_GLOBAL:         return byte_(runtime, "SET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL_LONG:    return shortOp(runtime, "SET_GLOBAL_LONG", chunk, offset);

        // Upvalues
    case OP_GET_UPVALUE:            return byte_(runtime, "GET_UPVALUE", chunk, offset);
    case OP_GET_UPVALUE_OPEN:       return byte_(runtime, "GET_UPVALUE_OPEN", chunk, offset);
    case OP_GET_UPVALUE_CLOSED:     return byte_(runtime, "GET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_IMMUTABLE:  return byte_(runtime, "GET_UPVALUE_IMM", chunk, offset);
    case OP_SET_UPVALUE:            return byte_(runtime, "SET_UPVALUE", chunk, offset);
    case OP_SET_UPVALUE_OPEN:       return byte_(runtime, "SET_UPVALUE_OPEN", chunk, offset);
    case OP_SET_UPVALUE_CLOSED:     return byte_(runtime, "SET_UPVALUE_CLOSED", chunk, offset);
    case OP_GET_UPVALUE_0:          return simple(runtime, "GET_UPVALUE_0", offset);
    case OP_GET_UPVALUE_OPEN_0:     return simple(runtime, "GET_UPVALUE_OPEN_0", offset);
    case OP_GET_UPVALUE_CLOSED_0:   return simple(runtime, "GET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_IMMUTABLE_0:return simple(runtime, "GET_UPVALUE_IMM_0", offset);
    case OP_SET_UPVALUE_0:          return simple(runtime, "SET_UPVALUE_0", offset);
    case OP_SET_UPVALUE_OPEN_0:     return simple(runtime, "SET_UPVALUE_OPEN_0", offset);
    case OP_SET_UPVALUE_CLOSED_0:   return simple(runtime, "SET_UPVALUE_CLOSED_0", offset);
    case OP_GET_UPVALUE_1:          return simple(runtime, "GET_UPVALUE_1", offset);
    case OP_GET_UPVALUE_OPEN_1:     return simple(runtime, "GET_UPVALUE_OPEN_1", offset);
    case OP_GET_UPVALUE_CLOSED_1:   return simple(runtime, "GET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_IMMUTABLE_1:return simple(runtime, "GET_UPVALUE_IMM_1", offset);
    case OP_SET_UPVALUE_1:          return simple(runtime, "SET_UPVALUE_1", offset);
    case OP_SET_UPVALUE_OPEN_1:     return simple(runtime, "SET_UPVALUE_OPEN_1", offset);
    case OP_SET_UPVALUE_CLOSED_1:   return simple(runtime, "SET_UPVALUE_CLOSED_1", offset);
    case OP_GET_UPVALUE_2:          return simple(runtime, "GET_UPVALUE_2", offset);
    case OP_GET_UPVALUE_OPEN_2:     return simple(runtime, "GET_UPVALUE_OPEN_2", offset);
    case OP_GET_UPVALUE_CLOSED_2:   return simple(runtime, "GET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_IMMUTABLE_2:return simple(runtime, "GET_UPVALUE_IMM_2", offset);
    case OP_SET_UPVALUE_2:          return simple(runtime, "SET_UPVALUE_2", offset);
    case OP_SET_UPVALUE_OPEN_2:     return simple(runtime, "SET_UPVALUE_OPEN_2", offset);
    case OP_SET_UPVALUE_CLOSED_2:   return simple(runtime, "SET_UPVALUE_CLOSED_2", offset);
    case OP_GET_UPVALUE_3:          return simple(runtime, "GET_UPVALUE_3", offset);
    case OP_GET_UPVALUE_OPEN_3:     return simple(runtime, "GET_UPVALUE_OPEN_3", offset);
    case OP_GET_UPVALUE_CLOSED_3:   return simple(runtime, "GET_UPVALUE_CLOSED_3", offset);
    case OP_GET_UPVALUE_IMMUTABLE_3:return simple(runtime, "GET_UPVALUE_IMM_3", offset);
    case OP_SET_UPVALUE_3:          return simple(runtime, "SET_UPVALUE_3", offset);
    case OP_SET_UPVALUE_OPEN_3:     return simple(runtime, "SET_UPVALUE_OPEN_3", offset);
    case OP_SET_UPVALUE_CLOSED_3:   return simple(runtime, "SET_UPVALUE_CLOSED_3", offset);

        // Fields
    case OP_FIELD:              return constant(runtime, "FIELD", chunk, offset);
    case OP_GET_FIELD_THIS:     return byte_(runtime, "GET_FIELD_THIS", chunk, offset);
    case OP_SET_FIELD_THIS:     return byte_(runtime, "SET_FIELD_THIS", chunk, offset);

        // Property IC
    case OP_GET_PROPERTY_IC:    return propertyIC(runtime, "GET_PROP_IC", chunk, offset);
    case OP_SET_PROPERTY_IC:    return propertyIC(runtime, "SET_PROP_IC", chunk, offset);

        // Super
    case OP_GET_SUPER:          return constant(runtime, "GET_SUPER", chunk, offset);
    case OP_GET_SUPER_LONG:     return constantLong(runtime, "GET_SUPER_LONG", chunk, offset);

        // Arithmetic
    case OP_EQUAL:              return simple(runtime, "EQUAL", offset);
    case OP_GREATER:            return simple(runtime, "GREATER", offset);
    case OP_LESS:               return simple(runtime, "LESS", offset);
    case OP_ADD:                return simple(runtime, "ADD", offset);
    case OP_SUBTRACT:           return simple(runtime, "SUB", offset);
    case OP_MULTIPLY:           return simple(runtime, "MUL", offset);
    case OP_DIVIDE:             return simple(runtime, "DIV", offset);
    case OP_MODULO:             return simple(runtime, "MOD", offset);
    case OP_NOT:                return simple(runtime, "NOT", offset);
    case OP_NEGATE:             return simple(runtime, "NEG", offset);

        // Jumps
    case OP_JUMP:               return jump(runtime, "JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:      return jump(runtime, "JUMP_IF_FALSE", 1, chunk, offset);
    case OP_POP_JUMP_IF_FALSE:  return jump(runtime, "POP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_JUMP_IF_TRUE:       return jump(runtime, "JUMP_IF_TRUE", 1, chunk, offset);
    case OP_POP_JUMP_IF_TRUE:   return jump(runtime, "POP_JUMP_IF_TRUE", 1, chunk, offset);
    case OP_JUMP_IF_NOT_EQUAL:  return jump(runtime, "JUMP_IF_NEQ", 1, chunk, offset);
    case OP_JUMP_IF_EQUAL:      return jump(runtime, "JUMP_IF_EQ", 1, chunk, offset);
    case OP_JUMP_IF_NOT_GREATER:return jump(runtime, "JUMP_IF_NGT", 1, chunk, offset);
    case OP_JUMP_IF_NOT_LESS:   return jump(runtime, "JUMP_IF_NLT", 1, chunk, offset);
    case OP_LOOP:               return jump(runtime, "LOOP", -1, chunk, offset);

        // Calls
    case OP_CALL_0:             return simple(runtime, "CALL_0", offset);
    case OP_CALL_1:             return simple(runtime, "CALL_1", offset);
    case OP_CALL_2:             return simple(runtime, "CALL_2", offset);
    case OP_CALL_3:             return simple(runtime, "CALL_3", offset);
    case OP_CALL_4:             return simple(runtime, "CALL_4", offset);
    case OP_CALL_5:             return simple(runtime, "CALL_5", offset);
    case OP_CALL_6:             return simple(runtime, "CALL_6", offset);
    case OP_CALL_7:             return simple(runtime, "CALL_7", offset);
    case OP_CALL_8:             return simple(runtime, "CALL_8", offset);
    case OP_CALL:               return byte_(runtime, "CALL", chunk, offset);

        // Tail Calls
    case OP_TAIL_CALL_0:        return simple(runtime, "TAIL_CALL_0", offset);
    case OP_TAIL_CALL_1:        return simple(runtime, "TAIL_CALL_1", offset);
    case OP_TAIL_CALL_2:        return simple(runtime, "TAIL_CALL_2", offset);
    case OP_TAIL_CALL_3:        return simple(runtime, "TAIL_CALL_3", offset);
    case OP_TAIL_CALL_4:        return simple(runtime, "TAIL_CALL_4", offset);
    case OP_TAIL_CALL_5:        return simple(runtime, "TAIL_CALL_5", offset);
    case OP_TAIL_CALL_6:        return simple(runtime, "TAIL_CALL_6", offset);
    case OP_TAIL_CALL_7:        return simple(runtime, "TAIL_CALL_7", offset);
    case OP_TAIL_CALL_8:        return simple(runtime, "TAIL_CALL_8", offset);
    case OP_TAIL_CALL:          return byte_(runtime, "TAIL_CALL", chunk, offset);

        // Invoke (indexed)
    case OP_INVOKE_0:           return invokeIndexed(runtime, "INVOKE_0", 0, chunk, offset);
    case OP_INVOKE_1:           return invokeIndexed(runtime, "INVOKE_1", 1, chunk, offset);
    case OP_INVOKE_2:           return invokeIndexed(runtime, "INVOKE_2", 2, chunk, offset);
    case OP_INVOKE_3:           return invokeIndexed(runtime, "INVOKE_3", 3, chunk, offset);
    case OP_INVOKE_4:           return invokeIndexed(runtime, "INVOKE_4", 4, chunk, offset);
    case OP_INVOKE_5:           return invokeIndexed(runtime, "INVOKE_5", 5, chunk, offset);
    case OP_INVOKE_6:           return invokeIndexed(runtime, "INVOKE_6", 6, chunk, offset);
    case OP_INVOKE_7:           return invokeIndexed(runtime, "INVOKE_7", 7, chunk, offset);
    case OP_INVOKE_8:           return invokeIndexed(runtime, "INVOKE_8", 8, chunk, offset);
    case OP_INVOKE:             return invoke(runtime, "INVOKE", chunk, offset);
    case OP_INVOKE_LONG:        return invokeLong(runtime, "INVOKE_LONG", chunk, offset);

        // Invoke IC
    case OP_INVOKE_IC:          return invokeIC(runtime, "INVOKE_IC", chunk, offset);

        // Tail Invoke (indexed)
    case OP_TAIL_INVOKE_0:      return invokeIndexed(runtime, "TAIL_INVOKE_0", 0, chunk, offset);
    case OP_TAIL_INVOKE_1:      return invokeIndexed(runtime, "TAIL_INVOKE_1", 1, chunk, offset);
    case OP_TAIL_INVOKE_2:      return invokeIndexed(runtime, "TAIL_INVOKE_2", 2, chunk, offset);
    case OP_TAIL_INVOKE_3:      return invokeIndexed(runtime, "TAIL_INVOKE_3", 3, chunk, offset);
    case OP_TAIL_INVOKE_4:      return invokeIndexed(runtime, "TAIL_INVOKE_4", 4, chunk, offset);
    case OP_TAIL_INVOKE_5:      return invokeIndexed(runtime, "TAIL_INVOKE_5", 5, chunk, offset);
    case OP_TAIL_INVOKE_6:      return invokeIndexed(runtime, "TAIL_INVOKE_6", 6, chunk, offset);
    case OP_TAIL_INVOKE_7:      return invokeIndexed(runtime, "TAIL_INVOKE_7", 7, chunk, offset);
    case OP_TAIL_INVOKE_8:      return invokeIndexed(runtime, "TAIL_INVOKE_8", 8, chunk, offset);
    case OP_TAIL_INVOKE:        return invoke(runtime, "TAIL_INVOKE", chunk, offset);
    case OP_TAIL_INVOKE_LONG:   return invokeLong(runtime, "TAIL_INVOKE_LONG", chunk, offset);

        // Tail Invoke IC
    case OP_TAIL_INVOKE_IC:     return invokeIC(runtime, "TAIL_INVOKE_IC", chunk, offset);

        // Super Invoke (indexed)
    case OP_SUPER_INVOKE_0:     return invokeIndexed(runtime, "SUPER_INVOKE_0", 0, chunk, offset);
    case OP_SUPER_INVOKE_1:     return invokeIndexed(runtime, "SUPER_INVOKE_1", 1, chunk, offset);
    case OP_SUPER_INVOKE_2:     return invokeIndexed(runtime, "SUPER_INVOKE_2", 2, chunk, offset);
    case OP_SUPER_INVOKE_3:     return invokeIndexed(runtime, "SUPER_INVOKE_3", 3, chunk, offset);
    case OP_SUPER_INVOKE_4:     return invokeIndexed(runtime, "SUPER_INVOKE_4", 4, chunk, offset);
    case OP_SUPER_INVOKE_5:     return invokeIndexed(runtime, "SUPER_INVOKE_5", 5, chunk, offset);
    case OP_SUPER_INVOKE_6:     return invokeIndexed(runtime, "SUPER_INVOKE_6", 6, chunk, offset);
    case OP_SUPER_INVOKE_7:     return invokeIndexed(runtime, "SUPER_INVOKE_7", 7, chunk, offset);
    case OP_SUPER_INVOKE_8:     return invokeIndexed(runtime, "SUPER_INVOKE_8", 8, chunk, offset);
    case OP_SUPER_INVOKE:       return invoke(runtime, "SUPER_INVOKE", chunk, offset);
    case OP_SUPER_INVOKE_LONG:  return invokeLong(runtime, "SUPER_INVOKE_LONG", chunk, offset);

        // Tail Super Invoke (indexed)
    case OP_TAIL_SUPER_INVOKE_0:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_0", 0, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_1:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_1", 1, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_2:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_2", 2, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_3:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_3", 3, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_4:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_4", 4, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_5:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_5", 5, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_6:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_6", 6, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_7:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_7", 7, chunk, offset);
    case OP_TAIL_SUPER_INVOKE_8:return invokeIndexed(runtime, "TAIL_SUPER_INVOKE_8", 8, chunk, offset);
    case OP_TAIL_SUPER_INVOKE:  return invoke(runtime, "TAIL_SUPER_INVOKE", chunk, offset);
    case OP_TAIL_SUPER_INVOKE_LONG: return invokeLong(runtime, "TAIL_SUPER_INVOKE_LONG", chunk, offset);

        // Closures
    case OP_CLOSURE:            return closure(runtime, "CLOSURE", chunk, offset, false);
    case OP_CLOSURE_LONG:       return closure(runtime, "CLOSURE_LONG", chunk, offset, true);
    case OP_CLOSE_UPVALUE:      return simple(runtime, "CLOSE_UPVALUE", offset);

        // Return
    case OP_RETURN:             return simple(runtime, "RETURN", offset);

        // Classes
    case OP_CLASS:              return constant(runtime, "CLASS", chunk, offset);
    case OP_CLASS_LONG:         return constantLong(runtime, "CLASS_LONG", chunk, offset);
    case OP_INHERIT:            return simple(runtime, "INHERIT", offset);
    case OP_METHOD:             return method(runtime, "METHOD", chunk, offset);
    case OP_METHOD_LONG:        return methodLong(runtime, "METHOD_LONG", chunk, offset);

        // Collections
    case OP_BUILD_LIST:         return byte_(runtime, "BUILD_LIST", chunk, offset);
    case OP_BUILD_TABLE:        return byte_(runtime, "BUILD_TABLE", chunk, offset);
    case OP_INDEX_GET:          return simple(runtime, "INDEX_GET", offset);
    case OP_INDEX_SET:          return simple(runtime, "INDEX_SET", offset);

        // Import
    case OP_IMPORT:             return constant(runtime, "IMPORT", chunk, offset);
    case OP_IMPORT_LONG:        return constantLong(runtime, "IMPORT_LONG", chunk, offset);

    case OP_DO_NEW:             return byte_(runtime, "OP_DO_NEW", chunk, offset);
    case OP_DO_INVOKE:          return invoke(runtime, "OP_DO_INVOKE", chunk, offset);

    default:
        debugPrint(runtime, "UNKNOWN_OP %d\n", op);
        return offset + 1;
    }
}