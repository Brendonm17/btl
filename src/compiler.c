#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"
#include "runtime.h"

#ifdef BTL_DEBUG_PRINT_CODE
#include "debug.h"
#endif

// --- Parser & Grammar ---

// Parser state for the compiler
typedef struct {
    BtlToken current;       // Current token being processed
    BtlToken previous;      // Previously consumed token
    bool hadError;          // True if any error occurred
    bool panicMode;         // True when recovering from error
    struct VM* vm;          // VM for error reporting
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    bool isConstant;
    BtlValue value;
    int length;
} LastInstruction;

// --- Forward Declarations ---

static void expression(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc);
static void statement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc);
static void declaration(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc);
static void function(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, BtlFunctionType type);
static void switchStatement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool isStatement);
static int parseVariable(Parser* p, BtlScanner* s, BtlCompiler* c, const char* errorMessage);
static void defineVariable(Parser* p, BtlCompiler* c, int global);
static ParseRule* getRule(BtlTokenType type);
static void parsePrecedence(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, Precedence precedence);
static void emitLong(Parser* p, BtlCompiler* c, BtlOpCode shortOp, BtlOpCode longOp, uint32_t index);
static int makeConstant(Parser* p, BtlCompiler* c, BtlValue value);
static void emitConstant(Parser* p, BtlCompiler* c, BtlValue value);
static int resolveLocal(Parser* p, BtlCompiler* c, BtlToken* name);
static int resolveUpvalue(Parser* p, BtlCompiler* c, BtlToken* name);
static void emitVariableSet(Parser* p, BtlCompiler* c, BtlClassCompiler* cc, BtlToken name);
static void prefixIncDec(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign);
static void doExpr(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign);


// --- Chunk Management ---

// Returns the bytecode chunk for the current function being compiled
static BtlChunk* currentChunk(BtlCompiler* c) {
    return &c->function->chunk;
}

// Report an error at a specific token location
static void errorAt(Parser* p, BtlToken* token, const char* message) {
    if (p->panicMode) return;
    p->panicMode = true;
    p->hadError = true;
    btl_errorf(p->vm, "[line %d] Error", token->line);

    if (token->type == BTL_TOKEN_EOF) {
        btl_error(p->vm, " at end");
    } else if (token->type != BTL_TOKEN_ERROR) {
        btl_errorf(p->vm, " at '%.*s'", token->length, token->start);
    }

    btl_errorf(p->vm, ": %s\n", message);
}

// Advance to the next token from the scanner
static void advance(Parser* p, BtlScanner* s) {
    p->previous = p->current;
    for (;;) {
        p->current = btl_scanner_scan_token(s);
        if (p->current.type != BTL_TOKEN_ERROR) break;
        errorAt(p, &p->current, p->current.start);
    }
}

// Consume expected token type or report error
static void consume(Parser* p, BtlScanner* s, BtlTokenType type, const char* message) {
    if (p->current.type == type) {
        advance(p, s);
        return;
    }
    errorAt(p, &p->current, message);
}

// Check if current token matches type
static bool check(Parser* p, BtlTokenType type) {
    return p->current.type == type;
}

// Match current token and advance if it matches
static bool match(Parser* p, BtlScanner* s, BtlTokenType type) {
    if (!check(p, type)) return false;
    advance(p, s);
    return true;
}

// Emit a single bytecode instruction
static void emitByte(Parser* p, BtlCompiler* c, uint8_t byte) {
    c->previousInstruction = c->lastInstruction;
    c->lastInstruction = currentChunk(c)->count;
    btl_chunk_write(c->vm, currentChunk(c), byte, p->previous.line);
}

// Emit two bytecode bytes
static void emitBytes(Parser* p, BtlCompiler* c, uint8_t byte1, uint8_t byte2) {
    emitByte(p, c, byte1);
    btl_chunk_write(c->vm, currentChunk(c), byte2, p->previous.line);
}

// Creates method signature string: "methodName\arity"
static ObjString* createMethodSignature(BtlCompiler* c, BtlToken* name, int arity) {
    int nameLen = name->length;
    char* buffer = BTL_ALLOCATE(c->vm, char, nameLen + 2);
    memcpy(buffer, name->start, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = btl_string_copy(c->vm, buffer, nameLen + 1);
    BTL_FREE_ARRAY(c->vm, char, buffer, nameLen + 2);
    return signature;
}

// Emit indexed invoke with optimized opcodes for 0-8 args
static void emitInvokeIndexed(Parser* p, BtlCompiler* c, int methodIndex, int argCount) {
    if (argCount <= 8 && methodIndex < 256) {
        emitBytes(p, c, (uint8_t) (BTL_OP_INVOKE_0 + argCount), (uint8_t) methodIndex);
    } else if (methodIndex < 256) {
        emitByte(p, c, BTL_OP_INVOKE);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    } else {
        emitByte(p, c, BTL_OP_INVOKE_LONG);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    }
}

// Emit indexed super invoke with optimized opcodes for 0-8 args
static void emitSuperInvokeIndexed(Parser* p, BtlCompiler* c, int methodIndex, int argCount) {
    if (argCount <= 8 && methodIndex < 256) {
        emitBytes(p, c, (uint8_t) (BTL_OP_SUPER_INVOKE_0 + argCount), (uint8_t) methodIndex);
    } else if (methodIndex < 256) {
        emitByte(p, c, BTL_OP_SUPER_INVOKE);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    } else {
        emitByte(p, c, BTL_OP_SUPER_INVOKE_LONG);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    }
}

// Try to resolve method index at compile time
// Returns -1 if not possible (polymorphic call)
static int tryResolveMethodIndex(BtlCompiler* c, BtlClassCompiler* cc, BtlToken* name, int argCount) {
    if (cc == NULL) return -1;

    ObjString* signature = createMethodSignature(c, name, argCount);
    btl_push(c->vm, OBJ_VAL(signature));

    BtlValue indexValue;
    int methodIndex = -1;
    if (btl_table_get(&cc->methodIndices, OBJ_VAL(signature), &indexValue)) {
        methodIndex = (int) AS_NUMBER(indexValue);
    }

    btl_pop(c->vm);
    return methodIndex;
}

static BtlToken syntheticToken(const char* text) {
    BtlToken token;
    token.start = text;
    token.length = (int) strlen(text);
    token.line = 0;
    token.type = BTL_TOKEN_IDENTIFIER;
    return token;
}

static void addLocal(Parser* p, BtlCompiler* c, BtlToken name) {
    if (c->localCount == 256) {
        errorAt(p, &name, "Too many local variables in function.");
        return;
    }

    BtlLocal* local = &c->locals[c->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
    local->isModified = false;
}

// --- Back-Patching Logic ---

static void markLocalAsModified(BtlCompiler* c, int localIndex) {
    c->locals[localIndex].isModified = true;
    for (int i = 0; i < c->patchCount; i++) {
        if (c->patches[i].localIndex == localIndex) {
            int offset = c->patches[i].codeOffset;
            currentChunk(c)->code[offset] = 1;
        }
    }
}

static void addPatch(BtlCompiler* c, int localIndex, int codeOffset) {
    if (c->patchCount < BTL_UINT8_COUNT) {
        c->patches[c->patchCount].localIndex = localIndex;
        c->patches[c->patchCount].codeOffset = codeOffset;
        c->patchCount++;
    }
}

// --- Bytecode Helpers ---

static void removeChunkTail(BtlChunk* chunk, int n) {
    if (n <= 0) return;
    if (n > chunk->count) n = chunk->count;
    chunk->count -= n;
}

static void emitPopOrRemoveLoad(Parser* p, BtlCompiler* c) {
    BtlChunk* chunk = currentChunk(c);
    if (c->lastInstruction >= 0 && c->lastInstruction < chunk->count) {
        uint8_t prevOp = chunk->code[c->lastInstruction];
        if (prevOp == BTL_OP_RETURN) return;
        // If the last instruction is a 2-byte GET_LOCAL or GET_UPVALUE,
        // remove it instead of emitting POP (the value is unused).
        if ((prevOp == BTL_OP_GET_LOCAL || prevOp == BTL_OP_GET_UPVALUE) &&
            c->lastInstruction + 2 == chunk->count) {
            removeChunkTail(chunk, 2);
            c->lastInstruction = c->previousInstruction;
            c->previousInstruction = -1;
            return;
        }
    }
    emitByte(p, c, BTL_OP_POP);
}

static void emitPopN(Parser* p, BtlCompiler* c, unsigned int count) {
    if (count == 0) return;
    if (count == 1) {
        emitPopOrRemoveLoad(p, c);
        return;
    }
    const unsigned int CHUNK = 255;
    while (count > CHUNK) {
        emitBytes(p, c, BTL_OP_POP_N, (uint8_t) CHUNK);
        count -= CHUNK;
    }
    if (count == 1) {
        emitPopOrRemoveLoad(p, c);
    } else {
        emitBytes(p, c, BTL_OP_POP_N, (uint8_t) count);
    }
}

static void emitConstant(Parser* p, BtlCompiler* c, BtlValue value) {
    emitLong(p, c, BTL_OP_CONSTANT, BTL_OP_CONSTANT_LONG, makeConstant(p, c, value));
}

static LastInstruction getInstructionAt(BtlCompiler* c, int offset) {
    BtlChunk* chunk = currentChunk(c);
    LastInstruction result = { .isConstant = false, .value = BTL_NULL_VAL, .length = 0 };
    if (offset < 0 || offset >= chunk->count) return result;

    uint8_t op = chunk->code[offset];
    if (op == BTL_OP_CONSTANT) {
        uint8_t index = chunk->code[offset + 1];
        result.isConstant = true;
        result.value = chunk->constants.values[index];
        result.length = 2;
    } else if (op == BTL_OP_CONSTANT_LONG) {
        uint8_t lo = chunk->code[offset + 1];
        uint8_t hi = chunk->code[offset + 2];
        uint16_t index = (hi << 8) | lo;
        result.isConstant = true;
        result.value = chunk->constants.values[index];
        result.length = 3;
    }
    return result;
}

static void emitLoop(Parser* p, BtlCompiler* c, int loopStart) {
    emitByte(p, c, BTL_OP_LOOP);
    int offset = currentChunk(c)->count - loopStart + 2;
    emitByte(p, c, (offset >> 8) & 0xff);
    emitByte(p, c, offset & 0xff);
}

static int emitJump(Parser* p, BtlCompiler* c, uint8_t instruction) {
    emitByte(p, c, instruction);
    btl_chunk_write(c->vm, currentChunk(c), 0xff, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), 0xff, p->previous.line);
    return currentChunk(c)->count - 2;
}

static int emitFusedJump(Parser* p, BtlCompiler* c, uint8_t defaultJump) {
    BtlChunk* chunk = currentChunk(c);

    // Don't fuse if an and_/or_ short-circuit jump was just patched to
    // chunk->count.  Removing the last comparison opcode would shift the
    // chunk boundary and leave that jump pointing into an operand byte.
    if (c->inhibitFusion) {
        c->inhibitFusion = false;
        return emitJump(p, c, defaultJump);
    }

    if (chunk->count > 0) {
        uint8_t lastOp = chunk->code[chunk->count - 1];
        uint8_t fusedOp = 0;

        switch (lastOp) {
        case BTL_OP_EQUAL:   fusedOp = BTL_OP_JUMP_IF_NOT_EQUAL; break;
        case BTL_OP_GREATER: fusedOp = BTL_OP_JUMP_IF_NOT_GREATER; break;
        case BTL_OP_LESS:    fusedOp = BTL_OP_JUMP_IF_NOT_LESS; break;
        case BTL_OP_NOT:
            if (chunk->count > 1 && chunk->code[chunk->count - 2] == BTL_OP_EQUAL) {
                removeChunkTail(chunk, 1);
                lastOp = BTL_OP_EQUAL;
                fusedOp = BTL_OP_JUMP_IF_EQUAL;
            }
            break;
        }

        if (fusedOp != 0) {
            removeChunkTail(chunk, 1);
            return emitJump(p, c, fusedOp);
        }
    }

    return emitJump(p, c, defaultJump);
}

static int makeConstant(Parser* p, BtlCompiler* c, BtlValue value) {
    BtlValue existingIndex;
    if (btl_table_get(&c->constants, value, &existingIndex)) {
        return (int) AS_NUMBER(existingIndex);
    }
    btl_push(c->vm, value);
    int constant = btl_chunk_add_constant(c->vm, currentChunk(c), value);
    if (constant > UINT16_MAX) {
        errorAt(p, &p->previous, "Too many constants in chunk.");
        btl_pop(c->vm);
        return 0;
    }
    btl_table_set(c->vm, &c->constants, value, NUMBER_VAL((double) constant));
    btl_pop(c->vm);
    return constant;
}

static void emitLong(Parser* p, BtlCompiler* c, BtlOpCode shortOp, BtlOpCode longOp, uint32_t index) {
    if (index < 256) {
        emitByte(p, c, shortOp);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) index, p->previous.line);
    } else {
        emitByte(p, c, longOp);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) (index & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) ((index >> 8) & 0xff), p->previous.line);
    }
}

static void patchJump(Parser* p, BtlCompiler* c, int offset) {
    int jump = currentChunk(c)->count - offset - 2;
    if (jump > UINT16_MAX) errorAt(p, &p->previous, "Too much code to jump over.");
    currentChunk(c)->code[offset] = (jump >> 8) & 0xff;
    currentChunk(c)->code[offset + 1] = jump & 0xff;
}

static void emitGetPropertyIC(Parser* p, BtlCompiler* c, int nameIdx) {
    if (nameIdx > 255 || c->fieldICCount > 255) {
        errorAt(p, &p->previous, "Too many property accesses in function.");
        return;
    }
    emitByte(p, c, BTL_OP_GET_PROPERTY_IC);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) c->fieldICCount++, p->previous.line);
}

static void emitSetPropertyIC(Parser* p, BtlCompiler* c, int nameIdx) {
    if (nameIdx > 255 || c->fieldICCount > 255) {
        errorAt(p, &p->previous, "Too many property accesses in function.");
        return;
    }
    emitByte(p, c, BTL_OP_SET_PROPERTY_IC);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) c->fieldICCount++, p->previous.line);
}

static void emitInvokeIC(Parser* p, BtlCompiler* c, int nameIdx, int argCount) {
    if (nameIdx > 255 || c->methodICCount > 255) {
        errorAt(p, &p->previous, "Too many method calls in function.");
        return;
    }
    emitByte(p, c, BTL_OP_INVOKE_IC);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), (uint8_t) c->methodICCount++, p->previous.line);
}

static void initCompiler(Parser* p, BtlCompiler* c, BtlCompiler* enclosing, BtlFunctionType type, ObjModule* module) {
    c->enclosing = enclosing;
    c->function = NULL;
    c->type = type;
    c->localCount = 0;
    c->scopeDepth = 0;
    c->lastInstruction = -1;
    c->previousInstruction = -1;
    c->vm = p->vm;
    c->module = module;
    c->patchCount = 0;
    btl_table_init(&c->constants);
    c->currentLoop = NULL;
    c->currentSwitch = NULL;
    c->fieldICCount = 0;
    c->methodICCount = 0;
    c->inhibitFusion = false;
    c->function = btl_function_new(p->vm, module);
    c->vm->compiler = (void*) c;

    if (type != BTL_TYPE_SCRIPT) {
        c->function->name = btl_string_copy(p->vm, p->previous.start, p->previous.length);
    }

    BtlLocal* local = &c->locals[c->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->isModified = false;

    if (type != BTL_TYPE_SCRIPT) {
        if (type == BTL_TYPE_METHOD || type == BTL_TYPE_INITIALIZER) {
            local->name.start = "this";
            local->name.length = 4;
        } else {
            local->name.start = p->previous.start;
            local->name.length = p->previous.length;
        }
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler(Parser* p, BtlCompiler* c) {
    if (c->type == BTL_TYPE_INITIALIZER || c->type == BTL_TYPE_SCRIPT) {
        emitBytes(p, c, BTL_OP_GET_LOCAL, 0);
    } else {
        emitByte(p, c, BTL_OP_NULL);
    }
    emitByte(p, c, BTL_OP_RETURN);
    ObjFunction* function = c->function;
    function->fieldICCount = c->fieldICCount;
    function->methodICCount = c->methodICCount;
#ifdef BTL_DEBUG_PRINT_CODE
    if (!p->hadError) btl_disassemble_chunk(c->vm->runtime, currentChunk(c), function->name != NULL ? function->name->chars : "<script>");
#endif
    btl_table_free(c->vm, &c->constants);
    c->vm->compiler = (void*) c->enclosing;
    return function;
}

static void beginScope(BtlCompiler* c) {
    c->scopeDepth++;
}

static void endScope(Parser* p, BtlCompiler* c) {
    c->scopeDepth--;
    int popCount = 0;
    while (c->localCount > 0 && c->locals[c->localCount - 1].depth > c->scopeDepth) {
        if (c->locals[c->localCount - 1].isCaptured) {
            if (popCount) {
                emitPopN(p, c, popCount);
                popCount = 0;
            }
            emitByte(p, c, BTL_OP_CLOSE_UPVALUE);
        } else {
            popCount++;
        }
        c->localCount--;
    }
    if (popCount) emitPopN(p, c, popCount);
}

static bool identifiersEqual(BtlToken* a, BtlToken* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int identifierConstant(BtlCompiler* c, BtlToken* name) {
    ObjString* nameString = btl_string_copy(c->vm, name->start, name->length);
    btl_push(c->vm, OBJ_VAL(nameString));
    BtlValue indexValue;
    if (btl_table_get(&c->module->globalNames, OBJ_VAL(nameString), &indexValue)) {
        btl_pop(c->vm);
        return (int) AS_NUMBER(indexValue);
    }
    int index = c->module->globalValues.count;
    btl_value_array_write(c->vm, &c->module->globalValues, BTL_EMPTY_VAL);
    btl_table_set(c->vm, &c->module->globalNames, OBJ_VAL(nameString), NUMBER_VAL((double) index));
    btl_pop(c->vm);
    return index;
}

static int resolveLocal(Parser* p, BtlCompiler* c, BtlToken* name) {
    for (int i = c->localCount - 1; i >= 0; i--) {
        if (identifiersEqual(name, &c->locals[i].name)) {
            if (c->locals[i].depth == -1) errorAt(p, name, "Can't read local variable in its own initializer.");
            return i;
        }
    }
    return -1;
}

static int addUpvalue(BtlCompiler* c, uint8_t index, bool isLocal, bool isMutable) {
    int count = c->function->upvalueCount;
    for (int i = 0; i < count; i++) {
        if (c->upvalues[i].index == index && c->upvalues[i].isLocal == isLocal) return i;
    }
    c->upvalues[count].isLocal = isLocal;
    c->upvalues[count].index = index;
    c->upvalues[count].isMutable = isMutable;
    return c->function->upvalueCount++;
}

static int resolveUpvalue(Parser* p, BtlCompiler* c, BtlToken* name) {
    if (c->enclosing == NULL) return -1;

    int local = resolveLocal(p, c->enclosing, name);
    if (local != -1) {
        c->enclosing->locals[local].isCaptured = true;
        return addUpvalue(c, (uint8_t) local, true, c->enclosing->locals[local].isModified);
    }

    int upvalue = resolveUpvalue(p, c->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(c, (uint8_t) upvalue, false, c->enclosing->upvalues[upvalue].isMutable);
    }
    return -1;
}

static void emitUpvalue(Parser* p, BtlCompiler* c, uint8_t arg, bool isSet) {
    if (isSet) {
        if (arg == 0) emitByte(p, c, BTL_OP_SET_UPVALUE_0);
        else if (arg == 1) emitByte(p, c, BTL_OP_SET_UPVALUE_1);
        else if (arg == 2) emitByte(p, c, BTL_OP_SET_UPVALUE_2);
        else if (arg == 3) emitByte(p, c, BTL_OP_SET_UPVALUE_3);
        else emitBytes(p, c, BTL_OP_SET_UPVALUE, arg);
    } else {
        if (arg == 0) emitByte(p, c, BTL_OP_GET_UPVALUE_0);
        else if (arg == 1) emitByte(p, c, BTL_OP_GET_UPVALUE_1);
        else if (arg == 2) emitByte(p, c, BTL_OP_GET_UPVALUE_2);
        else if (arg == 3) emitByte(p, c, BTL_OP_GET_UPVALUE_3);
        else emitBytes(p, c, BTL_OP_GET_UPVALUE, arg);
    }
}

static void emitVariableSet(Parser* p, BtlCompiler* c, BtlClassCompiler* cc, BtlToken name) {
    int arg = resolveLocal(p, c, &name);

    if (arg != -1) {
        markLocalAsModified(c, arg);
        if (arg <= 7) {
            emitByte(p, c, (uint8_t) (BTL_OP_SET_LOCAL_0 + arg));
        } else {
            emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) arg);
        }
        return;
    }

    arg = resolveUpvalue(p, c, &name);
    if (arg != -1) {
        if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
            markLocalAsModified(c->enclosing, c->upvalues[arg].index);
        }
        emitUpvalue(p, c, (uint8_t) arg, true);
        return;
    }

    if (cc != NULL) {
        ObjString* fieldName = btl_string_copy(c->vm, name.start, name.length);
        BtlValue indexVal;
        if (btl_table_get(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            uint8_t index = (uint8_t) AS_NUMBER(indexVal);
            emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
            return;
        }
    }

    arg = identifierConstant(c, &name);
    emitLong(p, c, BTL_OP_SET_GLOBAL, BTL_OP_SET_GLOBAL_LONG, arg);
}

static void namedVariable(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, BtlToken name, bool canAssign) {
    int arg = resolveLocal(p, c, &name);

    if (arg != -1) {
        // --- LOCAL VARIABLE ---
        if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
            int exprStart = currentChunk(c)->count;
            expression(p, s, c, cc);

            // Lookback Optimization for ++ (e.g., i = i + 1)
            BtlChunk* chunk = currentChunk(c);
            if (chunk->count >= exprStart + 3) {
                uint8_t op1 = chunk->code[exprStart];
                uint8_t op2 = chunk->code[exprStart + 1];
                uint8_t op3 = chunk->code[exprStart + 2];
                bool isCorrectVar = ((op1 == (uint8_t) (BTL_OP_GET_LOCAL_0 + arg)) && arg <= 7) ||
                    (op1 == BTL_OP_GET_LOCAL && chunk->code[exprStart + 1] == arg);
                bool isPlusOne = (op2 == BTL_OP_1 && op3 == BTL_OP_ADD) || (op3 == BTL_OP_1 && op2 == BTL_OP_ADD);
                if (isCorrectVar && isPlusOne) {
                    chunk->count = exprStart;
                    emitBytes(p, c, BTL_OP_INC_LOCAL, (uint8_t) arg);
                    markLocalAsModified(c, arg);
                    return;
                }
            }
            markLocalAsModified(c, arg);
            if (arg <= 7) emitByte(p, c, (uint8_t) (BTL_OP_SET_LOCAL_0 + arg));
            else emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) arg);
        } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
            match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
            match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {

            BtlTokenType assignOp = p->previous.type;

            // Load current value
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (BTL_OP_GET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) arg);
            }

            expression(p, s, c, cc);

            // Apply the operation
            switch (assignOp) {
            case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
            case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
            case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
            case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
            case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
            default: break;
            }

            markLocalAsModified(c, arg);
            if (arg <= 7) emitByte(p, c, (uint8_t) (BTL_OP_SET_LOCAL_0 + arg));
            else emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) arg);
        } else {
            if (arg <= 7) emitByte(p, c, (uint8_t) (BTL_OP_GET_LOCAL_0 + arg));
            else emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) arg);
        }
        return;
    }

    arg = resolveUpvalue(p, c, &name);
    if (arg != -1) {
        // --- UPVALUE ---
        if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }
            expression(p, s, c, cc);
            emitUpvalue(p, c, (uint8_t) arg, true);
        } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
            match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
            match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {

            BtlTokenType assignOp = p->previous.type;

            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }

            // Load current value
            emitUpvalue(p, c, (uint8_t) arg, false);

            expression(p, s, c, cc);

            switch (assignOp) {
            case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
            case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
            case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
            case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
            case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
            default: break;
            }

            emitUpvalue(p, c, (uint8_t) arg, true);
        } else {
            emitUpvalue(p, c, (uint8_t) arg, false);
        }
        return;
    }

    if (cc != NULL) {
        // --- CLASS FIELD ---
        ObjString* fieldName = btl_string_copy(c->vm, name.start, name.length);
        BtlValue indexVal;
        if (btl_table_get(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            uint8_t index = (uint8_t) AS_NUMBER(indexVal);
            if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
                expression(p, s, c, cc);
                emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
            } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
                match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
                match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {

                BtlTokenType assignOp = p->previous.type;

                // Load current value
                emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);

                expression(p, s, c, cc);

                switch (assignOp) {
                case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
                case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
                case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
                case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
                case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
                default: break;
                }

                emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
            } else {
                emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
            }
            return;
        }
    }

    // --- GLOBAL VARIABLE ---
    arg = identifierConstant(c, &name);
    if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitLong(p, c, BTL_OP_SET_GLOBAL, BTL_OP_SET_GLOBAL_LONG, arg);
    } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
        match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
        match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {

        BtlTokenType assignOp = p->previous.type;

        // Load current value
        emitLong(p, c, BTL_OP_GET_GLOBAL, BTL_OP_GET_GLOBAL_LONG, arg);

        expression(p, s, c, cc);

        switch (assignOp) {
        case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
        case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
        case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
        case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
        case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
        default: break;
        }

        emitLong(p, c, BTL_OP_SET_GLOBAL, BTL_OP_SET_GLOBAL_LONG, arg);
    } else {
        emitLong(p, c, BTL_OP_GET_GLOBAL, BTL_OP_GET_GLOBAL_LONG, arg);
    }
}

static void markInitialized(BtlCompiler* c) {
    if (c->scopeDepth == 0) return;
    c->locals[c->localCount - 1].depth = c->scopeDepth;
}

static void declareVariable(Parser* p, BtlCompiler* c) {
    if (c->scopeDepth == 0) return;
    BtlToken* name = &p->previous;
    for (int i = c->localCount - 1; i >= 0; i--) {
        BtlLocal* local = &c->locals[i];
        if (local->depth != -1 && local->depth < c->scopeDepth) break;
        if (identifiersEqual(name, &local->name)) errorAt(p, name, "Already a variable with this name in this scope.");
    }
    addLocal(p, c, *name);
}

// --- Expression Parse Functions ---

static void func(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    function(p, s, c, cc, BTL_TYPE_FUNCTION);
}

static void binary(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    BtlTokenType opType = p->previous.type;
    ParseRule* rule = getRule(opType);
    int lhsOffset = c->lastInstruction;
    LastInstruction lhs = getInstructionAt(c, lhsOffset);
    parsePrecedence(p, s, c, cc, (Precedence) (rule->precedence + 1));
    int rhsOffset = c->lastInstruction;
    LastInstruction rhs = getInstructionAt(c, rhsOffset);

    if (lhs.isConstant && rhs.isConstant && (lhsOffset + lhs.length == rhsOffset)) {
        // Int + Int constant folding
        if (IS_INT(lhs.value) && IS_INT(rhs.value)) {
            int64_t a = AS_INT(lhs.value);
            int64_t b = AS_INT(rhs.value);
            int64_t res;
            bool folded = true;
            switch (opType) {
            case BTL_TOKEN_PLUS:    res = a + b; break;
            case BTL_TOKEN_MINUS:   res = a - b; break;
            case BTL_TOKEN_STAR:    res = a * b; break;
            case BTL_TOKEN_SLASH:
                if (b == 0) { errorAt(p, &p->previous, "Division by zero."); return; }
                res = a / b; break;
            case BTL_TOKEN_PERCENT:
                if (b == 0) { errorAt(p, &p->previous, "Division by zero."); return; }
                res = a % b; break;
            default: folded = false; res = 0;
            }
            if (folded) {
                currentChunk(c)->count = lhsOffset;
                c->lastInstruction = c->previousInstruction;
                emitConstant(p, c, INT_VAL(res));
                return;
            }
        }
        // Double + Double or mixed numeric constant folding
        if ((IS_NUMBER(lhs.value) || IS_INT(lhs.value)) &&
            (IS_NUMBER(rhs.value) || IS_INT(rhs.value)) &&
            (IS_NUMBER(lhs.value) || IS_NUMBER(rhs.value))) {
            double a = IS_INT(lhs.value) ? (double)AS_INT(lhs.value) : AS_NUMBER(lhs.value);
            double b = IS_INT(rhs.value) ? (double)AS_INT(rhs.value) : AS_NUMBER(rhs.value);
            double res;
            bool folded = true;
            switch (opType) {
            case BTL_TOKEN_PLUS:    res = a + b; break;
            case BTL_TOKEN_MINUS:   res = a - b; break;
            case BTL_TOKEN_STAR:    res = a * b; break;
            case BTL_TOKEN_SLASH:
                if (b == 0) {
                    errorAt(p, &p->previous, "Division by zero.");
                    return;
                }
                res = a / b;
                break;
            case BTL_TOKEN_PERCENT: res = fmod(a, b); break;
            default: folded = false; res = 0;
            }
            if (folded) {
                currentChunk(c)->count = lhsOffset;
                c->lastInstruction = c->previousInstruction;
                emitConstant(p, c, NUMBER_VAL(res));
                return;
            }
        }
    }

    switch (opType) {
    case BTL_TOKEN_BANG_EQUAL:    emitBytes(p, c, BTL_OP_EQUAL, BTL_OP_NOT); break;
    case BTL_TOKEN_EQUAL_EQUAL:   emitByte(p, c, BTL_OP_EQUAL); break;
    case BTL_TOKEN_GREATER:       emitByte(p, c, BTL_OP_GREATER); break;
    case BTL_TOKEN_GREATER_EQUAL: emitBytes(p, c, BTL_OP_LESS, BTL_OP_NOT); break;
    case BTL_TOKEN_LESS:          emitByte(p, c, BTL_OP_LESS); break;
    case BTL_TOKEN_LESS_EQUAL:    emitBytes(p, c, BTL_OP_GREATER, BTL_OP_NOT); break;
    case BTL_TOKEN_PLUS:          emitByte(p, c, BTL_OP_ADD); break;
    case BTL_TOKEN_MINUS:         emitByte(p, c, BTL_OP_SUBTRACT); break;
    case BTL_TOKEN_STAR:          emitByte(p, c, BTL_OP_MULTIPLY); break;
    case BTL_TOKEN_SLASH:         emitByte(p, c, BTL_OP_DIVIDE); break;
    case BTL_TOKEN_PERCENT:       emitByte(p, c, BTL_OP_MODULO); break;
    default: return;
    }
}

static void literal(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    switch (p->previous.type) {
    case BTL_TOKEN_FALSE: emitByte(p, c, BTL_OP_FALSE); break;
    case BTL_TOKEN_NULL:   emitByte(p, c, BTL_OP_NULL); break;
    case BTL_TOKEN_TRUE:  emitByte(p, c, BTL_OP_TRUE); break;
    default: return;
    }
}

static void grouping(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    expression(p, s, c, cc);
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;

    // Detect hex (0x...) or binary (0b...) prefix — always int
    if (p->previous.length > 2 && p->previous.start[0] == '0') {
        char prefix = p->previous.start[1];
        if (prefix == 'x' || prefix == 'X') {
            int64_t value = strtoll(p->previous.start + 2, NULL, 16);
            emitConstant(p, c, INT_VAL(value));
            return;
        }
        if (prefix == 'b' || prefix == 'B') {
            int64_t value = strtoll(p->previous.start + 2, NULL, 2);
            emitConstant(p, c, INT_VAL(value));
            return;
        }
    }

    // Check if the literal contains a decimal point to distinguish int from float
    bool isFloat = false;
    for (int i = 0; i < p->previous.length; i++) {
        if (p->previous.start[i] == '.') { isFloat = true; break; }
    }

    if (isFloat) {
        double value = strtod(p->previous.start, NULL);
        if (value == 0.0) {
            emitByte(p, c, BTL_OP_0);
        } else if (value == 1.0) {
            emitByte(p, c, BTL_OP_1);
        } else if (value == 2.0) {
            emitByte(p, c, BTL_OP_2);
        } else {
            emitConstant(p, c, NUMBER_VAL(value));
        }
    } else {
        int64_t value = strtoll(p->previous.start, NULL, 10);
        if (value == 0) {
            emitByte(p, c, BTL_OP_INT_0);
        } else if (value == 1) {
            emitByte(p, c, BTL_OP_INT_1);
        } else if (value == 2) {
            emitByte(p, c, BTL_OP_INT_2);
        } else {
            emitConstant(p, c, INT_VAL(value));
        }
    }
}

static void string(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    const char* src = p->previous.start + 1;
    int srcLen = p->previous.length - 2;

    // Strip \r from \r\n sequences (cross-platform CRLF handling)
    // Check if any \r exists first to avoid allocation in the common case
    bool hasCR = false;
    for (int i = 0; i < srcLen; i++) {
        if (src[i] == '\r') { hasCR = true; break; }
    }
    if (!hasCR) {
        emitConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, src, srcLen)));
        return;
    }

    // Build a cleaned copy with \r removed before \n
    char* buf = BTL_ALLOCATE(c->vm, char, srcLen);
    int dst = 0;
    for (int i = 0; i < srcLen; i++) {
        if (src[i] == '\r' && i + 1 < srcLen && src[i + 1] == '\n') continue;
        buf[dst++] = src[i];
    }
    emitConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, buf, dst)));
    BTL_FREE_ARRAY(c->vm, char, buf, srcLen);
}

static void variable(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    BtlToken name = p->previous;

    // Check for postfix ++ or -- BEFORE calling namedVariable
    if (canAssign && (check(p, BTL_TOKEN_PLUS_PLUS) || check(p, BTL_TOKEN_MINUS_MINUS))) {
        bool isInc = check(p, BTL_TOKEN_PLUS_PLUS);
        advance(p, s);  // consume ++ or --

        // Resolve variable location once
        int arg = resolveLocal(p, c, &name);
        if (arg != -1) {
            // Local variable
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (BTL_OP_GET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) arg);
            }
            emitByte(p, c, BTL_OP_DUP);
            emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
            markLocalAsModified(c, arg);
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (BTL_OP_SET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) arg);
            }
            emitByte(p, c, BTL_OP_POP);
            return;
        }

        arg = resolveUpvalue(p, c, &name);
        if (arg != -1) {
            // Upvalue
            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }
            emitUpvalue(p, c, (uint8_t) arg, false);
            emitByte(p, c, BTL_OP_DUP);
            emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
            emitUpvalue(p, c, (uint8_t) arg, true);
            emitByte(p, c, BTL_OP_POP);
            return;
        }

        if (cc != NULL) {
            // Class field
            ObjString* fieldName = btl_string_copy(c->vm, name.start, name.length);
            BtlValue indexVal;
            if (btl_table_get(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
                uint8_t index = (uint8_t) AS_NUMBER(indexVal);
                emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
                emitByte(p, c, BTL_OP_DUP);
                emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
                emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
                emitByte(p, c, BTL_OP_POP);
                return;
            }
        }

        // Global variable - get index once and reuse
        arg = identifierConstant(c, &name);
        emitLong(p, c, BTL_OP_GET_GLOBAL, BTL_OP_GET_GLOBAL_LONG, arg);
        emitByte(p, c, BTL_OP_DUP);
        emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
        emitLong(p, c, BTL_OP_SET_GLOBAL, BTL_OP_SET_GLOBAL_LONG, arg);
        emitByte(p, c, BTL_OP_POP);
        return;
    }

    namedVariable(p, s, c, cc, name, canAssign);
}

static void prefixIncDec(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    BtlTokenType op = p->previous.type;
    bool isInc = (op == BTL_TOKEN_PLUS_PLUS);

    // Check if this is a property access (++this.field or ++obj.prop)
    if (check(p, BTL_TOKEN_THIS) || check(p, BTL_TOKEN_IDENTIFIER)) {
        BtlToken possibleObj = p->current;
        advance(p, s);

        if (match(p, s, BTL_TOKEN_DOT)) {
            // It's a property access: ++this.field or ++obj.prop
            consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect property name after '.'.");
            BtlToken propName = p->previous;

            // Check if it's 'this'
            bool isThis = (possibleObj.length == 4 && memcmp(possibleObj.start, "this", 4) == 0);

            if (isThis && cc != NULL) {
                // ++this.field
                ObjString* fieldName = btl_string_copy(c->vm, propName.start, propName.length);
                BtlValue indexVal;
                if (!btl_table_get(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
                    indexVal = NUMBER_VAL((double) cc->fieldCount);
                    btl_table_set(c->vm, &cc->fields, OBJ_VAL(fieldName), indexVal);
                    cc->fieldCount++;
                }
                uint8_t index = (uint8_t) AS_NUMBER(indexVal);

                // Get, increment, dup, set, pop (leaves new value on stack)
                emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
                emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
                emitByte(p, c, BTL_OP_DUP);
                emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
                emitByte(p, c, BTL_OP_POP);
            } else {
                // ++obj.prop
                namedVariable(p, s, c, cc, possibleObj, false);
                int nameIdx = makeConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, propName.start, propName.length)));
                emitGetPropertyIC(p, c, nameIdx);
                emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
                emitByte(p, c, BTL_OP_DUP);
                emitSetPropertyIC(p, c, nameIdx);
                emitByte(p, c, BTL_OP_POP);
            }
            return;
        } else {
            // Not a property access, it's a simple variable
            // possibleObj is the variable name
            BtlToken name = possibleObj;

            // Special optimization for local variables
            int arg = resolveLocal(p, c, &name);
            if (arg != -1 && isInc) {
                // Use optimized BTL_OP_INC_LOCAL
                markLocalAsModified(c, arg);
                emitBytes(p, c, BTL_OP_INC_LOCAL, (uint8_t) arg);
                return;
            }

            // General case: get, modify, dup, set, pop
            namedVariable(p, s, c, cc, name, false);
            emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
            emitByte(p, c, BTL_OP_DUP);
            emitVariableSet(p, c, cc, name);
            emitByte(p, c, BTL_OP_POP);
            return;
        }
    }

    errorAt(p, &p->previous, "Expect variable or property after '++' or '--'.");
}

static void list(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;

    // Check for empty dictionary [:] 
    if (check(p, BTL_TOKEN_COLON)) {
        advance(p, s);
        consume(p, s, BTL_TOKEN_RIGHT_BRACKET, "Expect ']' after '[:'.");
        emitBytes(p, c, BTL_OP_BUILD_TABLE, 0);
        return;
    }

    int count = 0;
    if (!check(p, BTL_TOKEN_RIGHT_BRACKET)) {
        do {
            expression(p, s, c, cc);

            // After first expression, check for colon (dictionary)
            if (count == 0 && check(p, BTL_TOKEN_COLON)) {
                // This is a dictionary, not a list
                // The first key is already on the stack
                consume(p, s, BTL_TOKEN_COLON, "Expect ':'.");
                expression(p, s, c, cc);
                count = 1;

                // Continue parsing remaining key:value pairs
                while (match(p, s, BTL_TOKEN_COMMA)) {
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);
                    consume(p, s, BTL_TOKEN_COLON, "Expect ':' after dictionary key.");
                    expression(p, s, c, cc);

                    if (count == 255) errorAt(p, &p->previous, "Dictionary too large.");
                    count++;
                }

                consume(p, s, BTL_TOKEN_RIGHT_BRACKET, "Expect ']'.");
                emitBytes(p, c, BTL_OP_BUILD_TABLE, (uint8_t) count);
                return;
            }

            if (count == 255) errorAt(p, &p->previous, "List too large.");
            count++;
        } while (match(p, s, BTL_TOKEN_COMMA));
    }

    consume(p, s, BTL_TOKEN_RIGHT_BRACKET, "Expect ']'.");
    emitBytes(p, c, BTL_OP_BUILD_LIST, (uint8_t) count);
}

static void subscript(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    expression(p, s, c, cc);
    consume(p, s, BTL_TOKEN_RIGHT_BRACKET, "Expect ']'.");
    if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitByte(p, c, BTL_OP_INDEX_SET);
    } else {
        emitByte(p, c, BTL_OP_INDEX_GET);
    }
}

static void dot(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect property name after '.'.");
    BtlToken name = p->previous;
    ObjString* fieldName = btl_string_copy(c->vm, name.start, name.length);

    bool isThis = false;
    if (cc != NULL && c->lastInstruction != -1) {
        uint8_t lastOp = currentChunk(c)->code[c->lastInstruction];
        if (lastOp == BTL_OP_GET_LOCAL_0) isThis = true;
    }

    // Specialized 'this' field access
    if (isThis && !check(p, BTL_TOKEN_LEFT_PAREN)) {
        BtlValue indexVal;
        if (!btl_table_get(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            indexVal = NUMBER_VAL((double) cc->fieldCount);
            btl_table_set(c->vm, &cc->fields, OBJ_VAL(fieldName), indexVal);
            cc->fieldCount++;
        }
        uint8_t index = (uint8_t) AS_NUMBER(indexVal);
        removeChunkTail(currentChunk(c), 1);

        if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
            expression(p, s, c, cc);
            emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
        } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
            match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
            match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {
            BtlTokenType assignOp = p->previous.type;
            emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
            expression(p, s, c, cc);
            switch (assignOp) {
            case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
            case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
            case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
            case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
            case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
            default: break;
            }
            emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
        } else if (match(p, s, BTL_TOKEN_PLUS_PLUS) || match(p, s, BTL_TOKEN_MINUS_MINUS)) {
            BtlTokenType op = p->previous.type;
            bool isInc = (op == BTL_TOKEN_PLUS_PLUS);
            emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
            emitByte(p, c, BTL_OP_DUP);
            emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
            emitBytes(p, c, BTL_OP_SET_FIELD_THIS, index);
            emitByte(p, c, BTL_OP_POP);
        } else {
            emitBytes(p, c, BTL_OP_GET_FIELD_THIS, index);
        }
        return;
    }

    // Method call
    if (match(p, s, BTL_TOKEN_LEFT_PAREN)) {
        if (isThis && cc != NULL) {
            uint8_t args = 0;
            if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
                do {
                    expression(p, s, c, cc);
                    args++;
                } while (match(p, s, BTL_TOKEN_COMMA));
            }
            consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");

            int methodIndex = tryResolveMethodIndex(c, cc, &name, args);

            if (methodIndex >= 0) {
                emitInvokeIndexed(p, c, methodIndex, args);
                return;
            }

            int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));
            emitInvokeIC(p, c, nameIdx, args);
            return;
        } else {
            int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));

            uint8_t args = 0;
            if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
                do {
                    expression(p, s, c, cc);
                    args++;
                } while (match(p, s, BTL_TOKEN_COMMA));
            }
            consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");

            emitInvokeIC(p, c, nameIdx, args);
        }
    } else {
        int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));

        if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
            expression(p, s, c, cc);
            emitSetPropertyIC(p, c, nameIdx);
        } else if (canAssign && (match(p, s, BTL_TOKEN_PLUS_EQUAL) || match(p, s, BTL_TOKEN_MINUS_EQUAL) ||
            match(p, s, BTL_TOKEN_STAR_EQUAL) || match(p, s, BTL_TOKEN_SLASH_EQUAL) ||
            match(p, s, BTL_TOKEN_PERCENT_EQUAL))) {
            BtlTokenType assignOp = p->previous.type;
            emitGetPropertyIC(p, c, nameIdx);
            expression(p, s, c, cc);
            switch (assignOp) {
            case BTL_TOKEN_PLUS_EQUAL:    emitByte(p, c, BTL_OP_ADD); break;
            case BTL_TOKEN_MINUS_EQUAL:   emitByte(p, c, BTL_OP_SUBTRACT); break;
            case BTL_TOKEN_STAR_EQUAL:    emitByte(p, c, BTL_OP_MULTIPLY); break;
            case BTL_TOKEN_SLASH_EQUAL:   emitByte(p, c, BTL_OP_DIVIDE); break;
            case BTL_TOKEN_PERCENT_EQUAL: emitByte(p, c, BTL_OP_MODULO); break;
            default: break;
            }
            emitSetPropertyIC(p, c, nameIdx);
        } else if (match(p, s, BTL_TOKEN_PLUS_PLUS) || match(p, s, BTL_TOKEN_MINUS_MINUS)) {
            BtlTokenType op = p->previous.type;
            bool isInc = (op == BTL_TOKEN_PLUS_PLUS);
            emitGetPropertyIC(p, c, nameIdx);
            emitByte(p, c, BTL_OP_DUP);
            emitByte(p, c, isInc ? BTL_OP_INCREMENT : BTL_OP_DECREMENT);
            emitSetPropertyIC(p, c, nameIdx);
        } else {
            emitGetPropertyIC(p, c, nameIdx);
        }
    }
}

static void unary(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    BtlTokenType opType = p->previous.type;
    parsePrecedence(p, s, c, cc, PREC_UNARY);
    int operandOffset = c->lastInstruction;
    LastInstruction operand = getInstructionAt(c, operandOffset);
    if (operand.isConstant) {
        if (opType == BTL_TOKEN_MINUS && IS_INT(operand.value)) {
            currentChunk(c)->count = operandOffset;
            c->lastInstruction = c->previousInstruction;
            emitConstant(p, c, INT_VAL(-AS_INT(operand.value)));
            return;
        }
        if (opType == BTL_TOKEN_MINUS && IS_NUMBER(operand.value)) {
            currentChunk(c)->count = operandOffset;
            c->lastInstruction = c->previousInstruction;
            emitConstant(p, c, NUMBER_VAL(-AS_NUMBER(operand.value)));
            return;
        }
        if (opType == BTL_TOKEN_BANG) {
            currentChunk(c)->count = operandOffset;
            c->lastInstruction = c->previousInstruction;
            bool valFalsey = IS_NULL(operand.value) || (IS_BOOL(operand.value) && !AS_BOOL(operand.value))
                || (IS_INT(operand.value) && AS_INT(operand.value) == 0)
                || (IS_NUMBER(operand.value) && AS_NUMBER(operand.value) == 0.0);
            emitByte(p, c, valFalsey ? BTL_OP_TRUE : BTL_OP_FALSE);
            return;
        }
    }
    switch (opType) {
    case BTL_TOKEN_BANG:  emitByte(p, c, BTL_OP_NOT); break;
    case BTL_TOKEN_MINUS: emitByte(p, c, BTL_OP_NEGATE); break;
    default: return;
    }
}

static void and_(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int endJump = emitJump(p, c, BTL_OP_JUMP_IF_FALSE);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_AND);
    patchJump(p, c, endJump);
    // The short-circuit jump now targets chunk->count.  If emitFusedJump
    // later removes the last comparison opcode, that target becomes invalid.
    // Inhibit fusion so the jump target stays on a valid instruction boundary.
    c->inhibitFusion = true;
}

static void or_(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int elseJump = emitJump(p, c, BTL_OP_JUMP_IF_TRUE);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, elseJump);
    // Same reasoning as and_() above.
    c->inhibitFusion = true;
}

static void call(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    uint8_t args = 0;
    if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
        do {
            expression(p, s, c, cc);
            args++;
        } while (match(p, s, BTL_TOKEN_COMMA));
    }
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");
    switch (args) {
    case 0: emitByte(p, c, BTL_OP_CALL_0); break;
    case 1: emitByte(p, c, BTL_OP_CALL_1); break;
    case 2: emitByte(p, c, BTL_OP_CALL_2); break;
    case 3: emitByte(p, c, BTL_OP_CALL_3); break;
    case 4: emitByte(p, c, BTL_OP_CALL_4); break;
    case 5: emitByte(p, c, BTL_OP_CALL_5); break;
    case 6: emitByte(p, c, BTL_OP_CALL_6); break;
    case 7: emitByte(p, c, BTL_OP_CALL_7); break;
    case 8: emitByte(p, c, BTL_OP_CALL_8); break;
    default: emitBytes(p, c, BTL_OP_CALL, args); break;
    }
}

static void this_(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'this' outside of a class.");
        return;
    }
    variable(p, s, c, cc, false);
}

static void super_(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'super' outside of a class.");
    } else if (!cc->hasSuperclass) {
        errorAt(p, &p->previous, "Can't use 'super' in a class with no superclass.");
    }

    consume(p, s, BTL_TOKEN_DOT, "Expect '.'.");
    consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect superclass method name.");
    BtlToken name = p->previous;

    BtlToken thisT = { .start = "this", .length = 4 };
    BtlToken superT = { .start = "super", .length = 5 };

    if (match(p, s, BTL_TOKEN_LEFT_PAREN)) {
        // Push this (receiver) first
        namedVariable(p, s, c, cc, thisT, false);

        // Push args
        uint8_t args = 0;
        if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                args++;
            } while (match(p, s, BTL_TOKEN_COMMA));
        }
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");

        // Push super AFTER args so it's on top for the invoke to pop
        namedVariable(p, s, c, cc, superT, false);

        int methodIndex = tryResolveMethodIndex(c, cc, &name, args);

        if (methodIndex < 0) {
            errorAt(p, &name, "Undefined superclass method.");
            return;
        }

        emitSuperInvokeIndexed(p, c, methodIndex, args);
    } else {
        namedVariable(p, s, c, cc, thisT, false);
        namedVariable(p, s, c, cc, superT, false);
        int nameIdx = makeConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, name.start, name.length)));
        emitLong(p, c, BTL_OP_GET_SUPER, BTL_OP_GET_SUPER_LONG, nameIdx);
    }
}

static void switch_(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    switchStatement(p, s, c, cc, false);
}

static void doExpr(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool canAssign) {
    (void) canAssign;

    // do func() { ... } - anonymous async function
    if (match(p, s, BTL_TOKEN_FUNC)) {
        function(p, s, c, cc, BTL_TYPE_FUNCTION);
        emitBytes(p, c, BTL_OP_DO_NEW, 0);
        return;
    }

    if (!check(p, BTL_TOKEN_IDENTIFIER)) {
        errorAt(p, &p->current, "Expect identifier or 'func' after 'do'.");
        return;
    }

    advance(p, s);
    BtlToken name = p->previous;

    if (check(p, BTL_TOKEN_LEFT_PAREN)) {
        // do identifier() - Class or function call
        namedVariable(p, s, c, cc, name, false);

        consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");
        uint8_t argCount = 0;
        if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                argCount++;
            } while (match(p, s, BTL_TOKEN_COMMA));
        }
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");

        emitBytes(p, c, BTL_OP_DO_NEW, argCount);

    } else if (check(p, BTL_TOKEN_DOT)) {
        // do obj.method()
        namedVariable(p, s, c, cc, name, false);

        consume(p, s, BTL_TOKEN_DOT, "Expect '.'.");
        consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect method name.");
        BtlToken methodName = p->previous;

        int nameConstant = makeConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, methodName.start, methodName.length)));

        consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");
        uint8_t argCount = 0;
        if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                argCount++;
            } while (match(p, s, BTL_TOKEN_COMMA));
        }
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");

        emitByte(p, c, BTL_OP_DO_INVOKE);
        emitByte(p, c, (uint8_t) nameConstant);
        emitByte(p, c, argCount);

    } else {
        errorAt(p, &p->current, "Expect '(' or '.' after identifier in 'do'.");
    }
}

// --- Parse Rules Table ---

ParseRule rules [] = {
    [BTL_TOKEN_LEFT_PAREN] = {grouping, call,   PREC_CALL},
    [BTL_TOKEN_LEFT_BRACKET] = {list,     subscript, PREC_CALL},
    [BTL_TOKEN_DOT] = {NULL,     dot,    PREC_CALL},
    [BTL_TOKEN_MINUS] = {unary,    binary, PREC_TERM},
    [BTL_TOKEN_PLUS] = {NULL,     binary, PREC_TERM},
    [BTL_TOKEN_STAR] = {NULL,     binary, PREC_FACTOR},
    [BTL_TOKEN_SLASH] = {NULL,     binary, PREC_FACTOR},
    [BTL_TOKEN_PERCENT] = {NULL,     binary, PREC_FACTOR},
    [BTL_TOKEN_BANG] = {unary,    NULL,   PREC_NONE},
    [BTL_TOKEN_BANG_EQUAL] = {NULL,     binary, PREC_EQUALITY},
    [BTL_TOKEN_EQUAL_EQUAL] = {NULL,     binary, PREC_EQUALITY},
    [BTL_TOKEN_GREATER] = {NULL,     binary, PREC_COMPARISON},
    [BTL_TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [BTL_TOKEN_LESS] = {NULL,     binary, PREC_COMPARISON},
    [BTL_TOKEN_LESS_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [BTL_TOKEN_IDENTIFIER] = {variable, NULL,   PREC_NONE},
    [BTL_TOKEN_STRING] = {string,   NULL,   PREC_NONE},
    [BTL_TOKEN_NUMBER] = {number,   NULL,   PREC_NONE},
    [BTL_TOKEN_AND] = {NULL,     and_,   PREC_AND},
    [BTL_TOKEN_OR] = {NULL,     or_,    PREC_OR},
    [BTL_TOKEN_FALSE] = {literal,  NULL,   PREC_NONE},
    [BTL_TOKEN_NULL] = {literal,  NULL,   PREC_NONE},
    [BTL_TOKEN_TRUE] = {literal,  NULL,   PREC_NONE},
    [BTL_TOKEN_SUPER] = {super_,   NULL,   PREC_NONE},
    [BTL_TOKEN_THIS] = {this_,    NULL,   PREC_NONE},
    [BTL_TOKEN_FUNC] = {func,     NULL,   PREC_NONE},
    [BTL_TOKEN_SWITCH] = {switch_,  NULL,   PREC_NONE},
    [BTL_TOKEN_COLON] = {NULL,     NULL,   PREC_NONE},
    [BTL_TOKEN_PLUS_PLUS] = {prefixIncDec, NULL, PREC_NONE},
    [BTL_TOKEN_MINUS_MINUS] = {prefixIncDec, NULL, PREC_NONE},
    [BTL_TOKEN_DO] = {doExpr, NULL, PREC_NONE},
    [BTL_TOKEN_EOF] = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, Precedence prec) {
    advance(p, s);

    ParseFn prefix = getRule(p->previous.type)->prefix;
    if (!prefix) {
        errorAt(p, &p->previous, "Expect expression.");
        return;
    }
    bool canAssign = prec <= PREC_ASSIGNMENT;
    prefix(p, s, c, cc, canAssign);

    while (prec <= getRule(p->current.type)->precedence) {
        advance(p, s);
        ParseFn infix = getRule(p->previous.type)->infix;
        infix(p, s, c, cc, canAssign);
    }
    if (canAssign && match(p, s, BTL_TOKEN_EQUAL)) {
        errorAt(p, &p->previous, "Invalid assignment target.");
    }
}

static ParseRule* getRule(BtlTokenType type) {
    return &rules[type];
}

static void expression(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    parsePrecedence(p, s, c, cc, PREC_ASSIGNMENT);
}

// --- Statement Functions ---

static void block(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    while (!check(p, BTL_TOKEN_RIGHT_BRACE) && !check(p, BTL_TOKEN_EOF)) {
        declaration(p, s, c, cc);
    }
    consume(p, s, BTL_TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, BtlFunctionType type) {
    BtlCompiler sub;
    initCompiler(p, &sub, c, type, c->module);
    beginScope(&sub);
    consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");
    if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++;
            int constant = parseVariable(p, s, &sub, "Expect parameter name.");
            defineVariable(p, &sub, constant);
        } while (match(p, s, BTL_TOKEN_COMMA));
    }
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");
    consume(p, s, BTL_TOKEN_LEFT_BRACE, "Expect '{'.");
    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);
    btl_push(c->vm, OBJ_VAL(f));
    int index = makeConstant(p, c, OBJ_VAL(f));
    emitLong(p, c, BTL_OP_CLOSURE, BTL_OP_CLOSURE_LONG, index);
    btl_pop(c->vm);

    for (int i = 0; i < f->upvalueCount; i++) {
        btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);

        bool isMut = sub.upvalues[i].isMutable;
        if (sub.upvalues[i].isLocal) {
            if (c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            } else if (!isMut) {
                addPatch(c, sub.upvalues[i].index, currentChunk(c)->count);
            }
        }
        btl_chunk_write(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
    }
}

static void method(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect method name.");
    BtlToken nameToken = p->previous;
    BtlFunctionType type = BTL_TYPE_METHOD;
    bool isInit = false;

    if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) {
        type = BTL_TYPE_INITIALIZER;
        isInit = true;
    }

    BtlCompiler sub;
    initCompiler(p, &sub, c, type, c->module);
    beginScope(&sub);

    consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");
    if (!check(p, BTL_TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++;
            int constant = parseVariable(p, s, &sub, "Expect parameter name.");
            defineVariable(p, &sub, constant);
        } while (match(p, s, BTL_TOKEN_COMMA));
    }
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");
    consume(p, s, BTL_TOKEN_LEFT_BRACE, "Expect '{'.");

    // INJECT FIELD INITIALIZERS at the start of init()
    if (isInit && cc != NULL) {
        for (int i = 0; i < cc->fieldCount; i++) {
            if (cc->fieldInfos[i].hasInit) {
                BtlScanner initScanner;
                initScanner.start = cc->fieldInfos[i].initSource;
                initScanner.current = cc->fieldInfos[i].initSource;
                initScanner.line = 1;

                Parser initParser = *p;
                initParser.hadError = false;
                initParser.panicMode = false;

                advance(&initParser, &initScanner);
                expression(&initParser, &initScanner, &sub, cc);

                emitBytes(p, &sub, BTL_OP_SET_FIELD_THIS, (uint8_t) i);
                emitPopOrRemoveLoad(p, &sub);

                if (initParser.hadError) {
                    errorAt(p, &nameToken, "Error in field initializer.");
                }
            }
        }
    }

    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);

    btl_push(c->vm, OBJ_VAL(f));
    int fnIdx = makeConstant(p, c, OBJ_VAL(f));
    emitLong(p, c, BTL_OP_CLOSURE, BTL_OP_CLOSURE_LONG, fnIdx);
    btl_pop(c->vm);

    for (int i = 0; i < f->upvalueCount; i++) {
        btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);
        bool isMut = sub.upvalues[i].isMutable;
        if (sub.upvalues[i].isLocal) {
            if (c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            } else if (!isMut) {
                addPatch(c, sub.upvalues[i].index, currentChunk(c)->count);
            }
        }
        btl_chunk_write(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
    }

    ObjString* signature = createMethodSignature(c, &nameToken, f->arity);
    btl_push(c->vm, OBJ_VAL(signature));

    BtlValue indexValue;
    int methodIndex;
    if (btl_table_get(&cc->methodIndices, OBJ_VAL(signature), &indexValue)) {
        methodIndex = (int) AS_NUMBER(indexValue);
    } else {
        methodIndex = cc->nextMethodIndex++;
        btl_table_set(c->vm, &cc->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    }

    btl_pop(c->vm);

    if (methodIndex < 256) {
        emitByte(p, c, BTL_OP_METHOD);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
    } else {
        emitByte(p, c, BTL_OP_METHOD_LONG);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        btl_chunk_write(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
    }
}

static void classDeclaration(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect class name.");
    BtlToken nameToken = p->previous;
    int nameStringConst = makeConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, nameToken.start, nameToken.length)));
    int nameGlobalIdx = identifierConstant(c, &nameToken);

    declareVariable(p, c);
    emitLong(p, c, BTL_OP_CLASS, BTL_OP_CLASS_LONG, nameStringConst);
    defineVariable(p, c, nameGlobalIdx);

    BtlClassCompiler classC;
    classC.enclosing = cc;
    classC.hasSuperclass = false;
    classC.fieldCount = 0;
    btl_table_init(&classC.fields);
    btl_table_init(&classC.methodIndices);
    classC.nextMethodIndex = 0;

    classC.fieldInfoCapacity = 8;
    classC.fieldInfos = BTL_ALLOCATE(c->vm, BtlFieldInfo, classC.fieldInfoCapacity);
    memset(classC.fieldInfos, 0, sizeof(BtlFieldInfo) * classC.fieldInfoCapacity);

    bool userDefinedInit = false;
    bool hasFieldInitializers = false;

    if (match(p, s, BTL_TOKEN_LESS)) {
        consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect superclass name.");
        BtlToken superClassName = p->previous;

        ObjString* parentName = btl_string_copy(c->vm, superClassName.start, superClassName.length);
        btl_push(c->vm, OBJ_VAL(parentName));

        BtlValue savedInfoValue;
        if (btl_table_get(&c->module->classInfo, OBJ_VAL(parentName), &savedInfoValue)) {
            BtlSavedClassInfo* parentInfo = (BtlSavedClassInfo*) (uintptr_t) AS_NUMBER(savedInfoValue);
            btl_table_add_all(c->vm, &parentInfo->methodIndices, &classC.methodIndices);

            int maxParentIndex = -1;
            for (int i = 0; i < parentInfo->methodIndices.capacity; i++) {
                BtlEntry* entry = &parentInfo->methodIndices.entries[i];
                if (IS_STRING(entry->key) && IS_NUMBER(entry->value)) {
                    int idx = (int) AS_NUMBER(entry->value);
                    if (idx > maxParentIndex) maxParentIndex = idx;
                }
            }
            classC.nextMethodIndex = maxParentIndex + 1;

            // Inherit parent field indices so child fields don't collide
            btl_table_add_all(c->vm, &parentInfo->fieldIndices, &classC.fields);
            classC.fieldCount = parentInfo->fieldCount;

            // Ensure fieldInfos capacity and mark inherited fields as no-init
            while (classC.fieldCount > classC.fieldInfoCapacity) {
                int oldCap = classC.fieldInfoCapacity;
                classC.fieldInfoCapacity *= 2;
                classC.fieldInfos = BTL_GROW_ARRAY(c->vm, BtlFieldInfo, classC.fieldInfos,
                    oldCap, classC.fieldInfoCapacity);
            }
            for (int i = 0; i < classC.fieldCount; i++) {
                classC.fieldInfos[i].hasInit = false;
            }
        }

        btl_pop(c->vm);

        variable(p, s, c, &classC, false);

        if (identifiersEqual(&nameToken, &superClassName)) {
            errorAt(p, &superClassName, "A class can't inherit from itself.");
        }

        beginScope(c);
        addLocal(p, c, syntheticToken("super"));
        markInitialized(c);

        namedVariable(p, s, c, NULL, superClassName, false);
        namedVariable(p, s, c, NULL, nameToken, false);

        emitByte(p, c, BTL_OP_INHERIT);
        classC.hasSuperclass = true;
    }

    namedVariable(p, s, c, NULL, nameToken, false);

    consume(p, s, BTL_TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(p, BTL_TOKEN_RIGHT_BRACE) && !check(p, BTL_TOKEN_EOF)) {
        if (match(p, s, BTL_TOKEN_VAR)) {
            do {
                consume(p, s, BTL_TOKEN_IDENTIFIER, "Expect variable name.");
                BtlToken fieldName = p->previous;

                ObjString* name = btl_string_copy(c->vm, fieldName.start, fieldName.length);
                BtlValue dummy;
                int fieldIndex;

                if (!btl_table_get(&classC.fields, OBJ_VAL(name), &dummy)) {
                    fieldIndex = classC.fieldCount++;
                    btl_table_set(c->vm, &classC.fields, OBJ_VAL(name), NUMBER_VAL((double) fieldIndex));

                    if (fieldIndex >= classC.fieldInfoCapacity) {
                        int oldCap = classC.fieldInfoCapacity;
                        classC.fieldInfoCapacity *= 2;
                        classC.fieldInfos = BTL_GROW_ARRAY(c->vm, BtlFieldInfo, classC.fieldInfos,
                            oldCap, classC.fieldInfoCapacity);
                        memset(classC.fieldInfos + oldCap, 0,
                            sizeof(BtlFieldInfo) * (classC.fieldInfoCapacity - oldCap));
                    }

                    classC.fieldInfos[fieldIndex].fieldName = name;
                    classC.fieldInfos[fieldIndex].fieldIndex = fieldIndex;
                    classC.fieldInfos[fieldIndex].hasInit = false;
                } else {
                    fieldIndex = (int) AS_NUMBER(dummy);
                }

                if (match(p, s, BTL_TOKEN_EQUAL)) {
                    hasFieldInitializers = true;

                    const char* exprStart = p->current.start;

                    int parenDepth = 0;
                    int braceDepth = 0;
                    int bracketDepth = 0;

                    while (!check(p, BTL_TOKEN_EOF)) {
                        if (check(p, BTL_TOKEN_LEFT_PAREN)) parenDepth++;
                        if (check(p, BTL_TOKEN_RIGHT_PAREN)) parenDepth--;
                        if (check(p, BTL_TOKEN_LEFT_BRACE)) braceDepth++;
                        if (check(p, BTL_TOKEN_RIGHT_BRACE)) braceDepth--;
                        if (check(p, BTL_TOKEN_LEFT_BRACKET)) bracketDepth++;
                        if (check(p, BTL_TOKEN_RIGHT_BRACKET)) bracketDepth--;

                        if (parenDepth == 0 && braceDepth == 0 && bracketDepth == 0) {
                            if (check(p, BTL_TOKEN_COMMA) || check(p, BTL_TOKEN_SEMICOLON)) {
                                break;
                            }
                        }

                        advance(p, s);
                    }

                    const char* exprEnd = p->previous.start + p->previous.length;
                    int exprLength = (int) (exprEnd - exprStart);

                    classC.fieldInfos[fieldIndex].initSource = exprStart;
                    classC.fieldInfos[fieldIndex].initLength = exprLength;
                    classC.fieldInfos[fieldIndex].hasInit = true;
                }
            } while (match(p, s, BTL_TOKEN_COMMA));

            consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
        } else if (match(p, s, BTL_TOKEN_FUNC)) {
            if (check(p, BTL_TOKEN_IDENTIFIER) &&
                p->current.length == 4 &&
                memcmp(p->current.start, "init", 4) == 0) {
                userDefinedInit = true;
            }

            method(p, s, c, &classC);
        } else {
            errorAt(p, &p->current, "Expect 'func' or 'var' in class body.");
            advance(p, s);
        }
    }
    consume(p, s, BTL_TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    // AUTO-GENERATE empty init() if field initializers exist but no user init
    if (hasFieldInitializers && !userDefinedInit) {
        namedVariable(p, s, c, NULL, nameToken, false);

        BtlToken initToken;
        initToken.start = "init";
        initToken.length = 4;
        initToken.line = nameToken.line;
        initToken.type = BTL_TOKEN_IDENTIFIER;

        BtlCompiler sub;
        initCompiler(p, &sub, c, BTL_TYPE_INITIALIZER, c->module);
        beginScope(&sub);

        sub.function->arity = 0;

        // Inject field initializers
        for (int i = 0; i < classC.fieldCount; i++) {
            if (classC.fieldInfos[i].hasInit) {
                BtlScanner initScanner;
                initScanner.start = classC.fieldInfos[i].initSource;
                initScanner.current = classC.fieldInfos[i].initSource;
                initScanner.line = 1;

                Parser initParser = *p;
                initParser.hadError = false;
                initParser.panicMode = false;

                advance(&initParser, &initScanner);
                expression(&initParser, &initScanner, &sub, &classC);

                emitBytes(p, &sub, BTL_OP_SET_FIELD_THIS, (uint8_t) i);
                emitPopOrRemoveLoad(p, &sub);

                if (initParser.hadError) {
                    p->hadError = true;
                }
            }
        }

        emitBytes(p, &sub, BTL_OP_GET_LOCAL, 0);
        emitByte(p, &sub, BTL_OP_RETURN);

        ObjFunction* f = sub.function;

        btl_push(c->vm, OBJ_VAL(f));
        int fnIdx = makeConstant(p, c, OBJ_VAL(f));
        emitLong(p, c, BTL_OP_CLOSURE, BTL_OP_CLOSURE_LONG, fnIdx);
        btl_pop(c->vm);

        for (int i = 0; i < f->upvalueCount; i++) {
            btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
            btl_chunk_write(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);
            bool isMut = sub.upvalues[i].isMutable;
            if (sub.upvalues[i].isLocal && c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            }
            btl_chunk_write(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
        }

        ObjString* signature = createMethodSignature(c, &initToken, 0);
        btl_push(c->vm, OBJ_VAL(signature));

        BtlValue indexValue;
        int methodIndex;
        if (btl_table_get(&classC.methodIndices, OBJ_VAL(signature), &indexValue)) {
            methodIndex = (int) AS_NUMBER(indexValue);
        } else {
            methodIndex = classC.nextMethodIndex++;
            btl_table_set(c->vm, &classC.methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
        }

        btl_pop(c->vm);

        if (methodIndex < 256) {
            emitByte(p, c, BTL_OP_METHOD);
            btl_chunk_write(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
            btl_chunk_write(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
        } else {
            emitByte(p, c, BTL_OP_METHOD_LONG);
            btl_chunk_write(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
            btl_chunk_write(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
            btl_chunk_write(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
        }

        emitByte(p, c, BTL_OP_POP);
    }

    // Sync Fields
    for (int i = 0; i < classC.fieldCount; i++) {
        ObjString* foundName = NULL;
        for (int j = 0; j < classC.fields.capacity; j++) {
            BtlEntry* entry = &classC.fields.entries[j];
            if (IS_STRING(entry->key) && (int) AS_NUMBER(entry->value) == i) {
                foundName = AS_STRING(entry->key);
                break;
            }
        }
        if (foundName != NULL) {
            int nameIdx = makeConstant(p, c, OBJ_VAL(foundName));
            emitLong(p, c, BTL_OP_FIELD, BTL_OP_FIELD, nameIdx);
        }
    }

    emitByte(p, c, BTL_OP_POP);

    if (classC.hasSuperclass) {
        endScope(p, c);
    }

    // Save method and field indices AFTER auto-generated init is added
    ObjString* className = btl_string_copy(c->vm, nameToken.start, nameToken.length);
    btl_push(c->vm, OBJ_VAL(className));

    BtlSavedClassInfo* savedInfo = BTL_ALLOCATE(c->vm, BtlSavedClassInfo, 1);
    btl_table_init(&savedInfo->methodIndices);
    btl_table_init(&savedInfo->fieldIndices);
    btl_table_add_all(c->vm, &classC.methodIndices, &savedInfo->methodIndices);
    btl_table_add_all(c->vm, &classC.fields, &savedInfo->fieldIndices);
    savedInfo->fieldCount = classC.fieldCount;

    btl_table_set(c->vm, &c->module->classInfo, OBJ_VAL(className),
        NUMBER_VAL((double) (uintptr_t) savedInfo));

    btl_pop(c->vm);
    btl_table_free(c->vm, &classC.fields);
    btl_table_free(c->vm, &classC.methodIndices);
    BTL_FREE_ARRAY(c->vm, BtlFieldInfo, classC.fieldInfos, classC.fieldInfoCapacity);
}

static void funDeclaration(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    int global = parseVariable(p, s, c, "Expect function name.");
    markInitialized(c);
    function(p, s, c, cc, BTL_TYPE_FUNCTION);
    defineVariable(p, c, global);
}

static void switchStatement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, bool isStatement) {
    consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    expression(p, s, c, cc);
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')' after switch expression.");
    consume(p, s, BTL_TOKEN_LEFT_BRACE, "Expect '{' before switch body.");

    BtlSwitchContext switchCtx;
    switchCtx.enclosing = c->currentSwitch;
    switchCtx.caseJumpCount = 0;
    switchCtx.caseJumpCapacity = 8;
    switchCtx.caseJumps = BTL_ALLOCATE(c->vm, int, switchCtx.caseJumpCapacity);
    switchCtx.breakCount = 0;
    switchCtx.breakCapacity = 8;
    switchCtx.breakJumps = BTL_ALLOCATE(c->vm, int, switchCtx.breakCapacity);
    switchCtx.scopeDepth = c->scopeDepth;
    switchCtx.hasDefault = false;
    switchCtx.isExpression = false;

    c->currentSwitch = &switchCtx;

    int* fallthroughJumps = BTL_ALLOCATE(c->vm, int, 16);
    int fallthroughCount = 0;
    int fallthroughCapacity = 16;
    bool lastCaseHadBreak = true;
    // Track where the previous case's jumps start in caseJumps so we can
    // patch ALL of them (not just the last) when the next case begins.
    // With 'and' conditions a single case may emit multiple jumps.
    int prevCaseJumpStart = 0;

    while (!check(p, BTL_TOKEN_RIGHT_BRACE) && !check(p, BTL_TOKEN_EOF)) {
        if (match(p, s, BTL_TOKEN_CASE)) {
            if (!lastCaseHadBreak) {
                int fallthroughJump = emitJump(p, c, BTL_OP_JUMP);
                if (fallthroughCount >= fallthroughCapacity) {
                    int oldCap = fallthroughCapacity;
                    fallthroughCapacity *= 2;
                    fallthroughJumps = BTL_GROW_ARRAY(c->vm, int, fallthroughJumps, oldCap, fallthroughCapacity);
                }
                fallthroughJumps[fallthroughCount++] = fallthroughJump;
            }

            if (switchCtx.caseJumpCount > prevCaseJumpStart) {
                for (int j = prevCaseJumpStart; j < switchCtx.caseJumpCount; j++) {
                    patchJump(p, c, switchCtx.caseJumps[j]);
                }
                prevCaseJumpStart = switchCtx.caseJumpCount;
                emitByte(p, c, BTL_OP_POP);
            }

            int* successJumps = BTL_ALLOCATE(c->vm, int, 8);
            int successCount = 0;
            int successCapacity = 8;

            int* failJumps = BTL_ALLOCATE(c->vm, int, 8);
            int failCount = 0;
            int failCapacity = 8;

            for (;;) {
                bool isBooleanCondition = (check(p, BTL_TOKEN_LESS) || check(p, BTL_TOKEN_LESS_EQUAL) ||
                    check(p, BTL_TOKEN_GREATER) || check(p, BTL_TOKEN_GREATER_EQUAL) ||
                    check(p, BTL_TOKEN_EQUAL_EQUAL) || check(p, BTL_TOKEN_BANG_EQUAL));

                if (isBooleanCondition) {
                    emitByte(p, c, BTL_OP_DUP);
                    BtlTokenType op = p->current.type;
                    advance(p, s);
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);

                    switch (op) {
                    case BTL_TOKEN_LESS: emitByte(p, c, BTL_OP_LESS); break;
                    case BTL_TOKEN_LESS_EQUAL: emitBytes(p, c, BTL_OP_GREATER, BTL_OP_NOT); break;
                    case BTL_TOKEN_GREATER: emitByte(p, c, BTL_OP_GREATER); break;
                    case BTL_TOKEN_GREATER_EQUAL: emitBytes(p, c, BTL_OP_LESS, BTL_OP_NOT); break;
                    case BTL_TOKEN_EQUAL_EQUAL: emitByte(p, c, BTL_OP_EQUAL); break;
                    case BTL_TOKEN_BANG_EQUAL: emitBytes(p, c, BTL_OP_EQUAL, BTL_OP_NOT); break;
                    default: break;
                    }
                } else {
                    emitByte(p, c, BTL_OP_DUP);
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);
                    emitByte(p, c, BTL_OP_EQUAL);
                }

                if (match(p, s, BTL_TOKEN_COMMA) || match(p, s, BTL_TOKEN_OR) || match(p, s, BTL_TOKEN_AND)) {
                    if (p->previous.type == BTL_TOKEN_AND) {
                        int andJump = emitJump(p, c, BTL_OP_JUMP_IF_FALSE);
                        emitByte(p, c, BTL_OP_POP);

                        if (failCount >= failCapacity) {
                            int oldCap = failCapacity;
                            failCapacity *= 2;
                            failJumps = BTL_GROW_ARRAY(c->vm, int, failJumps, oldCap, failCapacity);
                        }
                        failJumps[failCount++] = andJump;
                    } else {
                        int orJump = emitJump(p, c, BTL_OP_JUMP_IF_TRUE);
                        emitByte(p, c, BTL_OP_POP);

                        if (successCount >= successCapacity) {
                            int oldCap = successCapacity;
                            successCapacity *= 2;
                            successJumps = BTL_GROW_ARRAY(c->vm, int, successJumps, oldCap, successCapacity);
                        }
                        successJumps[successCount++] = orJump;
                    }
                } else {
                    break;
                }
            }

            consume(p, s, BTL_TOKEN_COLON, "Expect ':' after case condition.");

            int caseJump = emitJump(p, c, BTL_OP_JUMP_IF_FALSE);
            emitByte(p, c, BTL_OP_POP);

            for (int i = 0; i < successCount; i++) {
                patchJump(p, c, successJumps[i]);
            }
            if (successCount > 0) {
                emitByte(p, c, BTL_OP_POP);
            }

            BTL_FREE_ARRAY(c->vm, int, successJumps, successCapacity);

            if (switchCtx.caseJumpCount >= switchCtx.caseJumpCapacity) {
                int oldCap = switchCtx.caseJumpCapacity;
                switchCtx.caseJumpCapacity *= 2;
                switchCtx.caseJumps = BTL_GROW_ARRAY(c->vm, int, switchCtx.caseJumps, oldCap, switchCtx.caseJumpCapacity);
            }
            switchCtx.caseJumps[switchCtx.caseJumpCount++] = caseJump;

            for (int i = 0; i < failCount; i++) {
                if (switchCtx.caseJumpCount >= switchCtx.caseJumpCapacity) {
                    int oldCap = switchCtx.caseJumpCapacity;
                    switchCtx.caseJumpCapacity *= 2;
                    switchCtx.caseJumps = BTL_GROW_ARRAY(c->vm, int, switchCtx.caseJumps, oldCap, switchCtx.caseJumpCapacity);
                }
                switchCtx.caseJumps[switchCtx.caseJumpCount++] = failJumps[i];
            }

            BTL_FREE_ARRAY(c->vm, int, failJumps, failCapacity);

            for (int i = 0; i < fallthroughCount; i++) {
                patchJump(p, c, fallthroughJumps[i]);
            }
            fallthroughCount = 0;

            int startBreakCount = switchCtx.breakCount;

            while (!check(p, BTL_TOKEN_CASE) && !check(p, BTL_TOKEN_DEFAULT) &&
                !check(p, BTL_TOKEN_RIGHT_BRACE) && !check(p, BTL_TOKEN_EOF)) {
                statement(p, s, c, cc);
            }

            lastCaseHadBreak = (switchCtx.breakCount > startBreakCount);

        } else if (match(p, s, BTL_TOKEN_DEFAULT)) {
            if (switchCtx.hasDefault) {
                errorAt(p, &p->previous, "Switch can only have one default case.");
            }
            switchCtx.hasDefault = true;

            if (!lastCaseHadBreak) {
                int fallthroughJump = emitJump(p, c, BTL_OP_JUMP);
                if (fallthroughCount >= fallthroughCapacity) {
                    int oldCap = fallthroughCapacity;
                    fallthroughCapacity *= 2;
                    fallthroughJumps = BTL_GROW_ARRAY(c->vm, int, fallthroughJumps, oldCap, fallthroughCapacity);
                }
                fallthroughJumps[fallthroughCount++] = fallthroughJump;
            }

            if (switchCtx.caseJumpCount > prevCaseJumpStart) {
                for (int j = prevCaseJumpStart; j < switchCtx.caseJumpCount; j++) {
                    patchJump(p, c, switchCtx.caseJumps[j]);
                }
                prevCaseJumpStart = switchCtx.caseJumpCount;
                emitByte(p, c, BTL_OP_POP);
            }

            consume(p, s, BTL_TOKEN_COLON, "Expect ':' after 'default'.");

            for (int i = 0; i < fallthroughCount; i++) {
                patchJump(p, c, fallthroughJumps[i]);
            }
            fallthroughCount = 0;

            int startBreakCount = switchCtx.breakCount;

            while (!check(p, BTL_TOKEN_CASE) && !check(p, BTL_TOKEN_RIGHT_BRACE) &&
                !check(p, BTL_TOKEN_EOF)) {
                statement(p, s, c, cc);
            }

            lastCaseHadBreak = (switchCtx.breakCount > startBreakCount);

        } else {
            errorAt(p, &p->current, "Expect 'case' or 'default' in switch body.");
            advance(p, s);
        }
    }

    consume(p, s, BTL_TOKEN_RIGHT_BRACE, "Expect '}' after switch body.");

    if (switchCtx.caseJumpCount > prevCaseJumpStart && !switchCtx.hasDefault) {
        for (int j = prevCaseJumpStart; j < switchCtx.caseJumpCount; j++) {
            patchJump(p, c, switchCtx.caseJumps[j]);
        }
        prevCaseJumpStart = switchCtx.caseJumpCount;
        emitByte(p, c, BTL_OP_POP);
    }

    for (int i = 0; i < fallthroughCount; i++) {
        patchJump(p, c, fallthroughJumps[i]);
    }

    if (switchCtx.isExpression) {
        for (int i = 0; i < switchCtx.breakCount; i++) {
            patchJump(p, c, switchCtx.breakJumps[i]);
        }

        if (switchCtx.breakCount == 0) {
            emitByte(p, c, BTL_OP_NULL);
        }
        emitByte(p, c, BTL_OP_SWAP);
        emitByte(p, c, BTL_OP_POP);
    } else {
        emitByte(p, c, BTL_OP_POP);

        for (int i = 0; i < switchCtx.breakCount; i++) {
            patchJump(p, c, switchCtx.breakJumps[i]);
        }

        if (!isStatement) {
            emitByte(p, c, BTL_OP_NULL);
        }
    }

    BTL_FREE_ARRAY(c->vm, int, switchCtx.caseJumps, switchCtx.caseJumpCapacity);
    BTL_FREE_ARRAY(c->vm, int, switchCtx.breakJumps, switchCtx.breakCapacity);
    BTL_FREE_ARRAY(c->vm, int, fallthroughJumps, fallthroughCapacity);
    c->currentSwitch = switchCtx.enclosing;
}

// for-in statement: for (var x in collection) { ... }
static void forInStatement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc, int varSlot) {
    // At this point we've already parsed: for ( var <name> in
    // varSlot is the local slot for the loop variable

    // Compile the collection expression — pushes collection onto stack
    expression(p, s, c, cc);
    consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')' after for-in.");

    // Register the collection as an anonymous local so the compiler knows
    // this stack slot is occupied (prevents slot collisions with shadow vars)
    addLocal(p, c, syntheticToken(""));
    c->locals[c->localCount - 1].depth = c->scopeDepth;

    // Emit OP_ITER_INIT: validates collection is list/table, pushes index 0
    emitByte(p, c, BTL_OP_ITER_INIT);

    // Register the index as an anonymous local too
    addLocal(p, c, syntheticToken(""));
    c->locals[c->localCount - 1].depth = c->scopeDepth;

    // Stack: [..., loop_var(varSlot), collection(varSlot+1), index(varSlot+2)]

    // Loop start point
    int loopStart = currentChunk(c)->count;

    // Emit OP_ITER_NEXT: checks if more elements; if not, jumps to exit
    // Operands: [slot:8][offset:16] — slot is the loop variable to set
    emitByte(p, c, BTL_OP_ITER_NEXT);
    emitByte(p, c, (uint8_t) varSlot);
    int exitJumpOffset = currentChunk(c)->count;
    btl_chunk_write(c->vm, currentChunk(c), 0xff, p->previous.line);
    btl_chunk_write(c->vm, currentChunk(c), 0xff, p->previous.line);
    // If we get here (didn't jump), the loop variable is set and we continue

    // Set up the loop structure for break/continue
    BtlLoop loop = { .enclosing = c->currentLoop, .start = loopStart,
                     .scopeDepth = c->scopeDepth, .breakCount = 0 };
    c->currentLoop = &loop;

    // Compile the loop body with variable shadowing (same as C-style for)
    beginScope(c);
    int shadowVar = -1;
    if (varSlot != -1) {
        emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) varSlot);
        shadowVar = c->localCount;
        BtlLocal* shadow = &c->locals[c->localCount++];
        shadow->name = c->locals[varSlot].name;
        shadow->depth = c->scopeDepth;
        shadow->isCaptured = false;
        shadow->isModified = false;
    }
    statement(p, s, c, cc);
    if (varSlot != -1) {
        emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) shadowVar);
        emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) varSlot);
        emitPopOrRemoveLoad(p, c);
    }
    endScope(p, c);

    // Loop back to OP_ITER_NEXT
    emitLoop(p, c, loopStart);

    // Patch the exit jump
    patchJump(p, c, exitJumpOffset);

    // Patch break jumps
    for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
    c->currentLoop = loop.enclosing;

    // Pop iterator state: index and collection
    emitByte(p, c, BTL_OP_POP);  // index
    emitByte(p, c, BTL_OP_POP);  // collection
    // Remove the anonymous locals from the compiler
    c->localCount -= 2;
}

static void forStatement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    beginScope(c);
    consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");

    // Check for for-in pattern: for (var x in expr)
    if (match(p, s, BTL_TOKEN_VAR)) {
        int global = parseVariable(p, s, c, "Expect variable name.");
        if (match(p, s, BTL_TOKEN_IN)) {
            // for-in loop: for (var x in collection)
            emitByte(p, c, BTL_OP_NULL);  // initialize loop var to null
            defineVariable(p, c, global);
            int varSlot = c->localCount - 1;
            forInStatement(p, s, c, cc, varSlot);
            endScope(p, c);
            return;
        }
        // Not for-in, continue as C-style: for (var x = expr; ...)
        if (match(p, s, BTL_TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, BTL_OP_NULL);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");
        defineVariable(p, c, global);
    } else if (match(p, s, BTL_TOKEN_SEMICOLON)) {
        // for (; ...) - no initializer
    } else {
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");
        emitPopOrRemoveLoad(p, c);
    }

    // C-style for loop: condition
    int loopStart = currentChunk(c)->count;
    int exitJump = -1;
    if (!match(p, s, BTL_TOKEN_SEMICOLON)) {
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");
        exitJump = emitJump(p, c, BTL_OP_POP_JUMP_IF_FALSE);
    }

    // C-style for loop: increment
    if (!match(p, s, BTL_TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(p, c, BTL_OP_JUMP);
        int incrementStart = currentChunk(c)->count;
        expression(p, s, c, cc);
        emitPopOrRemoveLoad(p, c);
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");
        emitLoop(p, c, loopStart);
        loopStart = incrementStart;
        patchJump(p, c, bodyJump);
    }

    BtlLoop loop = { .enclosing = c->currentLoop, .start = loopStart, .scopeDepth = c->scopeDepth, .breakCount = 0 };
    c->currentLoop = &loop;
    int loopVar = -1;
    for (int i = c->localCount - 1; i >= 0; i--) {
        if (c->locals[i].depth != -1 && c->locals[i].depth == c->scopeDepth) {
            loopVar = i;
            break;
        }
    }
    beginScope(c);
    int shadowVar = -1;  // ADD THIS
    if (loopVar != -1) {
        emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) loopVar);
        shadowVar = c->localCount;  // ADD THIS - save index BEFORE incrementing
        BtlLocal* shadow = &c->locals[c->localCount++];
        shadow->name = c->locals[loopVar].name;
        shadow->depth = c->scopeDepth;
        shadow->isCaptured = false;
        shadow->isModified = false;
    }
    statement(p, s, c, cc);
    if (loopVar != -1) {
        emitBytes(p, c, BTL_OP_GET_LOCAL, (uint8_t) shadowVar);  // CHANGE THIS
        emitBytes(p, c, BTL_OP_SET_LOCAL, (uint8_t) loopVar);
        emitPopOrRemoveLoad(p, c);
    }
    endScope(p, c);
    emitLoop(p, c, loopStart);
    if (exitJump != -1) {
        patchJump(p, c, exitJump);
    }
    for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
    c->currentLoop = loop.enclosing;
    endScope(p, c);
}

static void returnStatement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    if (c->type == BTL_TYPE_SCRIPT) {
        errorAt(p, &p->previous, "Can't return from top-level code.");
        return;
    }
    if (match(p, s, BTL_TOKEN_SEMICOLON)) {
        emitByte(p, c, BTL_OP_NULL);
        emitByte(p, c, BTL_OP_RETURN);
    } else {
        if (c->type == BTL_TYPE_INITIALIZER) errorAt(p, &p->previous, "Can't return a value from an initializer.");
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");

        if (c->lastInstruction != -1) {
            BtlChunk* chunk = currentChunk(c);
            uint8_t* opcode = &chunk->code[c->lastInstruction];
            switch (*opcode) {
            case BTL_OP_CALL: *opcode = BTL_OP_TAIL_CALL; break;
            case BTL_OP_CALL_0: *opcode = BTL_OP_TAIL_CALL_0; break;
            case BTL_OP_CALL_1: *opcode = BTL_OP_TAIL_CALL_1; break;
            case BTL_OP_CALL_2: *opcode = BTL_OP_TAIL_CALL_2; break;
            case BTL_OP_CALL_3: *opcode = BTL_OP_TAIL_CALL_3; break;
            case BTL_OP_CALL_4: *opcode = BTL_OP_TAIL_CALL_4; break;
            case BTL_OP_CALL_5: *opcode = BTL_OP_TAIL_CALL_5; break;
            case BTL_OP_CALL_6: *opcode = BTL_OP_TAIL_CALL_6; break;
            case BTL_OP_CALL_7: *opcode = BTL_OP_TAIL_CALL_7; break;
            case BTL_OP_CALL_8: *opcode = BTL_OP_TAIL_CALL_8; break;
            case BTL_OP_INVOKE: *opcode = BTL_OP_TAIL_INVOKE; break;
            case BTL_OP_INVOKE_LONG: *opcode = BTL_OP_TAIL_INVOKE_LONG; break;
            case BTL_OP_INVOKE_0: *opcode = BTL_OP_TAIL_INVOKE_0; break;
            case BTL_OP_INVOKE_1: *opcode = BTL_OP_TAIL_INVOKE_1; break;
            case BTL_OP_INVOKE_2: *opcode = BTL_OP_TAIL_INVOKE_2; break;
            case BTL_OP_INVOKE_3: *opcode = BTL_OP_TAIL_INVOKE_3; break;
            case BTL_OP_INVOKE_4: *opcode = BTL_OP_TAIL_INVOKE_4; break;
            case BTL_OP_INVOKE_5: *opcode = BTL_OP_TAIL_INVOKE_5; break;
            case BTL_OP_INVOKE_6: *opcode = BTL_OP_TAIL_INVOKE_6; break;
            case BTL_OP_INVOKE_7: *opcode = BTL_OP_TAIL_INVOKE_7; break;
            case BTL_OP_INVOKE_8: *opcode = BTL_OP_TAIL_INVOKE_8; break;
            case BTL_OP_INVOKE_IC: *opcode = BTL_OP_TAIL_INVOKE_IC; break;  // NEW
            case BTL_OP_SUPER_INVOKE: *opcode = BTL_OP_TAIL_SUPER_INVOKE; break;
            case BTL_OP_SUPER_INVOKE_LONG: *opcode = BTL_OP_TAIL_SUPER_INVOKE_LONG; break;
            case BTL_OP_SUPER_INVOKE_0: *opcode = BTL_OP_TAIL_SUPER_INVOKE_0; break;
            case BTL_OP_SUPER_INVOKE_1: *opcode = BTL_OP_TAIL_SUPER_INVOKE_1; break;
            case BTL_OP_SUPER_INVOKE_2: *opcode = BTL_OP_TAIL_SUPER_INVOKE_2; break;
            case BTL_OP_SUPER_INVOKE_3: *opcode = BTL_OP_TAIL_SUPER_INVOKE_3; break;
            case BTL_OP_SUPER_INVOKE_4: *opcode = BTL_OP_TAIL_SUPER_INVOKE_4; break;
            case BTL_OP_SUPER_INVOKE_5: *opcode = BTL_OP_TAIL_SUPER_INVOKE_5; break;
            case BTL_OP_SUPER_INVOKE_6: *opcode = BTL_OP_TAIL_SUPER_INVOKE_6; break;
            case BTL_OP_SUPER_INVOKE_7: *opcode = BTL_OP_TAIL_SUPER_INVOKE_7; break;
            case BTL_OP_SUPER_INVOKE_8: *opcode = BTL_OP_TAIL_SUPER_INVOKE_8; break;
            default: break;
            }
        }
        emitByte(p, c, BTL_OP_RETURN);
    }
}

static void statement(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    if (match(p, s, BTL_TOKEN_SWITCH)) {
        switchStatement(p, s, c, cc, true);
    } else if (match(p, s, BTL_TOKEN_FOR)) {
        forStatement(p, s, c, cc);
    } else if (match(p, s, BTL_TOKEN_IF)) {
        consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
        int thenJump = emitFusedJump(p, c, BTL_OP_POP_JUMP_IF_FALSE);
        statement(p, s, c, cc);
        int elseJump = emitJump(p, c, BTL_OP_JUMP);
        patchJump(p, c, thenJump);
        if (match(p, s, BTL_TOKEN_ELSE)) statement(p, s, c, cc);
        patchJump(p, c, elseJump);
    } else if (match(p, s, BTL_TOKEN_RETURN)) {
        returnStatement(p, s, c, cc);
    } else if (match(p, s, BTL_TOKEN_WHILE)) {
        int start = currentChunk(c)->count;
        BtlLoop loop = { .enclosing = c->currentLoop, .start = start, .scopeDepth = c->scopeDepth, .breakCount = 0 };
        c->currentLoop = &loop;
        consume(p, s, BTL_TOKEN_LEFT_PAREN, "Expect '('.");
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_RIGHT_PAREN, "Expect ')'.");
        int exitJ = emitFusedJump(p, c, BTL_OP_POP_JUMP_IF_FALSE);
        statement(p, s, c, cc);
        emitLoop(p, c, start);
        patchJump(p, c, exitJ);
        for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
        c->currentLoop = loop.enclosing;
    } else if (match(p, s, BTL_TOKEN_BREAK)) {
        bool hasValue = !check(p, BTL_TOKEN_SEMICOLON);

        if (hasValue) {
            if (c->currentSwitch == NULL) {
                errorAt(p, &p->previous, "Can only use 'break <value>' in switch.");
                return;
            }
            c->currentSwitch->isExpression = true;
            expression(p, s, c, cc);
        }

        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';' after break.");

        if (c->currentLoop == NULL && c->currentSwitch == NULL) {
            errorAt(p, &p->previous, "Can't use 'break' outside of loop or switch.");
            return;
        }

        // Pop locals up to the target scope depth
        // BUT DON'T CHANGE c->localCount if we're in a switch with a value
        // because the switch cleanup will handle it
        int targetDepth = c->currentLoop ? c->currentLoop->scopeDepth
            : c->currentSwitch->scopeDepth;

        int popCount = 0;
        for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > targetDepth; i--) {
            if (c->locals[i].isCaptured) {
                if (popCount > 0) {
                    emitPopN(p, c, popCount);
                    popCount = 0;
                }
                emitByte(p, c, BTL_OP_CLOSE_UPVALUE);
            } else {
                if (!hasValue || c->currentLoop) {  // Only pop if not break with value in switch
                    popCount++;
                }
            }
        }

        if (popCount > 0) emitPopN(p, c, popCount);

        int jump = emitJump(p, c, BTL_OP_JUMP);

        if (c->currentLoop) {
            c->currentLoop->breakJumps[c->currentLoop->breakCount++] = jump;
        } else {
            if (c->currentSwitch->breakCount >= c->currentSwitch->breakCapacity) {
                int oldCap = c->currentSwitch->breakCapacity;
                c->currentSwitch->breakCapacity *= 2;
                c->currentSwitch->breakJumps = BTL_GROW_ARRAY(c->vm, int,
                    c->currentSwitch->breakJumps,
                    oldCap,
                    c->currentSwitch->breakCapacity);
            }
            c->currentSwitch->breakJumps[c->currentSwitch->breakCount++] = jump;
        }

        // DON'T decrement c->localCount here - the switch end will handle it!
    } else if (match(p, s, BTL_TOKEN_CONTINUE)) {
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';' after continue.");

        if (c->currentLoop == NULL) {
            errorAt(p, &p->previous, "Can't use 'continue' outside of a loop.");
            return;
        }

        // Pop locals up to the loop's scope depth
        int popCount = 0;
        for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > c->currentLoop->scopeDepth; i--) {
            if (c->locals[i].isCaptured) {
                if (popCount > 0) {
                    emitPopN(p, c, popCount);
                    popCount = 0;
                }
                emitByte(p, c, BTL_OP_CLOSE_UPVALUE);
            } else {
                popCount++;
            }
        }
        if (popCount > 0) emitPopN(p, c, popCount);

        // Jump back to loop start (condition/increment/iterator)
        emitLoop(p, c, c->currentLoop->start);
    } else if (match(p, s, BTL_TOKEN_LEFT_BRACE)) {
        beginScope(c);
        block(p, s, c, cc);
        endScope(p, c);
    } else {
        expression(p, s, c, cc);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");
        BtlChunk* chunk = currentChunk(c);
        if (chunk->count > 0 && c->lastInstruction >= 0) {
            uint8_t lastOp = chunk->code[c->lastInstruction];  // Use lastInstruction, not count-1
            if (lastOp == BTL_OP_POP) return;
            if (lastOp >= BTL_OP_SET_LOCAL_0 && lastOp <= BTL_OP_SET_LOCAL_7) {
                chunk->code[c->lastInstruction] = lastOp + (BTL_OP_SET_LOCAL_0_POP - BTL_OP_SET_LOCAL_0);
                return;
            }
            if (lastOp == BTL_OP_INC_LOCAL) {
                chunk->code[c->lastInstruction] = BTL_OP_INC_LOCAL_POP;
                return;
            }
        }
        emitPopOrRemoveLoad(p, c);
    }
}

static void defineVariable(Parser* p, BtlCompiler* c, int global) {
    if (c->scopeDepth > 0) {
        markInitialized(c);
        return;
    }
    emitLong(p, c, BTL_OP_DEFINE_GLOBAL, BTL_OP_DEFINE_GLOBAL_LONG, global);
}
static int parseVariable(Parser* p, BtlScanner* s, BtlCompiler* c, const char* msg) {
    consume(p, s, BTL_TOKEN_IDENTIFIER, msg);
    declareVariable(p, c);
    if (c->scopeDepth > 0) return 0;
    return identifierConstant(c, &p->previous);
}
static void declaration(Parser* p, BtlScanner* s, BtlCompiler* c, BtlClassCompiler* cc) {
    if (match(p, s, BTL_TOKEN_CLASS)) {
        classDeclaration(p, s, c, cc);
    } else if (match(p, s, BTL_TOKEN_FUNC)) {
        funDeclaration(p, s, c, cc);
    } else if (match(p, s, BTL_TOKEN_VAR)) {
        do {
            int global = parseVariable(p, s, c, "Expect name.");
            if (match(p, s, BTL_TOKEN_EQUAL)) {
                expression(p, s, c, cc);
            } else {
                emitByte(p, c, BTL_OP_NULL);
            }
            defineVariable(p, c, global);
        } while (match(p, s, BTL_TOKEN_COMMA));
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';'.");
    } else if (match(p, s, BTL_TOKEN_IMPORT)) {
        consume(p, s, BTL_TOKEN_STRING, "Expect filename.");
        BtlToken pathToken = p->previous;
        int file = makeConstant(p, c, OBJ_VAL(btl_string_copy(c->vm, pathToken.start + 1, pathToken.length - 2)));
        int alias;
        if (match(p, s, BTL_TOKEN_AS)) {
            alias = parseVariable(p, s, c, "Expect variable name after 'as'.");
        } else {
            const char* path = pathToken.start + 1;
            int length = pathToken.length - 2;
            const char* filename = path;
            for (int i = 0; i < length; i++) {
                if (path[i] == '/' || path[i] == '\\') filename = path + i + 1;
            }
            int nameLength = (int) (path + length - filename);
            for (int i = 0; i < nameLength; i++) {
                if (filename[i] == '.') {
                    nameLength = i;
                    break;
                }
            }
            BtlToken nameToken = { .start = filename, .length = nameLength };
            declareVariable(p, c);
            alias = identifierConstant(c, &nameToken);
        }
        emitLong(p, c, BTL_OP_IMPORT, BTL_OP_IMPORT_LONG, file);
        defineVariable(p, c, alias);
        consume(p, s, BTL_TOKEN_SEMICOLON, "Expect ';' after import.");
    } else {
        statement(p, s, c, cc);
    }
    if (p->panicMode) {
        advance(p, s);
        while (p->current.type != BTL_TOKEN_EOF) {
            if (p->previous.type == BTL_TOKEN_SEMICOLON) {
                p->panicMode = false;
                return;
            }
            switch (p->current.type) {
            case BTL_TOKEN_CLASS:
            case BTL_TOKEN_FUNC:
            case BTL_TOKEN_VAR:
            case BTL_TOKEN_FOR:
            case BTL_TOKEN_IF:
            case BTL_TOKEN_WHILE:
            case BTL_TOKEN_RETURN:
                p->panicMode = false;
                return;
            default:;
            }
            advance(p, s);
        }
    }
}
ObjFunction* btl_compile(struct VM* vm, ObjModule* module, const char* source) {
    BtlScanner s;
    btl_scanner_init(&s, source);
    Parser p = { .vm = vm, .hadError = false, .panicMode = false };
    BtlCompiler c;
    initCompiler(&p, &c, NULL, BTL_TYPE_SCRIPT, module);
    advance(&p, &s);
    while (!match(&p, &s, BTL_TOKEN_EOF)) declaration(&p, &s, &c, NULL);
    ObjFunction* function = endCompiler(&p, &c);
    return p.hadError ? NULL : function;
}
void btl_compiler_mark_roots(struct VM* vm) {
    BtlCompiler* c = (BtlCompiler*) vm->compiler;
    while (c) {
        btl_gc_mark_object(vm, (BtlObj*) c->function);
        btl_table_mark(vm, &c->constants);
        c = c->enclosing;
    }
}