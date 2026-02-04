#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

// --- Parser & Grammar ---

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
    struct VM* vm;
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

typedef void (*ParseFn)(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    bool isConstant;
    Value value;
    int length;
} LastInstruction;

// --- Forward Declarations ---

static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void function(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, FunctionType type);
static void switchStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool isStatement);
static int parseVariable(Parser* p, Scanner* s, Compiler* c, const char* errorMessage);
static void defineVariable(Parser* p, Compiler* c, int global);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence);
static void emitLong(Parser* p, Compiler* c, OpCode shortOp, OpCode longOp, uint32_t index);
static int makeConstant(Parser* p, Compiler* c, Value value);
static void emitConstant(Parser* p, Compiler* c, Value value);
static int resolveLocal(Parser* p, Compiler* c, Token* name);
static int resolveUpvalue(Parser* p, Compiler* c, Token* name);
static void emitVariableSet(Parser* p, Compiler* c, ClassCompiler* cc, Token name);
static void prefixIncDec(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign);
static void doExpr(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign);


// --- Chunk Management ---

static Chunk* currentChunk(Compiler* c) {
    return &c->function->chunk;
}

static void errorAt(Parser* p, Token* token, const char* message) {
    if (p->panicMode) return;
    p->panicMode = true;
    p->hadError = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type != TOKEN_ERROR) {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    fflush(stderr);
}

static void advance(Parser* p, Scanner* s) {
    p->previous = p->current;
    for (;;) {
        p->current = scanToken(s);
        if (p->current.type != TOKEN_ERROR) break;
        errorAt(p, &p->current, p->current.start);
    }
}

static void consume(Parser* p, Scanner* s, TokenType type, const char* message) {
    if (p->current.type == type) {
        advance(p, s);
        return;
    }
    errorAt(p, &p->current, message);
}

static bool check(Parser* p, TokenType type) {
    return p->current.type == type;
}

static bool match(Parser* p, Scanner* s, TokenType type) {
    if (!check(p, type)) return false;
    advance(p, s);
    return true;
}

static void emitByte(Parser* p, Compiler* c, uint8_t byte) {
    c->previousInstruction = c->lastInstruction;
    c->lastInstruction = currentChunk(c)->count;
    writeChunk(c->vm, currentChunk(c), byte, p->previous.line);
}

static void emitBytes(Parser* p, Compiler* c, uint8_t byte1, uint8_t byte2) {
    emitByte(p, c, byte1);
    writeChunk(c->vm, currentChunk(c), byte2, p->previous.line);
}

// Creates method signature string: "methodName/arity"
static ObjString* createMethodSignature(Compiler* c, Token* name, int arity) {
    int nameLen = name->length;
    char* buffer = ALLOCATE(c->vm, char, nameLen + 2);
    memcpy(buffer, name->start, nameLen);
    buffer[nameLen] = (char) arity;
    buffer[nameLen + 1] = '\0';
    ObjString* signature = copyString(c->vm, buffer, nameLen + 1);
    FREE_ARRAY(c->vm, char, buffer, nameLen + 2);
    return signature;
}

// Emit indexed invoke with optimized opcodes for 0-8 args
static void emitInvokeIndexed(Parser* p, Compiler* c, int methodIndex, int argCount) {
    if (argCount <= 8 && methodIndex < 256) {
        emitBytes(p, c, (uint8_t) (OP_INVOKE_0 + argCount), (uint8_t) methodIndex);
    } else if (methodIndex < 256) {
        emitByte(p, c, OP_INVOKE);
        writeChunk(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    } else {
        emitByte(p, c, OP_INVOKE_LONG);
        writeChunk(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    }
}

static void emitSuperInvokeIndexed(Parser* p, Compiler* c, int methodIndex, int argCount) {
    if (argCount <= 8 && methodIndex < 256) {
        emitBytes(p, c, (uint8_t) (OP_SUPER_INVOKE_0 + argCount), (uint8_t) methodIndex);
    } else if (methodIndex < 256) {
        emitByte(p, c, OP_SUPER_INVOKE);
        writeChunk(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    } else {
        emitByte(p, c, OP_SUPER_INVOKE_LONG);
        writeChunk(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    }
}

// Try to resolve method index at compile time
// Returns -1 if not possible (polymorphic call)
static int tryResolveMethodIndex(Compiler* c, ClassCompiler* cc, Token* name, int argCount) {
    if (cc == NULL) return -1;

    ObjString* signature = createMethodSignature(c, name, argCount);
    push(c->vm, OBJ_VAL(signature));

    Value indexValue;
    int methodIndex = -1;
    if (tableGet(&cc->methodIndices, OBJ_VAL(signature), &indexValue)) {
        methodIndex = (int) AS_NUMBER(indexValue);
    }

    pop(c->vm);
    return methodIndex;
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int) strlen(text);
    token.line = 0;
    token.type = TOKEN_IDENTIFIER;
    return token;
}

static void addLocal(Parser* p, Compiler* c, Token name) {
    if (c->localCount == 256) {
        errorAt(p, &name, "Too many local variables in function.");
        return;
    }

    Local* local = &c->locals[c->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
    local->isModified = false;
}

// --- Back-Patching Logic ---

static void markLocalAsModified(Compiler* c, int localIndex) {
    c->locals[localIndex].isModified = true;
    for (int i = 0; i < c->patchCount; i++) {
        if (c->patches[i].localIndex == localIndex) {
            int offset = c->patches[i].codeOffset;
            currentChunk(c)->code[offset] = 1;
        }
    }
}

static void addPatch(Compiler* c, int localIndex, int codeOffset) {
    if (c->patchCount < UINT8_COUNT) {
        c->patches[c->patchCount].localIndex = localIndex;
        c->patches[c->patchCount].codeOffset = codeOffset;
        c->patchCount++;
    }
}

// --- Bytecode Helpers ---

static void removeChunkTail(Chunk* chunk, int n) {
    if (n <= 0) return;
    if (n > chunk->count) n = chunk->count;
    chunk->count -= n;
}

static void emitPopOrRemoveLoad(Parser* p, Compiler* c) {
    Chunk* chunk = currentChunk(c);
    if (c->lastInstruction >= 0 && c->lastInstruction < chunk->count) {
        uint8_t prevOp = chunk->code[c->lastInstruction];
        if (prevOp == OP_RETURN) return;
    }
    if (chunk->count >= 2) {
        int lastIndex = chunk->count - 1;
        int opcodeIndex = lastIndex - 1;
        if (opcodeIndex >= 0) {
            uint8_t possibleOp = chunk->code[opcodeIndex];
            if (possibleOp == OP_GET_LOCAL || possibleOp == OP_GET_UPVALUE) {
                removeChunkTail(chunk, 2);
                c->lastInstruction = (chunk->count > 0) ? chunk->count - 1 : -1;
                c->previousInstruction = -1;
                return;
            }
        }
    }
    emitByte(p, c, OP_POP);
}

static void emitPopN(Parser* p, Compiler* c, unsigned int count) {
    if (count == 0) return;
    if (count == 1) {
        emitPopOrRemoveLoad(p, c);
        return;
    }
    const unsigned int CHUNK = 255;
    while (count > CHUNK) {
        emitBytes(p, c, OP_POP_N, (uint8_t) CHUNK);
        count -= CHUNK;
    }
    if (count == 1) {
        emitPopOrRemoveLoad(p, c);
    } else {
        emitBytes(p, c, OP_POP_N, (uint8_t) count);
    }
}

static void emitConstant(Parser* p, Compiler* c, Value value) {
    emitLong(p, c, OP_CONSTANT, OP_CONSTANT_LONG, makeConstant(p, c, value));
}

static LastInstruction getInstructionAt(Compiler* c, int offset) {
    Chunk* chunk = currentChunk(c);
    LastInstruction result = { .isConstant = false, .value = NULL_VAL, .length = 0 };
    if (offset < 0 || offset >= chunk->count) return result;

    uint8_t op = chunk->code[offset];
    if (op == OP_CONSTANT) {
        uint8_t index = chunk->code[offset + 1];
        result.isConstant = true;
        result.value = chunk->constants.values[index];
        result.length = 2;
    } else if (op == OP_CONSTANT_LONG) {
        uint8_t lo = chunk->code[offset + 1];
        uint8_t hi = chunk->code[offset + 2];
        uint16_t index = (hi << 8) | lo;
        result.isConstant = true;
        result.value = chunk->constants.values[index];
        result.length = 3;
    }
    return result;
}

static void emitLoop(Parser* p, Compiler* c, int loopStart) {
    emitByte(p, c, OP_LOOP);
    int offset = currentChunk(c)->count - loopStart + 2;
    emitByte(p, c, (offset >> 8) & 0xff);
    emitByte(p, c, offset & 0xff);
}

static int emitJump(Parser* p, Compiler* c, uint8_t instruction) {
    emitByte(p, c, instruction);
    writeChunk(c->vm, currentChunk(c), 0xff, p->previous.line);
    writeChunk(c->vm, currentChunk(c), 0xff, p->previous.line);
    return currentChunk(c)->count - 2;
}

static int emitFusedJump(Parser* p, Compiler* c, uint8_t defaultJump) {
    Chunk* chunk = currentChunk(c);

    if (chunk->count > 0) {
        uint8_t lastOp = chunk->code[chunk->count - 1];
        uint8_t fusedOp = 0;

        switch (lastOp) {
        case OP_EQUAL:   fusedOp = OP_JUMP_IF_NOT_EQUAL; break;
        case OP_GREATER: fusedOp = OP_JUMP_IF_NOT_GREATER; break;
        case OP_LESS:    fusedOp = OP_JUMP_IF_NOT_LESS; break;
        case OP_NOT:
            if (chunk->count > 1 && chunk->code[chunk->count - 2] == OP_EQUAL) {
                removeChunkTail(chunk, 1);
                lastOp = OP_EQUAL;
                fusedOp = OP_JUMP_IF_EQUAL;
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

static int makeConstant(Parser* p, Compiler* c, Value value) {
    Value existingIndex;
    if (tableGet(&c->constants, value, &existingIndex)) {
        return (int) AS_NUMBER(existingIndex);
    }
    push(c->vm, value);
    int constant = addConstant(c->vm, currentChunk(c), value);
    if (constant > UINT16_MAX) {
        errorAt(p, &p->previous, "Too many constants in chunk.");
        pop(c->vm);
        return 0;
    }
    tableSet(c->vm, &c->constants, value, NUMBER_VAL((double) constant));
    pop(c->vm);
    return constant;
}

static void emitLong(Parser* p, Compiler* c, OpCode shortOp, OpCode longOp, uint32_t index) {
    if (index < 256) {
        emitByte(p, c, shortOp);
        writeChunk(c->vm, currentChunk(c), (uint8_t) index, p->previous.line);
    } else {
        emitByte(p, c, longOp);
        writeChunk(c->vm, currentChunk(c), (uint8_t) (index & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) ((index >> 8) & 0xff), p->previous.line);
    }
}

static void patchJump(Parser* p, Compiler* c, int offset) {
    int jump = currentChunk(c)->count - offset - 2;
    if (jump > UINT16_MAX) errorAt(p, &p->previous, "Too much code to jump over.");
    currentChunk(c)->code[offset] = (jump >> 8) & 0xff;
    currentChunk(c)->code[offset + 1] = jump & 0xff;
}

static void emitGetPropertyIC(Parser* p, Compiler* c, int nameIdx) {
    if (nameIdx > 255 || c->fieldICCount > 255) {
        errorAt(p, &p->previous, "Too many property accesses in function.");
        return;
    }
    emitByte(p, c, OP_GET_PROPERTY_IC);
    writeChunk(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    writeChunk(c->vm, currentChunk(c), (uint8_t) c->fieldICCount++, p->previous.line);
}

static void emitSetPropertyIC(Parser* p, Compiler* c, int nameIdx) {
    if (nameIdx > 255 || c->fieldICCount > 255) {
        errorAt(p, &p->previous, "Too many property accesses in function.");
        return;
    }
    emitByte(p, c, OP_SET_PROPERTY_IC);
    writeChunk(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    writeChunk(c->vm, currentChunk(c), (uint8_t) c->fieldICCount++, p->previous.line);
}

static void emitInvokeIC(Parser* p, Compiler* c, int nameIdx, int argCount) {
    if (nameIdx > 255 || c->methodICCount > 255) {
        errorAt(p, &p->previous, "Too many method calls in function.");
        return;
    }
    emitByte(p, c, OP_INVOKE_IC);
    writeChunk(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
    writeChunk(c->vm, currentChunk(c), (uint8_t) argCount, p->previous.line);
    writeChunk(c->vm, currentChunk(c), (uint8_t) c->methodICCount++, p->previous.line);
}

static void initCompiler(Parser* p, Compiler* c, Compiler* enclosing, FunctionType type, ObjModule* module) {
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
    initTable(&c->constants);
    c->currentLoop = NULL;
    c->currentSwitch = NULL;
    c->fieldICCount = 0;
    c->methodICCount = 0;
    c->function = newFunction(p->vm, module);
    c->vm->compiler = (void*) c;

    if (type != TYPE_SCRIPT) {
        c->function->name = copyString(p->vm, p->previous.start, p->previous.length);
    }

    Local* local = &c->locals[c->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->isModified = false;

    if (type != TYPE_SCRIPT) {
        if (type == TYPE_METHOD || type == TYPE_INITIALIZER) {
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

static ObjFunction* endCompiler(Parser* p, Compiler* c) {
    if (c->type == TYPE_INITIALIZER || c->type == TYPE_SCRIPT) {
        emitBytes(p, c, OP_GET_LOCAL, 0);
    } else {
        emitByte(p, c, OP_NULL);
    }
    emitByte(p, c, OP_RETURN);
    ObjFunction* function = c->function;
    function->fieldICCount = c->fieldICCount;
    function->methodICCount = c->methodICCount;
#ifdef DEBUG_PRINT_CODE
    if (!p->hadError) disassembleChunk(currentChunk(c), function->name != NULL ? function->name->chars : "<script>");
#endif
    freeTable(c->vm, &c->constants);
    c->vm->compiler = (void*) c->enclosing;
    return function;
}

static void beginScope(Compiler* c) {
    c->scopeDepth++;
}

static void endScope(Parser* p, Compiler* c) {
    c->scopeDepth--;
    int popCount = 0;
    while (c->localCount > 0 && c->locals[c->localCount - 1].depth > c->scopeDepth) {
        if (c->locals[c->localCount - 1].isCaptured) {
            if (popCount) {
                emitPopN(p, c, popCount);
                popCount = 0;
            }
            emitByte(p, c, OP_CLOSE_UPVALUE);
        } else {
            popCount++;
        }
        c->localCount--;
    }
    if (popCount) emitPopN(p, c, popCount);
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int identifierConstant(Compiler* c, Token* name) {
    ObjString* nameString = copyString(c->vm, name->start, name->length);
    push(c->vm, OBJ_VAL(nameString));
    Value indexValue;
    if (tableGet(&c->module->globalNames, OBJ_VAL(nameString), &indexValue)) {
        pop(c->vm);
        return (int) AS_NUMBER(indexValue);
    }
    int index = c->module->globalValues.count;
    writeValueArray(c->vm, &c->module->globalValues, EMPTY_VAL);
    tableSet(c->vm, &c->module->globalNames, OBJ_VAL(nameString), NUMBER_VAL((double) index));
    pop(c->vm);
    return index;
}

static int resolveLocal(Parser* p, Compiler* c, Token* name) {
    for (int i = c->localCount - 1; i >= 0; i--) {
        if (identifiersEqual(name, &c->locals[i].name)) {
            if (c->locals[i].depth == -1) errorAt(p, name, "Can't read local variable in its own initializer.");
            return i;
        }
    }
    return -1;
}

static int addUpvalue(Compiler* c, uint8_t index, bool isLocal, bool isMutable) {
    int count = c->function->upvalueCount;
    for (int i = 0; i < count; i++) {
        if (c->upvalues[i].index == index && c->upvalues[i].isLocal == isLocal) return i;
    }
    c->upvalues[count].isLocal = isLocal;
    c->upvalues[count].index = index;
    c->upvalues[count].isMutable = isMutable;
    return c->function->upvalueCount++;
}

static int resolveUpvalue(Parser* p, Compiler* c, Token* name) {
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

static void emitUpvalue(Parser* p, Compiler* c, uint8_t arg, bool isSet) {
    if (isSet) {
        if (arg == 0) emitByte(p, c, OP_SET_UPVALUE_0);
        else if (arg == 1) emitByte(p, c, OP_SET_UPVALUE_1);
        else if (arg == 2) emitByte(p, c, OP_SET_UPVALUE_2);
        else if (arg == 3) emitByte(p, c, OP_SET_UPVALUE_3);
        else emitBytes(p, c, OP_SET_UPVALUE, arg);
    } else {
        if (arg == 0) emitByte(p, c, OP_GET_UPVALUE_0);
        else if (arg == 1) emitByte(p, c, OP_GET_UPVALUE_1);
        else if (arg == 2) emitByte(p, c, OP_GET_UPVALUE_2);
        else if (arg == 3) emitByte(p, c, OP_GET_UPVALUE_3);
        else emitBytes(p, c, OP_GET_UPVALUE, arg);
    }
}

static void emitVariableSet(Parser* p, Compiler* c, ClassCompiler* cc, Token name) {
    int arg = resolveLocal(p, c, &name);

    if (arg != -1) {
        markLocalAsModified(c, arg);
        if (arg <= 7) {
            emitByte(p, c, (uint8_t) (OP_SET_LOCAL_0 + arg));
        } else {
            emitBytes(p, c, OP_SET_LOCAL, (uint8_t) arg);
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
        ObjString* fieldName = copyString(c->vm, name.start, name.length);
        Value indexVal;
        if (tableGet(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            uint8_t index = (uint8_t) AS_NUMBER(indexVal);
            emitBytes(p, c, OP_SET_FIELD_THIS, index);
            return;
        }
    }

    arg = identifierConstant(c, &name);
    emitLong(p, c, OP_SET_GLOBAL, OP_SET_GLOBAL_LONG, arg);
}

static void namedVariable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Token name, bool canAssign) {
    int arg = resolveLocal(p, c, &name);

    if (arg != -1) {
        // --- LOCAL VARIABLE ---
        if (canAssign && match(p, s, TOKEN_EQUAL)) {
            int exprStart = currentChunk(c)->count;
            expression(p, s, c, cc);

            // Lookback Optimization for ++ (e.g., i = i + 1)
            Chunk* chunk = currentChunk(c);
            if (chunk->count >= exprStart + 3) {
                uint8_t op1 = chunk->code[exprStart];
                uint8_t op2 = chunk->code[exprStart + 1];
                uint8_t op3 = chunk->code[exprStart + 2];
                bool isCorrectVar = ((op1 == (uint8_t) (OP_GET_LOCAL_0 + arg)) && arg <= 7) ||
                    (op1 == OP_GET_LOCAL && chunk->code[exprStart + 1] == arg);
                bool isPlusOne = (op2 == OP_1 && op3 == OP_ADD) || (op3 == OP_1 && op2 == OP_ADD);
                if (isCorrectVar && isPlusOne) {
                    chunk->count = exprStart;
                    emitBytes(p, c, OP_INC_LOCAL, (uint8_t) arg);
                    markLocalAsModified(c, arg);
                    return;
                }
            }
            markLocalAsModified(c, arg);
            if (arg <= 7) emitByte(p, c, (uint8_t) (OP_SET_LOCAL_0 + arg));
            else emitBytes(p, c, OP_SET_LOCAL, (uint8_t) arg);
        } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
            match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
            match(p, s, TOKEN_PERCENT_EQUAL))) {

            TokenType assignOp = p->previous.type;

            // Load current value
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (OP_GET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, OP_GET_LOCAL, (uint8_t) arg);
            }

            expression(p, s, c, cc);

            // Apply the operation
            switch (assignOp) {
            case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
            case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
            case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
            case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
            case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
            default: break;
            }

            markLocalAsModified(c, arg);
            if (arg <= 7) emitByte(p, c, (uint8_t) (OP_SET_LOCAL_0 + arg));
            else emitBytes(p, c, OP_SET_LOCAL, (uint8_t) arg);
        } else {
            if (arg <= 7) emitByte(p, c, (uint8_t) (OP_GET_LOCAL_0 + arg));
            else emitBytes(p, c, OP_GET_LOCAL, (uint8_t) arg);
        }
        return;
    }

    arg = resolveUpvalue(p, c, &name);
    if (arg != -1) {
        // --- UPVALUE ---
        if (canAssign && match(p, s, TOKEN_EQUAL)) {
            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }
            expression(p, s, c, cc);
            emitUpvalue(p, c, (uint8_t) arg, true);
        } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
            match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
            match(p, s, TOKEN_PERCENT_EQUAL))) {

            TokenType assignOp = p->previous.type;

            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }

            // Load current value
            emitUpvalue(p, c, (uint8_t) arg, false);

            expression(p, s, c, cc);

            switch (assignOp) {
            case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
            case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
            case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
            case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
            case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
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
        ObjString* fieldName = copyString(c->vm, name.start, name.length);
        Value indexVal;
        if (tableGet(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            uint8_t index = (uint8_t) AS_NUMBER(indexVal);
            if (canAssign && match(p, s, TOKEN_EQUAL)) {
                expression(p, s, c, cc);
                emitBytes(p, c, OP_SET_FIELD_THIS, index);
            } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
                match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
                match(p, s, TOKEN_PERCENT_EQUAL))) {

                TokenType assignOp = p->previous.type;

                // Load current value
                emitBytes(p, c, OP_GET_FIELD_THIS, index);

                expression(p, s, c, cc);

                switch (assignOp) {
                case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
                case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
                case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
                case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
                case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
                default: break;
                }

                emitBytes(p, c, OP_SET_FIELD_THIS, index);
            } else {
                emitBytes(p, c, OP_GET_FIELD_THIS, index);
            }
            return;
        }
    }

    // --- GLOBAL VARIABLE ---
    arg = identifierConstant(c, &name);
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitLong(p, c, OP_SET_GLOBAL, OP_SET_GLOBAL_LONG, arg);
    } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
        match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
        match(p, s, TOKEN_PERCENT_EQUAL))) {

        TokenType assignOp = p->previous.type;

        // Load current value
        emitLong(p, c, OP_GET_GLOBAL, OP_GET_GLOBAL_LONG, arg);

        expression(p, s, c, cc);

        switch (assignOp) {
        case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
        case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
        case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
        case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
        case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
        default: break;
        }

        emitLong(p, c, OP_SET_GLOBAL, OP_SET_GLOBAL_LONG, arg);
    } else {
        emitLong(p, c, OP_GET_GLOBAL, OP_GET_GLOBAL_LONG, arg);
    }
}

static void markInitialized(Compiler* c) {
    if (c->scopeDepth == 0) return;
    c->locals[c->localCount - 1].depth = c->scopeDepth;
}

static void declareVariable(Parser* p, Compiler* c) {
    if (c->scopeDepth == 0) return;
    Token* name = &p->previous;
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* local = &c->locals[i];
        if (local->depth != -1 && local->depth < c->scopeDepth) break;
        if (identifiersEqual(name, &local->name)) errorAt(p, name, "Already a variable with this name in this scope.");
    }
    addLocal(p, c, *name);
}

// --- Expression Parse Functions ---

static void func(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    function(p, s, c, cc, TYPE_FUNCTION);
}

static void binary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    TokenType opType = p->previous.type;
    ParseRule* rule = getRule(opType);
    int lhsOffset = c->lastInstruction;
    LastInstruction lhs = getInstructionAt(c, lhsOffset);
    parsePrecedence(p, s, c, cc, (Precedence) (rule->precedence + 1));
    int rhsOffset = c->lastInstruction;
    LastInstruction rhs = getInstructionAt(c, rhsOffset);

    if (lhs.isConstant && rhs.isConstant && (lhsOffset + lhs.length == rhsOffset)) {
        if (IS_NUMBER(lhs.value) && IS_NUMBER(rhs.value)) {
            double a = AS_NUMBER(lhs.value);
            double b = AS_NUMBER(rhs.value);
            double res;
            bool folded = true;
            switch (opType) {
            case TOKEN_PLUS:    res = a + b; break;
            case TOKEN_MINUS:   res = a - b; break;
            case TOKEN_STAR:    res = a * b; break;
            case TOKEN_SLASH:
                if (b == 0) {
                    errorAt(p, &p->previous, "Division by zero.");
                    return;
                }
                res = a / b;
                break;
            case TOKEN_PERCENT: res = fmod(a, b); break;
            default: folded = false;
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
    case TOKEN_BANG_EQUAL:    emitBytes(p, c, OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(p, c, OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(p, c, OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(p, c, OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(p, c, OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(p, c, OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emitByte(p, c, OP_ADD); break;
    case TOKEN_MINUS:         emitByte(p, c, OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(p, c, OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(p, c, OP_DIVIDE); break;
    case TOKEN_PERCENT:       emitByte(p, c, OP_MODULO); break;
    default: return;
    }
}

static void literal(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    switch (p->previous.type) {
    case TOKEN_FALSE: emitByte(p, c, OP_FALSE); break;
    case TOKEN_NULL:   emitByte(p, c, OP_NULL); break;
    case TOKEN_TRUE:  emitByte(p, c, OP_TRUE); break;
    default: return;
    }
}

static void grouping(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    double value = strtod(p->previous.start, NULL);

    if (value == 0.0) {
        emitByte(p, c, OP_0);
    } else if (value == 1.0) {
        emitByte(p, c, OP_1);
    } else if (value == 2.0) {
        emitByte(p, c, OP_2);
    } else {
        emitConstant(p, c, NUMBER_VAL(value));
    }
}

static void string(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    emitConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start + 1, p->previous.length - 2)));
}

static void variable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    Token name = p->previous;

    // Check for postfix ++ or -- BEFORE calling namedVariable
    if (canAssign && (check(p, TOKEN_PLUS_PLUS) || check(p, TOKEN_MINUS_MINUS))) {
        bool isInc = check(p, TOKEN_PLUS_PLUS);
        advance(p, s);  // consume ++ or --

        // Resolve variable location once
        int arg = resolveLocal(p, c, &name);
        if (arg != -1) {
            // Local variable
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (OP_GET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, OP_GET_LOCAL, (uint8_t) arg);
            }
            emitByte(p, c, OP_DUP);
            emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
            markLocalAsModified(c, arg);
            if (arg <= 7) {
                emitByte(p, c, (uint8_t) (OP_SET_LOCAL_0 + arg));
            } else {
                emitBytes(p, c, OP_SET_LOCAL, (uint8_t) arg);
            }
            emitByte(p, c, OP_POP);
            return;
        }

        arg = resolveUpvalue(p, c, &name);
        if (arg != -1) {
            // Upvalue
            if (c->upvalues[arg].isLocal && c->enclosing != NULL) {
                markLocalAsModified(c->enclosing, c->upvalues[arg].index);
            }
            emitUpvalue(p, c, (uint8_t) arg, false);
            emitByte(p, c, OP_DUP);
            emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
            emitUpvalue(p, c, (uint8_t) arg, true);
            emitByte(p, c, OP_POP);
            return;
        }

        if (cc != NULL) {
            // Class field
            ObjString* fieldName = copyString(c->vm, name.start, name.length);
            Value indexVal;
            if (tableGet(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
                uint8_t index = (uint8_t) AS_NUMBER(indexVal);
                emitBytes(p, c, OP_GET_FIELD_THIS, index);
                emitByte(p, c, OP_DUP);
                emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
                emitBytes(p, c, OP_SET_FIELD_THIS, index);
                emitByte(p, c, OP_POP);
                return;
            }
        }

        // Global variable - get index once and reuse
        arg = identifierConstant(c, &name);
        emitLong(p, c, OP_GET_GLOBAL, OP_GET_GLOBAL_LONG, arg);
        emitByte(p, c, OP_DUP);
        emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
        emitLong(p, c, OP_SET_GLOBAL, OP_SET_GLOBAL_LONG, arg);
        emitByte(p, c, OP_POP);
        return;
    }

    namedVariable(p, s, c, cc, name, canAssign);
}

static void prefixIncDec(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    TokenType op = p->previous.type;
    bool isInc = (op == TOKEN_PLUS_PLUS);

    // Check if this is a property access (++this.field or ++obj.prop)
    if (check(p, TOKEN_THIS) || check(p, TOKEN_IDENTIFIER)) {
        Token possibleObj = p->current;
        advance(p, s);

        if (match(p, s, TOKEN_DOT)) {
            // It's a property access: ++this.field or ++obj.prop
            consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'.");
            Token propName = p->previous;

            // Check if it's 'this'
            bool isThis = (possibleObj.length == 4 && memcmp(possibleObj.start, "this", 4) == 0);

            if (isThis && cc != NULL) {
                // ++this.field
                ObjString* fieldName = copyString(c->vm, propName.start, propName.length);
                Value indexVal;
                if (!tableGet(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
                    indexVal = NUMBER_VAL((double) cc->fieldCount);
                    tableSet(c->vm, &cc->fields, OBJ_VAL(fieldName), indexVal);
                    cc->fieldCount++;
                }
                uint8_t index = (uint8_t) AS_NUMBER(indexVal);

                // Get, increment, dup, set, pop (leaves new value on stack)
                emitBytes(p, c, OP_GET_FIELD_THIS, index);
                emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
                emitByte(p, c, OP_DUP);
                emitBytes(p, c, OP_SET_FIELD_THIS, index);
                emitByte(p, c, OP_POP);
            } else {
                // ++obj.prop
                namedVariable(p, s, c, cc, possibleObj, false);
                int nameIdx = makeConstant(p, c, OBJ_VAL(copyString(c->vm, propName.start, propName.length)));
                emitGetPropertyIC(p, c, nameIdx);
                emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
                emitByte(p, c, OP_DUP);
                emitSetPropertyIC(p, c, nameIdx);
                emitByte(p, c, OP_POP);
            }
            return;
        } else {
            // Not a property access, it's a simple variable
            // possibleObj is the variable name
            Token name = possibleObj;

            // Special optimization for local variables
            int arg = resolveLocal(p, c, &name);
            if (arg != -1 && isInc) {
                // Use optimized OP_INC_LOCAL
                markLocalAsModified(c, arg);
                emitBytes(p, c, OP_INC_LOCAL, (uint8_t) arg);
                return;
            }

            // General case: get, modify, dup, set, pop
            namedVariable(p, s, c, cc, name, false);
            emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
            emitByte(p, c, OP_DUP);
            emitVariableSet(p, c, cc, name);
            emitByte(p, c, OP_POP);
            return;
        }
    }

    errorAt(p, &p->previous, "Expect variable or property after '++' or '--'.");
}

static void list(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;

    // Check for empty dictionary [:] 
    if (check(p, TOKEN_COLON)) {
        advance(p, s);
        consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']' after '[:'.");
        emitBytes(p, c, OP_BUILD_TABLE, 0);
        return;
    }

    int count = 0;
    if (!check(p, TOKEN_RIGHT_BRACKET)) {
        do {
            expression(p, s, c, cc);

            // After first expression, check for colon (dictionary)
            if (count == 0 && check(p, TOKEN_COLON)) {
                // This is a dictionary, not a list
                // The first key is already on the stack
                consume(p, s, TOKEN_COLON, "Expect ':'.");
                expression(p, s, c, cc);
                count = 1;

                // Continue parsing remaining key:value pairs
                while (match(p, s, TOKEN_COMMA)) {
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);
                    consume(p, s, TOKEN_COLON, "Expect ':' after dictionary key.");
                    expression(p, s, c, cc);

                    if (count == 255) errorAt(p, &p->previous, "Dictionary too large.");
                    count++;
                }

                consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']'.");
                emitBytes(p, c, OP_BUILD_TABLE, (uint8_t) count);
                return;
            }

            if (count == 255) errorAt(p, &p->previous, "List too large.");
            count++;
        } while (match(p, s, TOKEN_COMMA));
    }

    consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']'.");
    emitBytes(p, c, OP_BUILD_LIST, (uint8_t) count);
}

static void subscript(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']'.");
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitByte(p, c, OP_INDEX_SET);
    } else {
        emitByte(p, c, OP_INDEX_GET);
    }
}

static void dot(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'.");
    Token name = p->previous;
    ObjString* fieldName = copyString(c->vm, name.start, name.length);

    bool isThis = false;
    if (cc != NULL && c->lastInstruction != -1) {
        uint8_t lastOp = currentChunk(c)->code[c->lastInstruction];
        if (lastOp == OP_GET_LOCAL_0) isThis = true;
    }

    // Specialized 'this' field access
    if (isThis && !check(p, TOKEN_LEFT_PAREN)) {
        Value indexVal;
        if (!tableGet(&cc->fields, OBJ_VAL(fieldName), &indexVal)) {
            indexVal = NUMBER_VAL((double) cc->fieldCount);
            tableSet(c->vm, &cc->fields, OBJ_VAL(fieldName), indexVal);
            cc->fieldCount++;
        }
        uint8_t index = (uint8_t) AS_NUMBER(indexVal);
        removeChunkTail(currentChunk(c), 1);

        if (canAssign && match(p, s, TOKEN_EQUAL)) {
            expression(p, s, c, cc);
            emitBytes(p, c, OP_SET_FIELD_THIS, index);
        } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
            match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
            match(p, s, TOKEN_PERCENT_EQUAL))) {
            TokenType assignOp = p->previous.type;
            emitBytes(p, c, OP_GET_FIELD_THIS, index);
            expression(p, s, c, cc);
            switch (assignOp) {
            case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
            case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
            case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
            case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
            case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
            default: break;
            }
            emitBytes(p, c, OP_SET_FIELD_THIS, index);
        } else if (match(p, s, TOKEN_PLUS_PLUS) || match(p, s, TOKEN_MINUS_MINUS)) {
            TokenType op = p->previous.type;
            bool isInc = (op == TOKEN_PLUS_PLUS);
            emitBytes(p, c, OP_GET_FIELD_THIS, index);
            emitByte(p, c, OP_DUP);
            emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
            emitBytes(p, c, OP_SET_FIELD_THIS, index);
            emitByte(p, c, OP_POP);
        } else {
            emitBytes(p, c, OP_GET_FIELD_THIS, index);
        }
        return;
    }

    // Method call
    if (match(p, s, TOKEN_LEFT_PAREN)) {
        if (isThis && cc != NULL) {
            uint8_t args = 0;
            if (!check(p, TOKEN_RIGHT_PAREN)) {
                do {
                    expression(p, s, c, cc);
                    args++;
                } while (match(p, s, TOKEN_COMMA));
            }
            consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");

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
            if (!check(p, TOKEN_RIGHT_PAREN)) {
                do {
                    expression(p, s, c, cc);
                    args++;
                } while (match(p, s, TOKEN_COMMA));
            }
            consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");

            emitInvokeIC(p, c, nameIdx, args);
        }
    } else {
        int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));

        if (canAssign && match(p, s, TOKEN_EQUAL)) {
            expression(p, s, c, cc);
            emitSetPropertyIC(p, c, nameIdx);
        } else if (canAssign && (match(p, s, TOKEN_PLUS_EQUAL) || match(p, s, TOKEN_MINUS_EQUAL) ||
            match(p, s, TOKEN_STAR_EQUAL) || match(p, s, TOKEN_SLASH_EQUAL) ||
            match(p, s, TOKEN_PERCENT_EQUAL))) {
            TokenType assignOp = p->previous.type;
            emitGetPropertyIC(p, c, nameIdx);
            expression(p, s, c, cc);
            switch (assignOp) {
            case TOKEN_PLUS_EQUAL:    emitByte(p, c, OP_ADD); break;
            case TOKEN_MINUS_EQUAL:   emitByte(p, c, OP_SUBTRACT); break;
            case TOKEN_STAR_EQUAL:    emitByte(p, c, OP_MULTIPLY); break;
            case TOKEN_SLASH_EQUAL:   emitByte(p, c, OP_DIVIDE); break;
            case TOKEN_PERCENT_EQUAL: emitByte(p, c, OP_MODULO); break;
            default: break;
            }
            emitSetPropertyIC(p, c, nameIdx);
        } else if (match(p, s, TOKEN_PLUS_PLUS) || match(p, s, TOKEN_MINUS_MINUS)) {
            TokenType op = p->previous.type;
            bool isInc = (op == TOKEN_PLUS_PLUS);
            emitGetPropertyIC(p, c, nameIdx);
            emitByte(p, c, OP_DUP);
            emitByte(p, c, isInc ? OP_INCREMENT : OP_DECREMENT);
            emitSetPropertyIC(p, c, nameIdx);
        } else {
            emitGetPropertyIC(p, c, nameIdx);
        }
    }
}

static void unary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    TokenType opType = p->previous.type;
    parsePrecedence(p, s, c, cc, PREC_UNARY);
    int operandOffset = c->lastInstruction;
    LastInstruction operand = getInstructionAt(c, operandOffset);
    if (operand.isConstant) {
        if (opType == TOKEN_MINUS && IS_NUMBER(operand.value)) {
            currentChunk(c)->count = operandOffset;
            c->lastInstruction = c->previousInstruction;
            emitConstant(p, c, NUMBER_VAL(-AS_NUMBER(operand.value)));
            return;
        }
        if (opType == TOKEN_BANG) {
            currentChunk(c)->count = operandOffset;
            c->lastInstruction = c->previousInstruction;
            bool isFalsey = IS_NULL(operand.value) || (IS_BOOL(operand.value) && !AS_BOOL(operand.value));
            emitByte(p, c, isFalsey ? OP_TRUE : OP_FALSE);
            return;
        }
    }
    switch (opType) {
    case TOKEN_BANG:  emitByte(p, c, OP_NOT); break;
    case TOKEN_MINUS: emitByte(p, c, OP_NEGATE); break;
    default: return;
    }
}

static void and_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int endJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_AND);
    patchJump(p, c, endJump);
}

static void or_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int elseJump = emitJump(p, c, OP_JUMP_IF_TRUE);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, elseJump);
}

static void call(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    uint8_t args = 0;
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            expression(p, s, c, cc);
            args++;
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
    switch (args) {
    case 0: emitByte(p, c, OP_CALL_0); break;
    case 1: emitByte(p, c, OP_CALL_1); break;
    case 2: emitByte(p, c, OP_CALL_2); break;
    case 3: emitByte(p, c, OP_CALL_3); break;
    case 4: emitByte(p, c, OP_CALL_4); break;
    case 5: emitByte(p, c, OP_CALL_5); break;
    case 6: emitByte(p, c, OP_CALL_6); break;
    case 7: emitByte(p, c, OP_CALL_7); break;
    case 8: emitByte(p, c, OP_CALL_8); break;
    default: emitBytes(p, c, OP_CALL, args); break;
    }
}

static void this_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'this' outside of a class.");
        return;
    }
    variable(p, s, c, cc, false);
}

static void super_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'super' outside of a class.");
    } else if (!cc->hasSuperclass) {
        errorAt(p, &p->previous, "Can't use 'super' in a class with no superclass.");
    }

    consume(p, s, TOKEN_DOT, "Expect '.'.");
    consume(p, s, TOKEN_IDENTIFIER, "Expect superclass method name.");
    Token name = p->previous;

    Token thisT = { .start = "this", .length = 4 };
    namedVariable(p, s, c, cc, thisT, false);
    Token superT = { .start = "super", .length = 5 };
    namedVariable(p, s, c, cc, superT, false);

    if (match(p, s, TOKEN_LEFT_PAREN)) {
        uint8_t args = 0;
        if (!check(p, TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                args++;
            } while (match(p, s, TOKEN_COMMA));
        }
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int methodIndex = tryResolveMethodIndex(c, cc, &name, args);

        if (methodIndex < 0) {
            errorAt(p, &name, "Undefined superclass method.");
            return;
        }

        emitSuperInvokeIndexed(p, c, methodIndex, args);
    } else {
        int nameIdx = makeConstant(p, c, OBJ_VAL(copyString(c->vm, name.start, name.length)));
        emitLong(p, c, OP_GET_SUPER, OP_GET_SUPER_LONG, nameIdx);
    }
}

static void switch_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    switchStatement(p, s, c, cc, false);
}

static void doExpr(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;

    // do func() { ... } - anonymous async function
    if (match(p, s, TOKEN_FUNC)) {
        function(p, s, c, cc, TYPE_FUNCTION);
        emitBytes(p, c, OP_DO_NEW, 0);
        return;
    }

    if (!check(p, TOKEN_IDENTIFIER)) {
        errorAt(p, &p->current, "Expect identifier or 'func' after 'do'.");
        return;
    }

    advance(p, s);
    Token name = p->previous;

    if (check(p, TOKEN_LEFT_PAREN)) {
        // do identifier() - Class or function call
        namedVariable(p, s, c, cc, name, false);

        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
        uint8_t argCount = 0;
        if (!check(p, TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                argCount++;
            } while (match(p, s, TOKEN_COMMA));
        }
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");

        emitBytes(p, c, OP_DO_NEW, argCount);

    } else if (check(p, TOKEN_DOT)) {
        // do obj.method()
        namedVariable(p, s, c, cc, name, false);

        consume(p, s, TOKEN_DOT, "Expect '.'.");
        consume(p, s, TOKEN_IDENTIFIER, "Expect method name.");
        Token methodName = p->previous;

        int nameConstant = makeConstant(p, c, OBJ_VAL(copyString(c->vm, methodName.start, methodName.length)));

        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
        uint8_t argCount = 0;
        if (!check(p, TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc);
                argCount++;
            } while (match(p, s, TOKEN_COMMA));
        }
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");

        emitByte(p, c, OP_DO_INVOKE);
        emitByte(p, c, (uint8_t) nameConstant);
        emitByte(p, c, argCount);

    } else {
        errorAt(p, &p->current, "Expect '(' or '.' after identifier in 'do'.");
    }
}

// --- Parse Rules Table ---

ParseRule rules [] = {
    [TOKEN_LEFT_PAREN] = {grouping, call,   PREC_CALL},
    [TOKEN_LEFT_BRACKET] = {list,     subscript, PREC_CALL},
    [TOKEN_DOT] = {NULL,     dot,    PREC_CALL},
    [TOKEN_MINUS] = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL,     binary, PREC_TERM},
    [TOKEN_STAR] = {NULL,     binary, PREC_FACTOR},
    [TOKEN_SLASH] = {NULL,     binary, PREC_FACTOR},
    [TOKEN_PERCENT] = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL] = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING] = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER] = {number,   NULL,   PREC_NONE},
    [TOKEN_AND] = {NULL,     and_,   PREC_AND},
    [TOKEN_OR] = {NULL,     or_,    PREC_OR},
    [TOKEN_FALSE] = {literal,  NULL,   PREC_NONE},
    [TOKEN_NULL] = {literal,  NULL,   PREC_NONE},
    [TOKEN_TRUE] = {literal,  NULL,   PREC_NONE},
    [TOKEN_SUPER] = {super_,   NULL,   PREC_NONE},
    [TOKEN_THIS] = {this_,    NULL,   PREC_NONE},
    [TOKEN_FUNC] = {func,     NULL,   PREC_NONE},
    [TOKEN_SWITCH] = {switch_,  NULL,   PREC_NONE},
    [TOKEN_COLON] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PLUS_PLUS] = {prefixIncDec, NULL, PREC_NONE},
    [TOKEN_MINUS_MINUS] = {prefixIncDec, NULL, PREC_NONE},
    [TOKEN_DO] = {doExpr, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence prec) {
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
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        errorAt(p, &p->previous, "Invalid assignment target.");
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    parsePrecedence(p, s, c, cc, PREC_ASSIGNMENT);
}

// --- Statement Functions ---

static void block(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        declaration(p, s, c, cc);
    }
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, FunctionType type) {
    Compiler sub;
    initCompiler(p, &sub, c, type, c->module);
    beginScope(&sub);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++;
            int constant = parseVariable(p, s, &sub, "Expect parameter name.");
            defineVariable(p, &sub, constant);
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{'.");
    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);
    push(c->vm, OBJ_VAL(f));
    int index = makeConstant(p, c, OBJ_VAL(f));
    emitLong(p, c, OP_CLOSURE, OP_CLOSURE_LONG, index);
    pop(c->vm);

    for (int i = 0; i < f->upvalueCount; i++) {
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);

        bool isMut = sub.upvalues[i].isMutable;
        if (sub.upvalues[i].isLocal) {
            if (c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            } else if (!isMut) {
                addPatch(c, sub.upvalues[i].index, currentChunk(c)->count);
            }
        }
        writeChunk(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
    }
}

static void method(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect method name.");
    Token nameToken = p->previous;
    FunctionType type = TYPE_METHOD;
    bool isInit = false;

    if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
        isInit = true;
    }

    Compiler sub;
    initCompiler(p, &sub, c, type, c->module);
    beginScope(&sub);

    consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++;
            int constant = parseVariable(p, s, &sub, "Expect parameter name.");
            defineVariable(p, &sub, constant);
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{'.");

    // INJECT FIELD INITIALIZERS at the start of init()
    if (isInit && cc != NULL) {
        for (int i = 0; i < cc->fieldCount; i++) {
            if (cc->fieldInfos[i].hasInit) {
                Scanner initScanner;
                initScanner.start = cc->fieldInfos[i].initSource;
                initScanner.current = cc->fieldInfos[i].initSource;
                initScanner.line = 1;

                Parser initParser = *p;
                initParser.hadError = false;
                initParser.panicMode = false;

                advance(&initParser, &initScanner);
                expression(&initParser, &initScanner, &sub, cc);

                emitBytes(p, &sub, OP_SET_FIELD_THIS, (uint8_t) i);
                emitPopOrRemoveLoad(p, &sub);

                if (initParser.hadError) {
                    errorAt(p, &nameToken, "Error in field initializer.");
                }
            }
        }
    }

    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);

    push(c->vm, OBJ_VAL(f));
    int fnIdx = makeConstant(p, c, OBJ_VAL(f));
    emitLong(p, c, OP_CLOSURE, OP_CLOSURE_LONG, fnIdx);
    pop(c->vm);

    for (int i = 0; i < f->upvalueCount; i++) {
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);
        bool isMut = sub.upvalues[i].isMutable;
        if (sub.upvalues[i].isLocal) {
            if (c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            } else if (!isMut) {
                addPatch(c, sub.upvalues[i].index, currentChunk(c)->count);
            }
        }
        writeChunk(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
    }

    ObjString* signature = createMethodSignature(c, &nameToken, f->arity);
    push(c->vm, OBJ_VAL(signature));

    Value indexValue;
    int methodIndex;
    if (tableGet(&cc->methodIndices, OBJ_VAL(signature), &indexValue)) {
        methodIndex = (int) AS_NUMBER(indexValue);
    } else {
        methodIndex = cc->nextMethodIndex++;
        tableSet(c->vm, &cc->methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
    }

    pop(c->vm);

    if (methodIndex < 256) {
        emitByte(p, c, OP_METHOD);
        writeChunk(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
    } else {
        emitByte(p, c, OP_METHOD_LONG);
        writeChunk(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
        writeChunk(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
    }
}

static void classDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect class name.");
    Token nameToken = p->previous;
    int nameStringConst = makeConstant(p, c, OBJ_VAL(copyString(c->vm, nameToken.start, nameToken.length)));
    int nameGlobalIdx = identifierConstant(c, &nameToken);

    declareVariable(p, c);
    emitLong(p, c, OP_CLASS, OP_CLASS_LONG, nameStringConst);
    defineVariable(p, c, nameGlobalIdx);

    ClassCompiler classC;
    classC.enclosing = cc;
    classC.hasSuperclass = false;
    classC.fieldCount = 0;
    initTable(&classC.fields);
    initTable(&classC.methodIndices);
    classC.nextMethodIndex = 0;

    classC.fieldInfoCapacity = 8;
    classC.fieldInfos = ALLOCATE(c->vm, FieldInfo, classC.fieldInfoCapacity);

    bool userDefinedInit = false;
    bool hasFieldInitializers = false;

    if (match(p, s, TOKEN_LESS)) {
        consume(p, s, TOKEN_IDENTIFIER, "Expect superclass name.");
        Token superClassName = p->previous;

        ObjString* parentName = copyString(c->vm, superClassName.start, superClassName.length);
        push(c->vm, OBJ_VAL(parentName));

        Value savedIndicesValue;
        if (tableGet(&c->module->classInfo, OBJ_VAL(parentName), &savedIndicesValue)) {
            Table* parentIndices = (Table*) (uintptr_t) AS_NUMBER(savedIndicesValue);
            tableAddAll(c->vm, parentIndices, &classC.methodIndices);

            int maxParentIndex = -1;
            for (int i = 0; i < parentIndices->capacity; i++) {
                Entry* entry = &parentIndices->entries[i];
                if (IS_STRING(entry->key) && IS_NUMBER(entry->value)) {
                    int idx = (int) AS_NUMBER(entry->value);
                    if (idx > maxParentIndex) maxParentIndex = idx;
                }
            }
            classC.nextMethodIndex = maxParentIndex + 1;
        }

        pop(c->vm);

        variable(p, s, c, &classC, false);

        if (identifiersEqual(&nameToken, &superClassName)) {
            errorAt(p, &superClassName, "A class can't inherit from itself.");
        }

        beginScope(c);
        addLocal(p, c, syntheticToken("super"));
        markInitialized(c);

        namedVariable(p, s, c, NULL, superClassName, false);
        namedVariable(p, s, c, NULL, nameToken, false);

        emitByte(p, c, OP_INHERIT);
        classC.hasSuperclass = true;
    }

    namedVariable(p, s, c, NULL, nameToken, false);

    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        if (match(p, s, TOKEN_VAR)) {
            do {
                consume(p, s, TOKEN_IDENTIFIER, "Expect variable name.");
                Token fieldName = p->previous;

                ObjString* name = copyString(c->vm, fieldName.start, fieldName.length);
                Value dummy;
                int fieldIndex;

                if (!tableGet(&classC.fields, OBJ_VAL(name), &dummy)) {
                    fieldIndex = classC.fieldCount++;
                    tableSet(c->vm, &classC.fields, OBJ_VAL(name), NUMBER_VAL((double) fieldIndex));

                    if (fieldIndex >= classC.fieldInfoCapacity) {
                        int oldCap = classC.fieldInfoCapacity;
                        classC.fieldInfoCapacity *= 2;
                        classC.fieldInfos = GROW_ARRAY(c->vm, FieldInfo, classC.fieldInfos,
                            oldCap, classC.fieldInfoCapacity);
                    }

                    classC.fieldInfos[fieldIndex].fieldName = name;
                    classC.fieldInfos[fieldIndex].fieldIndex = fieldIndex;
                    classC.fieldInfos[fieldIndex].hasInit = false;
                } else {
                    fieldIndex = (int) AS_NUMBER(dummy);
                }

                if (match(p, s, TOKEN_EQUAL)) {
                    hasFieldInitializers = true;

                    const char* exprStart = p->current.start;

                    int parenDepth = 0;
                    int braceDepth = 0;
                    int bracketDepth = 0;

                    while (!check(p, TOKEN_EOF)) {
                        if (check(p, TOKEN_LEFT_PAREN)) parenDepth++;
                        if (check(p, TOKEN_RIGHT_PAREN)) parenDepth--;
                        if (check(p, TOKEN_LEFT_BRACE)) braceDepth++;
                        if (check(p, TOKEN_RIGHT_BRACE)) braceDepth--;
                        if (check(p, TOKEN_LEFT_BRACKET)) bracketDepth++;
                        if (check(p, TOKEN_RIGHT_BRACKET)) bracketDepth--;

                        if (parenDepth == 0 && braceDepth == 0 && bracketDepth == 0) {
                            if (check(p, TOKEN_COMMA) || check(p, TOKEN_SEMICOLON)) {
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
            } while (match(p, s, TOKEN_COMMA));

            consume(p, s, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
        } else if (match(p, s, TOKEN_FUNC)) {
            if (check(p, TOKEN_IDENTIFIER) &&
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
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    // AUTO-GENERATE empty init() if field initializers exist but no user init
    if (hasFieldInitializers && !userDefinedInit) {
        namedVariable(p, s, c, NULL, nameToken, false);

        Token initToken;
        initToken.start = "init";
        initToken.length = 4;
        initToken.line = nameToken.line;
        initToken.type = TOKEN_IDENTIFIER;

        Compiler sub;
        initCompiler(p, &sub, c, TYPE_INITIALIZER, c->module);
        beginScope(&sub);

        sub.function->arity = 0;

        // Inject field initializers
        for (int i = 0; i < classC.fieldCount; i++) {
            if (classC.fieldInfos[i].hasInit) {
                Scanner initScanner;
                initScanner.start = classC.fieldInfos[i].initSource;
                initScanner.current = classC.fieldInfos[i].initSource;
                initScanner.line = 1;

                Parser initParser = *p;
                initParser.hadError = false;
                initParser.panicMode = false;

                advance(&initParser, &initScanner);
                expression(&initParser, &initScanner, &sub, &classC);

                emitBytes(p, &sub, OP_SET_FIELD_THIS, (uint8_t) i);
                emitPopOrRemoveLoad(p, &sub);

                if (initParser.hadError) {
                    p->hadError = true;
                }
            }
        }

        emitBytes(p, &sub, OP_GET_LOCAL, 0);
        emitByte(p, &sub, OP_RETURN);

        ObjFunction* f = sub.function;

        push(c->vm, OBJ_VAL(f));
        int fnIdx = makeConstant(p, c, OBJ_VAL(f));
        emitLong(p, c, OP_CLOSURE, OP_CLOSURE_LONG, fnIdx);
        pop(c->vm);

        for (int i = 0; i < f->upvalueCount; i++) {
            writeChunk(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
            writeChunk(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);
            bool isMut = sub.upvalues[i].isMutable;
            if (sub.upvalues[i].isLocal && c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            }
            writeChunk(c->vm, currentChunk(c), isMut ? 1 : 0, p->previous.line);
        }

        ObjString* signature = createMethodSignature(c, &initToken, 0);
        push(c->vm, OBJ_VAL(signature));

        Value indexValue;
        int methodIndex;
        if (tableGet(&classC.methodIndices, OBJ_VAL(signature), &indexValue)) {
            methodIndex = (int) AS_NUMBER(indexValue);
        } else {
            methodIndex = classC.nextMethodIndex++;
            tableSet(c->vm, &classC.methodIndices, OBJ_VAL(signature), NUMBER_VAL((double) methodIndex));
        }

        pop(c->vm);

        if (methodIndex < 256) {
            emitByte(p, c, OP_METHOD);
            writeChunk(c->vm, currentChunk(c), (uint8_t) methodIndex, p->previous.line);
            writeChunk(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
        } else {
            emitByte(p, c, OP_METHOD_LONG);
            writeChunk(c->vm, currentChunk(c), (uint8_t) (methodIndex & 0xff), p->previous.line);
            writeChunk(c->vm, currentChunk(c), (uint8_t) ((methodIndex >> 8) & 0xff), p->previous.line);
            writeChunk(c->vm, currentChunk(c), (uint8_t) f->arity, p->previous.line);
        }

        emitByte(p, c, OP_POP);
    }

    // Sync Fields
    for (int i = 0; i < classC.fieldCount; i++) {
        ObjString* foundName = NULL;
        for (int j = 0; j < classC.fields.capacity; j++) {
            Entry* entry = &classC.fields.entries[j];
            if (IS_STRING(entry->key) && (int) AS_NUMBER(entry->value) == i) {
                foundName = AS_STRING(entry->key);
                break;
            }
        }
        if (foundName != NULL) {
            int nameIdx = makeConstant(p, c, OBJ_VAL(foundName));
            emitLong(p, c, OP_FIELD, OP_FIELD, nameIdx);
        }
    }

    emitByte(p, c, OP_POP);

    if (classC.hasSuperclass) {
        endScope(p, c);
    }

    freeTable(c->vm, &classC.fields);

    // Save method indices AFTER auto-generated init is added
    ObjString* className = copyString(c->vm, nameToken.start, nameToken.length);
    push(c->vm, OBJ_VAL(className));

    Table* savedIndices = ALLOCATE(c->vm, Table, 1);
    initTable(savedIndices);
    tableAddAll(c->vm, &classC.methodIndices, savedIndices);

    tableSet(c->vm, &c->module->classInfo, OBJ_VAL(className),
        NUMBER_VAL((double) (uintptr_t) savedIndices));

    pop(c->vm);
    freeTable(c->vm, &classC.methodIndices);
    FREE_ARRAY(c->vm, FieldInfo, classC.fieldInfos, classC.fieldInfoCapacity);
}

static void funDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    int global = parseVariable(p, s, c, "Expect function name.");
    markInitialized(c);
    function(p, s, c, cc, TYPE_FUNCTION);
    defineVariable(p, c, global);
}

static void switchStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool isStatement) {
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after switch expression.");
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before switch body.");

    SwitchContext switchCtx;
    switchCtx.enclosing = c->currentSwitch;
    switchCtx.caseJumpCount = 0;
    switchCtx.caseJumpCapacity = 8;
    switchCtx.caseJumps = ALLOCATE(c->vm, int, switchCtx.caseJumpCapacity);
    switchCtx.breakCount = 0;
    switchCtx.breakCapacity = 8;
    switchCtx.breakJumps = ALLOCATE(c->vm, int, switchCtx.breakCapacity);
    switchCtx.scopeDepth = c->scopeDepth;
    switchCtx.hasDefault = false;
    switchCtx.isExpression = false;

    c->currentSwitch = &switchCtx;

    int* fallthroughJumps = ALLOCATE(c->vm, int, 16);
    int fallthroughCount = 0;
    int fallthroughCapacity = 16;
    bool lastCaseHadBreak = true;

    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        if (match(p, s, TOKEN_CASE)) {
            if (!lastCaseHadBreak) {
                int fallthroughJump = emitJump(p, c, OP_JUMP);
                if (fallthroughCount >= fallthroughCapacity) {
                    int oldCap = fallthroughCapacity;
                    fallthroughCapacity *= 2;
                    fallthroughJumps = GROW_ARRAY(c->vm, int, fallthroughJumps, oldCap, fallthroughCapacity);
                }
                fallthroughJumps[fallthroughCount++] = fallthroughJump;
            }

            if (switchCtx.caseJumpCount > 0) {
                patchJump(p, c, switchCtx.caseJumps[switchCtx.caseJumpCount - 1]);
                emitByte(p, c, OP_POP);
            }

            int* successJumps = ALLOCATE(c->vm, int, 8);
            int successCount = 0;
            int successCapacity = 8;

            int* failJumps = ALLOCATE(c->vm, int, 8);
            int failCount = 0;
            int failCapacity = 8;

            for (;;) {
                bool isBooleanCondition = (check(p, TOKEN_LESS) || check(p, TOKEN_LESS_EQUAL) ||
                    check(p, TOKEN_GREATER) || check(p, TOKEN_GREATER_EQUAL) ||
                    check(p, TOKEN_EQUAL_EQUAL) || check(p, TOKEN_BANG_EQUAL));

                if (isBooleanCondition) {
                    emitByte(p, c, OP_DUP);
                    TokenType op = p->current.type;
                    advance(p, s);
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);

                    switch (op) {
                    case TOKEN_LESS: emitByte(p, c, OP_LESS); break;
                    case TOKEN_LESS_EQUAL: emitBytes(p, c, OP_GREATER, OP_NOT); break;
                    case TOKEN_GREATER: emitByte(p, c, OP_GREATER); break;
                    case TOKEN_GREATER_EQUAL: emitBytes(p, c, OP_LESS, OP_NOT); break;
                    case TOKEN_EQUAL_EQUAL: emitByte(p, c, OP_EQUAL); break;
                    case TOKEN_BANG_EQUAL: emitBytes(p, c, OP_EQUAL, OP_NOT); break;
                    default: break;
                    }
                } else {
                    emitByte(p, c, OP_DUP);
                    parsePrecedence(p, s, c, cc, PREC_COMPARISON);
                    emitByte(p, c, OP_EQUAL);
                }

                if (match(p, s, TOKEN_COMMA) || match(p, s, TOKEN_OR) || match(p, s, TOKEN_AND)) {
                    if (p->previous.type == TOKEN_AND) {
                        int andJump = emitJump(p, c, OP_JUMP_IF_FALSE);
                        emitByte(p, c, OP_POP);

                        if (failCount >= failCapacity) {
                            int oldCap = failCapacity;
                            failCapacity *= 2;
                            failJumps = GROW_ARRAY(c->vm, int, failJumps, oldCap, failCapacity);
                        }
                        failJumps[failCount++] = andJump;
                    } else {
                        int orJump = emitJump(p, c, OP_JUMP_IF_TRUE);
                        emitByte(p, c, OP_POP);

                        if (successCount >= successCapacity) {
                            int oldCap = successCapacity;
                            successCapacity *= 2;
                            successJumps = GROW_ARRAY(c->vm, int, successJumps, oldCap, successCapacity);
                        }
                        successJumps[successCount++] = orJump;
                    }
                } else {
                    break;
                }
            }

            consume(p, s, TOKEN_COLON, "Expect ':' after case condition.");

            int caseJump = emitJump(p, c, OP_JUMP_IF_FALSE);
            emitByte(p, c, OP_POP);

            for (int i = 0; i < successCount; i++) {
                patchJump(p, c, successJumps[i]);
            }
            if (successCount > 0) {
                emitByte(p, c, OP_POP);
            }

            FREE_ARRAY(c->vm, int, successJumps, successCapacity);

            if (switchCtx.caseJumpCount >= switchCtx.caseJumpCapacity) {
                int oldCap = switchCtx.caseJumpCapacity;
                switchCtx.caseJumpCapacity *= 2;
                switchCtx.caseJumps = GROW_ARRAY(c->vm, int, switchCtx.caseJumps, oldCap, switchCtx.caseJumpCapacity);
            }
            switchCtx.caseJumps[switchCtx.caseJumpCount++] = caseJump;

            for (int i = 0; i < failCount; i++) {
                if (switchCtx.caseJumpCount >= switchCtx.caseJumpCapacity) {
                    int oldCap = switchCtx.caseJumpCapacity;
                    switchCtx.caseJumpCapacity *= 2;
                    switchCtx.caseJumps = GROW_ARRAY(c->vm, int, switchCtx.caseJumps, oldCap, switchCtx.caseJumpCapacity);
                }
                switchCtx.caseJumps[switchCtx.caseJumpCount++] = failJumps[i];
            }

            FREE_ARRAY(c->vm, int, failJumps, failCapacity);

            for (int i = 0; i < fallthroughCount; i++) {
                patchJump(p, c, fallthroughJumps[i]);
            }
            fallthroughCount = 0;

            int startBreakCount = switchCtx.breakCount;

            while (!check(p, TOKEN_CASE) && !check(p, TOKEN_DEFAULT) &&
                !check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
                statement(p, s, c, cc);
            }

            lastCaseHadBreak = (switchCtx.breakCount > startBreakCount);

        } else if (match(p, s, TOKEN_DEFAULT)) {
            if (switchCtx.hasDefault) {
                errorAt(p, &p->previous, "Switch can only have one default case.");
            }
            switchCtx.hasDefault = true;

            if (!lastCaseHadBreak) {
                int fallthroughJump = emitJump(p, c, OP_JUMP);
                if (fallthroughCount >= fallthroughCapacity) {
                    int oldCap = fallthroughCapacity;
                    fallthroughCapacity *= 2;
                    fallthroughJumps = GROW_ARRAY(c->vm, int, fallthroughJumps, oldCap, fallthroughCapacity);
                }
                fallthroughJumps[fallthroughCount++] = fallthroughJump;
            }

            if (switchCtx.caseJumpCount > 0) {
                patchJump(p, c, switchCtx.caseJumps[switchCtx.caseJumpCount - 1]);
                emitByte(p, c, OP_POP);
            }

            consume(p, s, TOKEN_COLON, "Expect ':' after 'default'.");

            for (int i = 0; i < fallthroughCount; i++) {
                patchJump(p, c, fallthroughJumps[i]);
            }
            fallthroughCount = 0;

            int startBreakCount = switchCtx.breakCount;

            while (!check(p, TOKEN_CASE) && !check(p, TOKEN_RIGHT_BRACE) &&
                !check(p, TOKEN_EOF)) {
                statement(p, s, c, cc);
            }

            lastCaseHadBreak = (switchCtx.breakCount > startBreakCount);

        } else {
            errorAt(p, &p->current, "Expect 'case' or 'default' in switch body.");
            advance(p, s);
        }
    }

    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after switch body.");

    if (switchCtx.caseJumpCount > 0 && !switchCtx.hasDefault) {
        patchJump(p, c, switchCtx.caseJumps[switchCtx.caseJumpCount - 1]);
        emitByte(p, c, OP_POP);
    }

    for (int i = 0; i < fallthroughCount; i++) {
        patchJump(p, c, fallthroughJumps[i]);
    }

    if (switchCtx.isExpression) {
        for (int i = 0; i < switchCtx.breakCount; i++) {
            patchJump(p, c, switchCtx.breakJumps[i]);
        }

        if (switchCtx.breakCount == 0) {
            emitByte(p, c, OP_NULL);
        }
        emitByte(p, c, OP_SWAP);
        emitByte(p, c, OP_POP);
    } else {
        emitByte(p, c, OP_POP);

        for (int i = 0; i < switchCtx.breakCount; i++) {
            patchJump(p, c, switchCtx.breakJumps[i]);
        }

        if (!isStatement) {
            emitByte(p, c, OP_NULL);
        }
    }

    FREE_ARRAY(c->vm, int, switchCtx.caseJumps, switchCtx.caseJumpCapacity);
    FREE_ARRAY(c->vm, int, switchCtx.breakJumps, switchCtx.breakCapacity);
    FREE_ARRAY(c->vm, int, fallthroughJumps, fallthroughCapacity);
    c->currentSwitch = switchCtx.enclosing;
}

static void forStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    beginScope(c);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
    if (match(p, s, TOKEN_SEMICOLON)) {
    } else if (match(p, s, TOKEN_VAR)) {
        int global = parseVariable(p, s, c, "Expect name.");
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, OP_NULL);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        defineVariable(p, c, global);
    } else {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        emitPopOrRemoveLoad(p, c);
    }

    int loopStart = currentChunk(c)->count;
    int exitJump = -1;
    if (!match(p, s, TOKEN_SEMICOLON)) {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        exitJump = emitJump(p, c, OP_POP_JUMP_IF_FALSE);
    }

    if (!match(p, s, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(p, c, OP_JUMP);
        int incrementStart = currentChunk(c)->count;
        expression(p, s, c, cc);
        emitPopOrRemoveLoad(p, c);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        emitLoop(p, c, loopStart);
        loopStart = incrementStart;
        patchJump(p, c, bodyJump);
    }

    Loop loop = { .enclosing = c->currentLoop, .start = loopStart, .scopeDepth = c->scopeDepth, .breakCount = 0 };
    c->currentLoop = &loop;
    int loopVar = -1;
    for (int i = c->localCount - 1; i >= 0; i--) {
        if (c->locals[i].depth != -1 && c->locals[i].depth == c->scopeDepth) {
            loopVar = i;
            break;
        }
    }
    beginScope(c);
    if (loopVar != -1) {
        emitBytes(p, c, OP_GET_LOCAL, (uint8_t) loopVar);
        Local* shadow = &c->locals[c->localCount++];
        shadow->name = c->locals[loopVar].name;
        shadow->depth = c->scopeDepth;
        shadow->isCaptured = false;
        shadow->isModified = false;
    }
    statement(p, s, c, cc);
    if (loopVar != -1) {
        emitBytes(p, c, OP_GET_LOCAL, (uint8_t) c->localCount - 1);
        emitBytes(p, c, OP_SET_LOCAL, (uint8_t) loopVar);
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

static void returnStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (c->type == TYPE_SCRIPT) {
        errorAt(p, &p->previous, "Can't return from top-level code.");
        return;
    }
    if (match(p, s, TOKEN_SEMICOLON)) {
        emitByte(p, c, OP_NULL);
        emitByte(p, c, OP_RETURN);
    } else {
        if (c->type == TYPE_INITIALIZER) errorAt(p, &p->previous, "Can't return a value from an initializer.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");

        if (c->lastInstruction != -1) {
            Chunk* chunk = currentChunk(c);
            uint8_t* opcode = &chunk->code[c->lastInstruction];
            switch (*opcode) {
            case OP_CALL: *opcode = OP_TAIL_CALL; break;
            case OP_CALL_0: *opcode = OP_TAIL_CALL_0; break;
            case OP_CALL_1: *opcode = OP_TAIL_CALL_1; break;
            case OP_CALL_2: *opcode = OP_TAIL_CALL_2; break;
            case OP_CALL_3: *opcode = OP_TAIL_CALL_3; break;
            case OP_CALL_4: *opcode = OP_TAIL_CALL_4; break;
            case OP_CALL_5: *opcode = OP_TAIL_CALL_5; break;
            case OP_CALL_6: *opcode = OP_TAIL_CALL_6; break;
            case OP_CALL_7: *opcode = OP_TAIL_CALL_7; break;
            case OP_CALL_8: *opcode = OP_TAIL_CALL_8; break;
            case OP_INVOKE: *opcode = OP_TAIL_INVOKE; break;
            case OP_INVOKE_LONG: *opcode = OP_TAIL_INVOKE_LONG; break;
            case OP_INVOKE_0: *opcode = OP_TAIL_INVOKE_0; break;
            case OP_INVOKE_1: *opcode = OP_TAIL_INVOKE_1; break;
            case OP_INVOKE_2: *opcode = OP_TAIL_INVOKE_2; break;
            case OP_INVOKE_3: *opcode = OP_TAIL_INVOKE_3; break;
            case OP_INVOKE_4: *opcode = OP_TAIL_INVOKE_4; break;
            case OP_INVOKE_5: *opcode = OP_TAIL_INVOKE_5; break;
            case OP_INVOKE_6: *opcode = OP_TAIL_INVOKE_6; break;
            case OP_INVOKE_7: *opcode = OP_TAIL_INVOKE_7; break;
            case OP_INVOKE_8: *opcode = OP_TAIL_INVOKE_8; break;
            case OP_INVOKE_IC: *opcode = OP_TAIL_INVOKE_IC; break;  // NEW
            case OP_SUPER_INVOKE: *opcode = OP_TAIL_SUPER_INVOKE; break;
            case OP_SUPER_INVOKE_LONG: *opcode = OP_TAIL_SUPER_INVOKE_LONG; break;
            case OP_SUPER_INVOKE_0: *opcode = OP_TAIL_SUPER_INVOKE_0; break;
            case OP_SUPER_INVOKE_1: *opcode = OP_TAIL_SUPER_INVOKE_1; break;
            case OP_SUPER_INVOKE_2: *opcode = OP_TAIL_SUPER_INVOKE_2; break;
            case OP_SUPER_INVOKE_3: *opcode = OP_TAIL_SUPER_INVOKE_3; break;
            case OP_SUPER_INVOKE_4: *opcode = OP_TAIL_SUPER_INVOKE_4; break;
            case OP_SUPER_INVOKE_5: *opcode = OP_TAIL_SUPER_INVOKE_5; break;
            case OP_SUPER_INVOKE_6: *opcode = OP_TAIL_SUPER_INVOKE_6; break;
            case OP_SUPER_INVOKE_7: *opcode = OP_TAIL_SUPER_INVOKE_7; break;
            case OP_SUPER_INVOKE_8: *opcode = OP_TAIL_SUPER_INVOKE_8; break;
            default: break;
            }
        }
        emitByte(p, c, OP_RETURN);
    }
}

static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_SWITCH)) {
        switchStatement(p, s, c, cc, true);
    } else if (match(p, s, TOKEN_FOR)) {
        forStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_IF)) {
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
        int thenJump = emitFusedJump(p, c, OP_POP_JUMP_IF_FALSE);
        statement(p, s, c, cc);
        int elseJump = emitJump(p, c, OP_JUMP);
        patchJump(p, c, thenJump);
        if (match(p, s, TOKEN_ELSE)) statement(p, s, c, cc);
        patchJump(p, c, elseJump);
    } else if (match(p, s, TOKEN_RETURN)) {
        returnStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_WHILE)) {
        int start = currentChunk(c)->count;
        Loop loop = { .enclosing = c->currentLoop, .start = start, .scopeDepth = c->scopeDepth, .breakCount = 0 };
        c->currentLoop = &loop;
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int exitJ = emitFusedJump(p, c, OP_POP_JUMP_IF_FALSE);
        statement(p, s, c, cc);
        emitLoop(p, c, start);
        patchJump(p, c, exitJ);
        for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
        c->currentLoop = loop.enclosing;
    } else if (match(p, s, TOKEN_BREAK)) {
        bool hasValue = !check(p, TOKEN_SEMICOLON);

        if (hasValue) {
            if (c->currentSwitch == NULL) {
                errorAt(p, &p->previous, "Can only use 'break <value>' in switch.");
                return;
            }
            c->currentSwitch->isExpression = true;
            expression(p, s, c, cc);
        }

        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after break.");

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
                emitByte(p, c, OP_CLOSE_UPVALUE);
            } else {
                if (!hasValue || c->currentLoop) {  // Only pop if not break with value in switch
                    popCount++;
                }
            }
        }

        if (popCount > 0) emitPopN(p, c, popCount);

        int jump = emitJump(p, c, OP_JUMP);

        if (c->currentLoop) {
            c->currentLoop->breakJumps[c->currentLoop->breakCount++] = jump;
        } else {
            if (c->currentSwitch->breakCount >= c->currentSwitch->breakCapacity) {
                int oldCap = c->currentSwitch->breakCapacity;
                c->currentSwitch->breakCapacity *= 2;
                c->currentSwitch->breakJumps = GROW_ARRAY(c->vm, int,
                    c->currentSwitch->breakJumps,
                    oldCap,
                    c->currentSwitch->breakCapacity);
            }
            c->currentSwitch->breakJumps[c->currentSwitch->breakCount++] = jump;
        }

        // DON'T decrement c->localCount here - the switch end will handle it!
    } else if (match(p, s, TOKEN_LEFT_BRACE)) {
        beginScope(c);
        block(p, s, c, cc);
        endScope(p, c);
    } else {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        Chunk* chunk = currentChunk(c);
        if (chunk->count > 0 && c->lastInstruction >= 0) {
            uint8_t lastOp = chunk->code[c->lastInstruction];  // Use lastInstruction, not count-1
            if (lastOp == OP_POP) return;
            if (lastOp >= OP_SET_LOCAL_0 && lastOp <= OP_SET_LOCAL_7) {
                chunk->code[c->lastInstruction] = lastOp + (OP_SET_LOCAL_0_POP - OP_SET_LOCAL_0);
                return;
            }
            if (lastOp == OP_INC_LOCAL) {
                chunk->code[c->lastInstruction] = OP_INC_LOCAL_POP;
                return;
            }
        }
        emitPopOrRemoveLoad(p, c);
    }
}

static void defineVariable(Parser* p, Compiler* c, int global) {
    if (c->scopeDepth > 0) {
        markInitialized(c);
        return;
    }
    emitLong(p, c, OP_DEFINE_GLOBAL, OP_DEFINE_GLOBAL_LONG, global);
}
static int parseVariable(Parser* p, Scanner* s, Compiler* c, const char* msg) {
    consume(p, s, TOKEN_IDENTIFIER, msg);
    declareVariable(p, c);
    if (c->scopeDepth > 0) return 0;
    return identifierConstant(c, &p->previous);
}
static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_CLASS)) {
        classDeclaration(p, s, c, cc);
    } else if (match(p, s, TOKEN_FUNC)) {
        funDeclaration(p, s, c, cc);
    } else if (match(p, s, TOKEN_VAR)) {
        do {
            int global = parseVariable(p, s, c, "Expect name.");
            if (match(p, s, TOKEN_EQUAL)) {
                expression(p, s, c, cc);
            } else {
                emitByte(p, c, OP_NULL);
            }
            defineVariable(p, c, global);
        } while (match(p, s, TOKEN_COMMA));
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
    } else if (match(p, s, TOKEN_IMPORT)) {
        consume(p, s, TOKEN_STRING, "Expect filename.");
        Token pathToken = p->previous;
        int file = makeConstant(p, c, OBJ_VAL(copyString(c->vm, pathToken.start + 1, pathToken.length - 2)));
        int alias;
        if (match(p, s, TOKEN_AS)) {
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
            Token nameToken = { .start = filename, .length = nameLength };
            declareVariable(p, c);
            alias = identifierConstant(c, &nameToken);
        }
        emitLong(p, c, OP_IMPORT, OP_IMPORT_LONG, file);
        defineVariable(p, c, alias);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after import.");
    } else {
        statement(p, s, c, cc);
    }
    if (p->panicMode) {
        advance(p, s);
        while (p->current.type != TOKEN_EOF) {
            if (p->previous.type == TOKEN_SEMICOLON) {
                p->panicMode = false;
                return;
            }
            switch (p->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUNC:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                p->panicMode = false;
                return;
            default:;
            }
            advance(p, s);
        }
    }
}
ObjFunction* compile(struct VM* vm, ObjModule* module, const char* source) {
    Scanner s;
    initScanner(&s, source);
    Parser p = { .vm = vm, .hadError = false, .panicMode = false };
    Compiler c;
    initCompiler(&p, &c, NULL, TYPE_SCRIPT, module);
    advance(&p, &s);
    while (!match(&p, &s, TOKEN_EOF)) declaration(&p, &s, &c, NULL);
    ObjFunction* function = endCompiler(&p, &c);
    return p.hadError ? NULL : function;
}
void markCompilerRoots(struct VM* vm) {
    Compiler* c = (Compiler*) vm->compiler;
    while (c) {
        markObject(vm, (Obj*) c->function);
        markTable(vm, &c->constants);
        c = c->enclosing;
    }
}