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
static int parseVariable(Parser* p, Scanner* s, Compiler* c, const char* errorMessage);
static void defineVariable(Parser* p, Compiler* c, int global);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence);
static void emitLong(Parser* p, Compiler* c, OpCode shortOp, OpCode longOp, uint32_t index);
static int makeConstant(Parser* p, Compiler* c, Value value);
static void emitConstant(Parser* p, Compiler* c, Value value);
static int resolveLocal(Parser* p, Compiler* c, Token* name);
static int resolveUpvalue(Parser* p, Compiler* c, Token* name);

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
    //Chunk* chunk = currentChunk(c);
    /*
    if ((byte1 == OP_GET_LOCAL || byte1 == OP_GET_UPVALUE) &&
        c->lastInstruction >= 0 && c->lastInstruction + 2 == chunk->count) {
        uint8_t prevOp = chunk->code[c->lastInstruction];
        uint8_t prevOperand = chunk->code[c->lastInstruction + 1];
        if (byte1 == OP_GET_LOCAL && prevOp == OP_SET_LOCAL && prevOperand == byte2) return;
        if (byte1 == OP_GET_UPVALUE && prevOp == OP_SET_UPVALUE && prevOperand == byte2) return;
    }*/

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
    token.line = 0; // Synthetic tokens don't have a real line
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
    local->depth = -1; // Declared but uninitialized
    local->isCaptured = false;
    local->isModified = false;
}

// --- Back-Patching Logic ---

static void markLocalAsModified(Compiler* c, int localIndex) {
    c->locals[localIndex].isModified = true;
    for (int i = 0; i < c->patchCount; i++) {
        if (c->patches[i].localIndex == localIndex) {
            int offset = c->patches[i].codeOffset;
            currentChunk(c)->code[offset] = 1; // Flip to Mutable
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
    // Don't pop if the last instruction was a return
    if (c->lastInstruction >= 0 && c->lastInstruction < chunk->count) {
        uint8_t prevOp = chunk->code[c->lastInstruction];
        if (prevOp == OP_RETURN) return;
    }
    // why would we go to all this trouble? because we want to avoid
    // generating unnecessary bytecode for loading a local/upvalue
    // only to immediately pop it
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
    LastInstruction result = { .isConstant = false, .value = NIL_VAL, .length = 0 };
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

    // Look at the very last byte emitted
    if (chunk->count > 0) {
        uint8_t lastOp = chunk->code[chunk->count - 1];
        uint8_t fusedOp = 0;

        // Map standard comparisons to their "Jump if False" equivalents
        switch (lastOp) {
        case OP_EQUAL:   fusedOp = OP_JUMP_IF_NOT_EQUAL; break;
        case OP_GREATER: fusedOp = OP_JUMP_IF_NOT_GREATER; break;
        case OP_LESS:    fusedOp = OP_JUMP_IF_NOT_LESS; break;
            // For != (which is OP_EQUAL + OP_NOT), check 2 bytes
        case OP_NOT:
            if (chunk->count > 1 && chunk->code[chunk->count - 2] == OP_EQUAL) {
                removeChunkTail(chunk, 1); // Remove OP_NOT
                lastOp = OP_EQUAL;
                fusedOp = OP_JUMP_IF_EQUAL;
            }
            break;
        }

        if (fusedOp != 0) {
            removeChunkTail(chunk, 1); // Remove the original OP_EQUAL/LESS/etc
            return emitJump(p, c, fusedOp);
        }
    }

    // Fallback to standard popping jump if no comparison was found
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
        emitByte(p, c, OP_NIL);
    }
    emitByte(p, c, OP_RETURN);
    ObjFunction* function = c->function;
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
                emitPopN(p, c, popCount); popCount = 0;
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
        pop(c->vm); return (int) AS_NUMBER(indexValue);
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
        // Optimistic: Assume immutable unless already known to be modified.
        // We will back-patch later if it becomes modified.
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
        } else {
            if (arg <= 7) emitByte(p, c, (uint8_t) (OP_GET_LOCAL_0 + arg));
            else emitBytes(p, c, OP_GET_LOCAL, (uint8_t) arg);
        }
        return; // CRITICAL: Stop here!
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
        } else {
            emitUpvalue(p, c, (uint8_t) arg, false);
        }
        return; // CRITICAL: Stop here!
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
            } else {
                emitBytes(p, c, OP_GET_FIELD_THIS, index);
            }
            return; // Found a field, stop here!
        }
    }

    // --- GLOBAL VARIABLE ---
    // If it's not a local, upvalue, or field, it MUST be a global.
    arg = identifierConstant(c, &name);
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
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
    Local* l = &c->locals[c->localCount++];
    l->name = *name;
    l->depth = -1;
    l->isCaptured = false;
    l->isModified = false;
}

static void func(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign; function(p, s, c, cc, TYPE_FUNCTION);
}

// ... (binary, unary, literal, grouping, number, string, variable, list, subscript same as before) ...
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
            case TOKEN_SLASH:   if (b == 0) {
                errorAt(p, &p->previous, "Division by zero."); return;
            } res = a / b; break;
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
    case TOKEN_NIL:   emitByte(p, c, OP_NIL); break;
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
    namedVariable(p, s, c, cc, p->previous, canAssign);
}

static void list(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int count = 0;
    if (!check(p, TOKEN_RIGHT_BRACKET)) {
        do {
            expression(p, s, c, cc);
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
    } else emitByte(p, c, OP_INDEX_GET);
}

static void dot(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'.");
    Token name = p->previous;
    ObjString* fieldName = copyString(c->vm, name.start, name.length);

    // Check if receiver is 'this'
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

            if (args <= 8 && nameIdx < 256) {
                emitBytes(p, c, (uint8_t) (OP_INVOKE_0 + args), 0xFF);
                writeChunk(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
            } else if (nameIdx < 256) {
                emitByte(p, c, OP_INVOKE);
                writeChunk(c->vm, currentChunk(c), 0xFF, p->previous.line);
                writeChunk(c->vm, currentChunk(c), (uint8_t) nameIdx, p->previous.line);
                writeChunk(c->vm, currentChunk(c), (uint8_t) args, p->previous.line);
            } else {
                emitByte(p, c, OP_INVOKE_LONG);
                writeChunk(c->vm, currentChunk(c), 0xFF, p->previous.line);
                writeChunk(c->vm, currentChunk(c), 0xFF, p->previous.line);
                writeChunk(c->vm, currentChunk(c), (uint8_t) (nameIdx & 0xff), p->previous.line);
                writeChunk(c->vm, currentChunk(c), (uint8_t) ((nameIdx >> 8) & 0xff), p->previous.line);
                writeChunk(c->vm, currentChunk(c), (uint8_t) args, p->previous.line);
            }
            return;

        } else {
            // Polymorphic call - GET_PROPERTY first, THEN args, THEN call
            int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));
            emitLong(p, c, OP_GET_PROPERTY, OP_GET_PROPERTY_LONG, nameIdx);

            // NOW parse arguments
            uint8_t args = 0;
            if (!check(p, TOKEN_RIGHT_PAREN)) {
                do {
                    expression(p, s, c, cc);
                    args++;
                } while (match(p, s, TOKEN_COMMA));
            }
            consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");

            // Now call the bound method/function
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
    } else {
        // Property access
        int nameIdx = makeConstant(p, c, OBJ_VAL(fieldName));
        if (canAssign && match(p, s, TOKEN_EQUAL)) {
            expression(p, s, c, cc);
            emitLong(p, c, OP_SET_PROPERTY, OP_SET_PROPERTY_LONG, nameIdx);
        } else {
            emitLong(p, c, OP_GET_PROPERTY, OP_GET_PROPERTY_LONG, nameIdx);
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
            bool isFalsey = IS_NIL(operand.value) || (IS_BOOL(operand.value) && !AS_BOOL(operand.value));
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
    /*int elseJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    int endJump = emitJump(p, c, OP_JUMP);
    patchJump(p, c, elseJump);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, endJump);*/

    // The left operand is on the stack.
    // If it is truthy, we short-circuit: jump over the right operand
    // and keep the current value on the stack.
    int elseJump = emitJump(p, c, OP_JUMP_IF_TRUE);

    // If we didn't jump, the left side was false. 
    // Pop it and evaluate the right side.
    //emitByte(p, c, OP_POP);
    emitPopOrRemoveLoad(p, c);
    parsePrecedence(p, s, c, cc, PREC_OR);

    patchJump(p, c, elseJump);
}

static void call(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    uint8_t args = 0;
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            expression(p, s, c, cc); args++;
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
        errorAt(p, &p->previous, "Can't use 'this' outside of a class."); return;
    }
    variable(p, s, c, cc, false);
}

static void super_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    if (cc == NULL) errorAt(p, &p->previous, "Can't use 'super' outside of a class.");
    else if (!cc->hasSuperclass) errorAt(p, &p->previous, "Can't use 'super' in a class with no superclass.");

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
                expression(p, s, c, cc); args++;
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

ParseRule rules [] = {
  [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
  [TOKEN_LEFT_BRACKET] = {list, subscript, PREC_CALL},
  [TOKEN_DOT] = {NULL, dot, PREC_CALL},
  [TOKEN_MINUS] = {unary, binary, PREC_TERM},
  [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
  [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
  [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
  [TOKEN_PERCENT] = {NULL, binary, PREC_FACTOR},
  [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
  [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
  [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
  [TOKEN_STRING] = {string, NULL, PREC_NONE},
  [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
  [TOKEN_AND] = {NULL, and_, PREC_AND},
  [TOKEN_OR] = {NULL, or_, PREC_OR},
  [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
  [TOKEN_NIL] = {literal, NULL, PREC_NONE},
  [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
  [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
  [TOKEN_THIS] = {this_, NULL, PREC_NONE},
  [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
  [TOKEN_FUN] = {func, NULL,   PREC_NONE},
};

static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence prec) {
    advance(p, s);
    ParseFn prefix = getRule(p->previous.type)->prefix;
    if (!prefix) {
        errorAt(p, &p->previous, "Expect expression."); return;
    }
    bool canAssign = prec <= PREC_ASSIGNMENT;
    prefix(p, s, c, cc, canAssign);
    while (prec <= getRule(p->current.type)->precedence) {
        advance(p, s);
        ParseFn infix = getRule(p->previous.type)->infix;
        infix(p, s, c, cc, canAssign);
    }
    if (canAssign && match(p, s, TOKEN_EQUAL)) errorAt(p, &p->previous, "Invalid assignment target.");
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}
static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    parsePrecedence(p, s, c, cc, PREC_ASSIGNMENT);
}

static void block(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) declaration(p, s, c, cc);
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

    // Emit Upvalue Info + Mutability Flag
    for (int i = 0; i < f->upvalueCount; i++) {
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].isLocal ? 1 : 0, p->previous.line);
        writeChunk(c->vm, currentChunk(c), sub.upvalues[i].index, p->previous.line);

        bool isMut = sub.upvalues[i].isMutable;

        // CRITICAL FIX: If local variable is known to be modified, force mutable.
        // Otherwise, track for back-patching.
        if (sub.upvalues[i].isLocal) {
            if (c->locals[sub.upvalues[i].index].isModified) {
                isMut = true;
            } else if (!isMut) {
                // Record the location of this byte so we can flip it to 1 later if needed
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
    if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
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
    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);

    push(c->vm, OBJ_VAL(f));
    int fnIdx = makeConstant(p, c, OBJ_VAL(f));
    emitLong(p, c, OP_CLOSURE, OP_CLOSURE_LONG, fnIdx);
    pop(c->vm);

    // Emit upvalue info (unchanged)
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

    // Create signature and assign/lookup method index
    ObjString* signature = createMethodSignature(c, &nameToken, f->arity);
    push(c->vm, OBJ_VAL(signature));

    Value indexValue;
    int methodIndex;
    if (tableGet(&cc->methodIndices, OBJ_VAL(signature), &indexValue)) {
        // Method override - use existing index
        methodIndex = (int) AS_NUMBER(indexValue);
    } else {
        // New method - assign next index
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

    // Initialize method tracking
    initTable(&classC.methodIndices);
    classC.nextMethodIndex = 0;

    if (match(p, s, TOKEN_LESS)) {
        consume(p, s, TOKEN_IDENTIFIER, "Expect superclass name.");
        Token superClassName = p->previous;

        // Look up parent's compilation info from module
        ObjString* parentName = copyString(c->vm, superClassName.start, superClassName.length);
        push(c->vm, OBJ_VAL(parentName));

        Value savedIndicesValue;
        if (tableGet(&c->module->classInfo, OBJ_VAL(parentName), &savedIndicesValue)) {
            // Retrieve the saved table pointer
            Table* parentIndices = (Table*) (uintptr_t) AS_NUMBER(savedIndicesValue);

            // Copy parent's methodIndices to child
            tableAddAll(c->vm, parentIndices, &classC.methodIndices);

            // Find max parent index
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
            consume(p, s, TOKEN_IDENTIFIER, "Expect variable name.");
            Token fieldName = p->previous;
            consume(p, s, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
            ObjString* name = copyString(c->vm, fieldName.start, fieldName.length);
            Value dummy;
            if (!tableGet(&classC.fields, OBJ_VAL(name), &dummy)) {
                tableSet(c->vm, &classC.fields, OBJ_VAL(name), NUMBER_VAL((double) classC.fieldCount++));
            }
        } else {
            method(p, s, c, &classC);
        }
    }
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

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
    // Save class compilation info for child classes to use
    ObjString* className = copyString(c->vm, nameToken.start, nameToken.length);
    push(c->vm, OBJ_VAL(className));

    // Create a copy of the methodIndices table
    Table* savedIndices = ALLOCATE(c->vm, Table, 1);
    initTable(savedIndices);
    tableAddAll(c->vm, &classC.methodIndices, savedIndices);

    // Store it in the module's classInfo table as a pointer
    // (We'll use a number to store the pointer)
    tableSet(c->vm, &c->module->classInfo, OBJ_VAL(className),
        NUMBER_VAL((double) (uintptr_t) savedIndices));

    pop(c->vm);
    freeTable(c->vm, &classC.methodIndices);
}

static void funDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    int global = parseVariable(p, s, c, "Expect function name.");
    markInitialized(c);
    function(p, s, c, cc, TYPE_FUNCTION);
    defineVariable(p, c, global);
}

static void forStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    beginScope(c);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
    if (match(p, s, TOKEN_SEMICOLON)) {
    } else if (match(p, s, TOKEN_VAR)) {
        int global = parseVariable(p, s, c, "Expect name.");
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, OP_NIL);
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
        //emitPopOrRemoveLoad(p, c);
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
            loopVar = i; break;
        }
    }
    beginScope(c);
    if (loopVar != -1) {
        emitBytes(p, c, OP_GET_LOCAL, (uint8_t) loopVar);
        Local* shadow = &c->locals[c->localCount++];
        shadow->name = c->locals[loopVar].name; shadow->depth = c->scopeDepth; shadow->isCaptured = false; shadow->isModified = false;
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
        //emitPopOrRemoveLoad(p, c);
    }
    for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
    c->currentLoop = loop.enclosing;
    endScope(p, c);
}

static void returnStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (c->type == TYPE_SCRIPT) {
        errorAt(p, &p->previous, "Can't return from top-level code."); return;
    }
    if (match(p, s, TOKEN_SEMICOLON)) {
        emitByte(p, c, OP_NIL); emitByte(p, c, OP_RETURN);
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
    if (match(p, s, TOKEN_PRINT)) {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        emitByte(p, c, OP_PRINT);
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
        if (c->currentLoop == NULL) errorAt(p, &p->previous, "Lonely break.");
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        int i = c->localCount - 1;
        int popCount = 0;
        while (i >= 0 && c->locals[i].depth > c->currentLoop->scopeDepth) {
            if (c->locals[i].isCaptured) {
                if (popCount > 0) {
                    emitPopN(p, c, (unsigned int) popCount); popCount = 0;
                }
                emitByte(p, c, OP_CLOSE_UPVALUE);
            } else popCount++;
            i--; c->localCount--;
        }
        if (popCount > 0) emitPopN(p, c, (unsigned int) popCount);
        int jump = emitJump(p, c, OP_JUMP);
        c->currentLoop->breakJumps[c->currentLoop->breakCount++] = jump;
    } else if (match(p, s, TOKEN_LEFT_BRACE)) {
        beginScope(c);
        block(p, s, c, cc);
        endScope(p, c);
    } else {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        Chunk* chunk = currentChunk(c);
        if (chunk->count > 0) {
            uint8_t lastOp = chunk->code[chunk->count - 1];
            // Optimize Specialized Locals (0-7)
            if (lastOp >= OP_SET_LOCAL_0 && lastOp <= OP_SET_LOCAL_7) {
                // Mutate OP_SET_LOCAL_n to OP_SET_LOCAL_n_POP
                chunk->code[chunk->count - 1] = lastOp + (OP_SET_LOCAL_0_POP - OP_SET_LOCAL_0);
                return;
            }
            if (lastOp == OP_INC_LOCAL) {
                chunk->code[chunk->count - 1] = OP_INC_LOCAL_POP;
                return;
            }
        }

        emitPopOrRemoveLoad(p, c);
    }
}

static void defineVariable(Parser* p, Compiler* c, int global) {
    if (c->scopeDepth > 0) {
        markInitialized(c); return;
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
    if (match(p, s, TOKEN_CLASS)) classDeclaration(p, s, c, cc);
    else if (match(p, s, TOKEN_FUN)) funDeclaration(p, s, c, cc);
    else if (match(p, s, TOKEN_VAR)) {
        int global = parseVariable(p, s, c, "Expect name.");
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, OP_NIL);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        defineVariable(p, c, global);
    } else if (match(p, s, TOKEN_IMPORT)) {
        consume(p, s, TOKEN_STRING, "Expect filename.");
        Token pathToken = p->previous;
        int file = makeConstant(p, c, OBJ_VAL(copyString(c->vm, pathToken.start + 1, pathToken.length - 2)));
        int alias;
        if (match(p, s, TOKEN_AS)) alias = parseVariable(p, s, c, "Expect variable name after 'as'.");
        else {
            const char* path = pathToken.start + 1; int length = pathToken.length - 2;
            const char* filename = path;
            for (int i = 0; i < length; i++) if (path[i] == '/' || path[i] == '\\') filename = path + i + 1;
            int nameLength = (int) (path + length - filename);
            for (int i = 0; i < nameLength; i++) if (filename[i] == '.') {
                nameLength = i; break;
            }
            Token nameToken = { .start = filename, .length = nameLength };
            declareVariable(p, c); alias = identifierConstant(c, &nameToken);
        }
        emitLong(p, c, OP_IMPORT, OP_IMPORT_LONG, file); defineVariable(p, c, alias);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after import.");
    } else statement(p, s, c, cc);

    if (p->panicMode) {
        advance(p, s);
        while (p->current.type != TOKEN_EOF) {
            if (p->previous.type == TOKEN_SEMICOLON) {
                p->panicMode = false; return;
            }
            switch (p->current.type) {
            case TOKEN_CLASS: case TOKEN_FUN: case TOKEN_VAR: case TOKEN_FOR:
            case TOKEN_IF: case TOKEN_WHILE: case TOKEN_PRINT: case TOKEN_RETURN: p->panicMode = false; return;
            default:;
            }
            advance(p, s);
        }
    }
}

ObjFunction* compile(struct VM* vm, ObjModule* module, const char* source) {
    Scanner s; initScanner(&s, source);
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