#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
    VM* vm;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
    VM* vm;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

typedef void (*ParseFn)(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static Chunk* currentChunk(Compiler* c) {
    return &c->function->chunk;
}

static void errorAt(Parser* p, Token* token, const char* message) {
    if (p->panicMode) return;
    p->panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    p->hadError = true;
}

static void error(Parser* p, const char* message) {
    errorAt(p, &p->previous, message);
}

static void errorAtCurrent(Parser* p, const char* message) {
    errorAt(p, &p->current, message);
}

static void advance(Parser* p, Scanner* s) {
    p->previous = p->current;

    for (;;) {
        p->current = scanToken(s);
        if (p->current.type != TOKEN_ERROR) break;

        errorAtCurrent(p, p->current.start);
    }
}

static void consume(Parser* p, Scanner* s, TokenType type, const char* message) {
    if (p->current.type == type) {
        advance(p, s);
        return;
    }

    errorAtCurrent(p, message);
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
    writeChunk(c->vm, currentChunk(c), byte, p->previous.line);
}

static void emitBytes(Parser* p, Compiler* c, uint8_t byte1, uint8_t byte2) {
    emitByte(p, c, byte1);
    emitByte(p, c, byte2);
}

static void emitLoop(Parser* p, Compiler* c, int loopStart) {
    emitByte(p, c, OP_LOOP);

    int offset = currentChunk(c)->count - loopStart + 2;
    if (offset > UINT16_MAX) error(p, "Loop body too large.");

    emitByte(p, c, (offset >> 8) & 0xff);
    emitByte(p, c, offset & 0xff);
}

static int emitJump(Parser* p, Compiler* c, uint8_t instruction) {
    emitByte(p, c, instruction);
    emitByte(p, c, 0xff);
    emitByte(p, c, 0xff);
    return currentChunk(c)->count - 2;
}

static void emitReturn(Parser* p, Compiler* c) {
    if (c->type == TYPE_INITIALIZER) {
        emitBytes(p, c, OP_GET_LOCAL, 0);
    } else {
        emitByte(p, c, OP_NIL);
    }

    emitByte(p, c, OP_RETURN);
}

static uint8_t makeConstant(Parser* p, Compiler* c, Value value) {
    int constant = addConstant(c->vm, currentChunk(c), value);
    if (constant > UINT8_MAX) {
        error(p, "Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t) constant;
}

static void emitConstant(Parser* p, Compiler* c, Value value) {
    emitBytes(p, c, OP_CONSTANT, makeConstant(p, c, value));
}

static void patchJump(Parser* p, Compiler* c, int offset) {
    int jump = currentChunk(c)->count - offset - 2;

    if (jump > UINT16_MAX) {
        error(p, "Too much code to jump over.");
    }

    currentChunk(c)->code[offset] = (jump >> 8) & 0xff;
    currentChunk(c)->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Parser* p, Compiler* c, Compiler* enclosing, FunctionType type) {
    c->enclosing = enclosing;
    c->function = NULL;
    c->type = type;
    c->localCount = 0;
    c->scopeDepth = 0;
    c->vm = p->vm;
    c->vm->compiler = (void*) c;
    c->function = newFunction(p->vm);

    if (type != TYPE_SCRIPT) {
        c->function->name = copyString(p->vm, p->previous.start, p->previous.length);
    }

    Local* local = &c->locals[c->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_SCRIPT) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler(Parser* p, Compiler* c) {
    emitReturn(p, c);
    ObjFunction* function = c->function;

#ifdef DEBUG_PRINT_CODE
    if (!p->hadError) {
        disassembleChunk(currentChunk(c), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    c->vm->compiler = (void*) c->enclosing;
    return function;
}

static void beginScope(Compiler* c) {
    c->scopeDepth++;
}

static void endScope(Parser* p, Compiler* c) {
    c->scopeDepth--;

    while (c->localCount > 0 &&
        c->locals[c->localCount - 1].depth > c->scopeDepth) {
        if (c->locals[c->localCount - 1].isCaptured) {
            emitByte(p, c, OP_CLOSE_UPVALUE);
        } else {
            emitByte(p, c, OP_POP);
        }
        c->localCount--;
    }
}

static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence);

static uint8_t identifierConstant(Parser* p, Compiler* c, Token* name) {
    ObjString* nameString = copyString(c->vm, name->start, name->length);
    Value indexValue;

    // Check for Fast Global index
    if (tableGet(&c->vm->globalNames, nameString, &indexValue)) {
        return (uint8_t) AS_NUMBER(indexValue);
    }

    int index = c->vm->globalValues.count;
    if (index > 255) {
        errorAt(p, name, "Too many global variables in VM.");
        return 0;
    }

    writeValueArray(c->vm, &c->vm->globalValues, EMPTY_VAL);
    tableSet(c->vm, &c->vm->globalNames, nameString, NUMBER_VAL((double) index));
    return (uint8_t) index;
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Parser* p, Compiler* c, Token* name) {
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* local = &c->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                errorAt(p, name, "Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Parser* p, Compiler* c, uint8_t index, bool isLocal) {
    int upvalueCount = c->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &c->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        errorAt(p, &p->previous, "Too many closure variables in function.");
        return 0;
    }

    c->upvalues[upvalueCount].isLocal = isLocal;
    c->upvalues[upvalueCount].index = index;
    return c->function->upvalueCount++;
}

static int resolveUpvalue(Parser* p, Compiler* c, Token* name) {
    if (c->enclosing == NULL) return -1;

    int local = resolveLocal(p, c->enclosing, name);
    if (local != -1) {
        c->enclosing->locals[local].isCaptured = true;
        return addUpvalue(p, c, (uint8_t) local, true);
    }

    int upvalue = resolveUpvalue(p, c->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(p, c, (uint8_t) upvalue, false);
    }

    return -1;
}

static void addLocal(Parser* p, Compiler* c, Token name) {
    if (c->localCount == UINT8_COUNT) {
        errorAt(p, &name, "Too many local variables in function.");
        return;
    }

    Local* local = &c->locals[c->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable(Parser* p, Compiler* c) {
    if (c->scopeDepth == 0) return;

    Token* name = &p->previous;
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* local = &c->locals[i];
        if (local->depth != -1 && local->depth < c->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            errorAt(p, name, "Already a variable with this name in this scope.");
        }
    }

    addLocal(p, c, *name);
}

static uint8_t parseVariable(Parser* p, Scanner* s, Compiler* c, const char* errorMessage) {
    consume(p, s, TOKEN_IDENTIFIER, errorMessage);

    declareVariable(p, c);
    if (c->scopeDepth > 0) return 0;

    return identifierConstant(p, c, &p->previous);
}

static void markInitialized(Compiler* c) {
    if (c->scopeDepth == 0) return;
    c->locals[c->localCount - 1].depth = c->scopeDepth;
}

static void defineVariable(Parser* p, Compiler* c, uint8_t global) {
    if (c->scopeDepth > 0) {
        markInitialized(c);
        return;
    }

    emitBytes(p, c, OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    uint8_t argCount = 0;
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            expression(p, s, c, cc);
            if (argCount == 255) {
                errorAt(p, &p->previous, "Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    int endJump = emitJump(p, c, OP_JUMP_IF_FALSE);

    emitByte(p, c, OP_POP);
    parsePrecedence(p, s, c, cc, PREC_AND);

    patchJump(p, c, endJump);
}

static void binary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    TokenType operatorType = p->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(p, s, c, cc, (Precedence) (rule->precedence + 1));

    switch (operatorType) {
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
    default: return;
    }
}

static void call(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    uint8_t argCount = argumentList(p, s, c, cc);
    emitBytes(p, c, OP_CALL, argCount);
}

static void dot(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(p, c, &p->previous);

    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitBytes(p, c, OP_SET_PROPERTY, name);
    } else if (match(p, s, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(p, s, c, cc);
        emitBytes(p, c, OP_INVOKE, name);
        emitByte(p, c, argCount);
    } else {
        emitBytes(p, c, OP_GET_PROPERTY, name);
    }
}

static void literal(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    switch (p->previous.type) {
    case TOKEN_FALSE: emitByte(p, c, OP_FALSE); break;
    case TOKEN_NIL:   emitByte(p, c, OP_NIL); break;
    case TOKEN_TRUE:  emitByte(p, c, OP_TRUE); break;
    default: return;
    }
}

static void grouping(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    double value = strtod(p->previous.start, NULL);
    emitConstant(p, c, NUMBER_VAL(value));
}

static void or_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    int elseJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    int endJump = emitJump(p, c, OP_JUMP);

    patchJump(p, c, elseJump);
    emitByte(p, c, OP_POP);

    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, endJump);
}

static void string(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    emitConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start + 1, p->previous.length - 2)));
}

static void namedVariable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(p, c, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(p, c, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(p, c, &name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitBytes(p, c, setOp, (uint8_t) arg);
    } else {
        emitBytes(p, c, getOp, (uint8_t) arg);
    }
}

static void variable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    namedVariable(p, s, c, cc, p->previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int) strlen(text);
    return token;
}

static void super_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'super' outside of a class.");
    } else if (!cc->hasSuperclass) {
        errorAt(p, &p->previous, "Can't use 'super' in a class with no superclass.");
    }

    consume(p, s, TOKEN_DOT, "Expect '.' after 'super'.");
    consume(p, s, TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(p, c, &p->previous);

    namedVariable(p, s, c, cc, syntheticToken("this"), false);
    if (match(p, s, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(p, s, c, cc);
        namedVariable(p, s, c, cc, syntheticToken("super"), false);
        emitBytes(p, c, OP_SUPER_INVOKE, name);
        emitByte(p, c, argCount);
    } else {
        namedVariable(p, s, c, cc, syntheticToken("super"), false);
        emitBytes(p, c, OP_GET_SUPER, name);
    }
}

static void this_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'this' outside of a class.");
        return;
    }

    variable(p, s, c, cc, false);
}

static void unary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    TokenType operatorType = p->previous.type;

    parsePrecedence(p, s, c, cc, PREC_UNARY);

    switch (operatorType) {
    case TOKEN_BANG:  emitByte(p, c, OP_NOT); break;
    case TOKEN_MINUS: emitByte(p, c, OP_NEGATE); break;
    default: return;
    }
}

ParseRule rules [] = {
  [TOKEN_LEFT_PAREN] = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT] = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS] = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS] = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH] = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR] = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG] = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL] = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL] = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER] = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING] = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER] = {number,   NULL,   PREC_NONE},
  [TOKEN_AND] = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE] = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL] = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR] = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER] = {super_,   NULL,   PREC_NONE},
  [TOKEN_THIS] = {this_,    NULL,   PREC_NONE},
  [TOKEN_TRUE] = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF] = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence) {
    advance(p, s);
    ParseFn prefixRule = getRule(p->previous.type)->prefix;
    if (prefixRule == NULL) {
        errorAt(p, &p->previous, "Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(p, s, c, cc, canAssign);

    while (precedence <= getRule(p->current.type)->precedence) {
        advance(p, s);
        ParseFn infixRule = getRule(p->previous.type)->infix;
        infixRule(p, s, c, cc, canAssign);
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

static void block(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        declaration(p, s, c, cc);
    }

    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, FunctionType type) {
    Compiler subCompiler;
    initCompiler(p, &subCompiler, c, type);
    beginScope(&subCompiler);

    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            subCompiler.function->arity++;
            if (subCompiler.function->arity > 255) {
                errorAtCurrent(p, "Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable(p, s, &subCompiler, "Expect parameter name.");
            defineVariable(p, &subCompiler, constant);
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block(p, s, &subCompiler, cc);

    ObjFunction* function = endCompiler(p, &subCompiler);
    emitBytes(p, c, OP_CLOSURE, makeConstant(p, c, OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(p, c, subCompiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(p, c, subCompiler.upvalues[i].index);
    }
}

static void method(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t name = identifierConstant(p, c, &p->previous);

    FunctionType type = TYPE_METHOD;
    if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(p, s, c, cc, type);
    emitBytes(p, c, OP_METHOD, name);
}

static void classDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect class name.");
    Token className = p->previous;
    uint8_t nameConstant = identifierConstant(p, c, &p->previous);
    declareVariable(p, c);

    emitBytes(p, c, OP_CLASS, nameConstant);
    defineVariable(p, c, nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = cc;
    classCompiler.hasSuperclass = false;

    if (match(p, s, TOKEN_LESS)) {
        consume(p, s, TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(p, s, c, &classCompiler, false);

        if (identifiersEqual(&className, &p->previous)) {
            errorAt(p, &p->previous, "A class can't inherit from itself.");
        }

        beginScope(c);
        addLocal(p, c, syntheticToken("super"));
        defineVariable(p, c, 0);

        namedVariable(p, s, c, &classCompiler, className, false);
        emitByte(p, c, OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(p, s, c, &classCompiler, className, false);
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        method(p, s, c, &classCompiler);
    }
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(p, c, OP_POP);

    if (classCompiler.hasSuperclass) {
        endScope(p, c);
    }
}

static void funDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    uint8_t global = parseVariable(p, s, c, "Expect function name.");
    markInitialized(c);
    function(p, s, c, cc, TYPE_FUNCTION);
    defineVariable(p, c, global);
}

static void varDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    uint8_t global = parseVariable(p, s, c, "Expect variable name.");

    if (match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
    } else {
        emitByte(p, c, OP_NIL);
    }
    consume(p, s, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(p, c, global);
}

static void expressionStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    expression(p, s, c, cc);
    consume(p, s, TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(p, c, OP_POP);
}

static void forStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    beginScope(c);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(p, s, TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(p, s, TOKEN_VAR)) {
        varDeclaration(p, s, c, cc);
    } else {
        expressionStatement(p, s, c, cc);
    }

    int loopStart = currentChunk(c)->count;
    int exitJump = -1;
    if (!match(p, s, TOKEN_SEMICOLON)) {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        exitJump = emitJump(p, c, OP_JUMP_IF_FALSE);
        emitByte(p, c, OP_POP);
    }

    if (!match(p, s, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(p, c, OP_JUMP);
        int incrementStart = currentChunk(c)->count;
        expression(p, s, c, cc);
        emitByte(p, c, OP_POP);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(p, c, loopStart);
        loopStart = incrementStart;
        patchJump(p, c, bodyJump);
    }

    statement(p, s, c, cc);
    emitLoop(p, c, loopStart);

    if (exitJump != -1) {
        patchJump(p, c, exitJump);
        emitByte(p, c, OP_POP);
    }

    endScope(p, c);
}

static void ifStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    emitByte(p, c, OP_POP);
    statement(p, s, c, cc);

    int elseJump = emitJump(p, c, OP_JUMP);

    patchJump(p, c, thenJump);
    emitByte(p, c, OP_POP);

    if (match(p, s, TOKEN_ELSE)) statement(p, s, c, cc);
    patchJump(p, c, elseJump);
}

static void printStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    expression(p, s, c, cc);
    consume(p, s, TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(p, c, OP_PRINT);
}

static void returnStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (c->type == TYPE_SCRIPT) {
        errorAt(p, &p->previous, "Can't return from top-level code.");
    }

    if (match(p, s, TOKEN_SEMICOLON)) {
        emitReturn(p, c);
    } else {
        if (c->type == TYPE_INITIALIZER) {
            errorAt(p, &p->previous, "Can't return a value from an initializer.");
        }

        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(p, c, OP_RETURN);
    }
}

static void whileStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    int loopStart = currentChunk(c)->count;
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    emitByte(p, c, OP_POP);
    statement(p, s, c, cc);
    emitLoop(p, c, loopStart);

    patchJump(p, c, exitJump);
    emitByte(p, c, OP_POP);
}

static void synchronize(Parser* p, Scanner* s) {
    p->panicMode = false;

    while (p->current.type != TOKEN_EOF) {
        if (p->previous.type == TOKEN_SEMICOLON) return;
        switch (p->current.type) {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;

        default:
            ;
        }

        advance(p, s);
    }
}

static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_CLASS)) {
        classDeclaration(p, s, c, cc);
    } else if (match(p, s, TOKEN_FUN)) {
        funDeclaration(p, s, c, cc);
    } else if (match(p, s, TOKEN_VAR)) {
        varDeclaration(p, s, c, cc);
    } else {
        statement(p, s, c, cc);
    }

    if (p->panicMode) synchronize(p, s);
}

static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_PRINT)) {
        printStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_FOR)) {
        forStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_IF)) {
        ifStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_RETURN)) {
        returnStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_WHILE)) {
        whileStatement(p, s, c, cc);
    } else if (match(p, s, TOKEN_LEFT_BRACE)) {
        beginScope(c);
        block(p, s, c, cc);
        endScope(p, c);
    } else {
        expressionStatement(p, s, c, cc);
    }
}

ObjFunction* compile(VM* vm, const char* source) {
    Scanner scanner;
    initScanner(&scanner, source);
    Parser parser;
    parser.vm = vm;
    parser.hadError = false;
    parser.panicMode = false;

    Compiler compiler;
    initCompiler(&parser, &compiler, NULL, TYPE_SCRIPT);

    advance(&parser, &scanner);

    while (!match(&parser, &scanner, TOKEN_EOF)) {
        declaration(&parser, &scanner, &compiler, NULL);
    }

    ObjFunction* function = endCompiler(&parser, &compiler);
    return parser.hadError ? NULL : function;
}

void markCompilerRoots(VM* vm) {
    Compiler* compiler = (Compiler*) vm->compiler;
    while (compiler != NULL) {
        markObject(vm, (struct Obj*) compiler->function);
        compiler = compiler->enclosing;
    }
}