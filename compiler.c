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
    struct VM* vm;
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
    PREC_CALL,        // . () []
    PREC_PRIMARY
} Precedence;

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
    p->hadError = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
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

static void initCompiler(Parser* p, Compiler* c, Compiler* enclosing, FunctionType type, ObjModule* module) {
    c->enclosing = enclosing;
    c->function = NULL;
    c->type = type;
    c->localCount = 0;
    c->scopeDepth = 0;
    c->currentLoop = NULL;
    c->vm = p->vm;
    c->module = module;
    c->vm->compiler = (void*) c;
    c->function = newFunction(p->vm, module);

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
    emitBytes(p, c, OP_NIL, OP_RETURN);
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

// Forward declarations for recursive descent
static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence);

static uint8_t identifierConstant(Parser* p, Compiler* c, Token* name) {
    ObjString* nameString = copyString(c->vm, name->start, name->length);
    Value indexValue;
    if (tableGet(&c->module->globalNames, nameString, &indexValue)) return (uint8_t) AS_NUMBER(indexValue);

    int index = c->module->globalValues.count;
    writeValueArray(c->vm, &c->module->globalValues, EMPTY_VAL);
    tableSet(c->vm, &c->module->globalNames, nameString, NUMBER_VAL((double) index));
    return (uint8_t) index;
}

static bool identifiersEqual(Token* a, Token* b) {
    return a->length == b->length && memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* c, Token* name) {
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* local = &c->locals[i];
        if (identifiersEqual(name, &local->name)) {
            return i;
        }
    }
    return -1;
}

static int addUpvalue(Parser* p, Compiler* c, uint8_t index, bool isLocal) {
    int count = c->function->upvalueCount;
    for (int i = 0; i < count; i++) {
        if (c->upvalues[i].index == index && c->upvalues[i].isLocal == isLocal) return i;
    }
    c->upvalues[count].isLocal = isLocal;
    c->upvalues[count].index = index;
    return c->function->upvalueCount++;
}

static int resolveUpvalue(Parser* p, Compiler* c, Token* name) {
    if (c->enclosing == NULL) return -1;
    int local = resolveLocal(c->enclosing, name);
    if (local != -1) {
        c->enclosing->locals[local].isCaptured = true;
        return addUpvalue(p, c, (uint8_t) local, true);
    }
    int upvalue = resolveUpvalue(p, c->enclosing, name);
    if (upvalue != -1) return addUpvalue(p, c, (uint8_t) upvalue, false);
    return -1;
}

static void binary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    TokenType opType = p->previous.type;
    ParseRule* rule = getRule(opType);
    parsePrecedence(p, s, c, cc, (Precedence) (rule->precedence + 1));

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
    default: return;
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

static void string(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    emitConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start + 1, p->previous.length - 2)));
}

static void namedVariable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(c, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL; setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(p, c, &name)) != -1) {
        getOp = OP_GET_UPVALUE; setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(p, c, &name);
        getOp = OP_GET_GLOBAL; setOp = OP_SET_GLOBAL;
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

static void listLiteral(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    int count = 0;
    if (!check(p, TOKEN_RIGHT_BRACKET)) {
        do {
            expression(p, s, c, cc);
            if (count == 255) errorAt(p, &p->previous, "List too large.");
            count++;
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']' after list.");
    emitBytes(p, c, OP_BUILD_LIST, (uint8_t) count);
}

static void subscript(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    expression(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_BRACKET, "Expect ']' after subscript.");
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitByte(p, c, OP_INDEX_SET);
    } else {
        emitByte(p, c, OP_INDEX_GET);
    }
}

static void dot(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(p, c, &p->previous);

    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitBytes(p, c, OP_SET_PROPERTY, name);
    } else if (match(p, s, TOKEN_LEFT_PAREN)) {
        uint8_t args = 0;
        if (!check(p, TOKEN_RIGHT_PAREN)) {
            do {
                expression(p, s, c, cc); args++;
            } while (match(p, s, TOKEN_COMMA));
        }
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        emitBytes(p, c, OP_INVOKE, name);
        emitByte(p, c, args);
    } else {
        emitBytes(p, c, OP_GET_PROPERTY, name);
    }
}

static void unary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    TokenType op = p->previous.type;
    parsePrecedence(p, s, c, cc, PREC_UNARY);
    switch (op) {
    case TOKEN_BANG:  emitByte(p, c, OP_NOT); break;
    case TOKEN_MINUS: emitByte(p, c, OP_NEGATE); break;
    default: return;
    }
}

static void and_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    int endJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    emitByte(p, c, OP_POP);
    parsePrecedence(p, s, c, cc, PREC_AND);
    patchJump(p, c, endJump);
}

static void or_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    int elseJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    int endJump = emitJump(p, c, OP_JUMP);
    patchJump(p, c, elseJump);
    emitByte(p, c, OP_POP);
    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, endJump);
}

static void call(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    uint8_t args = 0;
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            expression(p, s, c, cc); args++;
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
    emitBytes(p, c, OP_CALL, args);
}

static void this_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    if (cc == NULL) {
        errorAt(p, &p->previous, "Can't use 'this' outside class."); return;
    }
    variable(p, s, c, cc, false);
}

static void super_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    if (cc == NULL) errorAt(p, &p->previous, "Can't use 'super' outside class.");
    consume(p, s, TOKEN_DOT, "Expect '.'.");
    consume(p, s, TOKEN_IDENTIFIER, "Expect method.");
    uint8_t name = identifierConstant(p, c, &p->previous);

    Token thisT = { .start = "this", .length = 4 };
    namedVariable(p, s, c, cc, thisT, false);
    Token superT = { .start = "super", .length = 5 };
    namedVariable(p, s, c, cc, superT, false);
    emitBytes(p, c, OP_GET_SUPER, name);
}

ParseRule rules [] = {
  [TOKEN_LEFT_PAREN] = {grouping,    call,       PREC_CALL},
  [TOKEN_RIGHT_PAREN] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_LEFT_BRACKET] = {listLiteral, subscript,  PREC_CALL},
  [TOKEN_RIGHT_BRACKET] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_LEFT_BRACE] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_RIGHT_BRACE] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_COMMA] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_DOT] = {NULL,        dot,        PREC_CALL},
  [TOKEN_MINUS] = {unary,       binary,     PREC_TERM},
  [TOKEN_PLUS] = {NULL,        binary,     PREC_TERM},
  [TOKEN_SEMICOLON] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_SLASH] = {NULL,        binary,     PREC_FACTOR},
  [TOKEN_STAR] = {NULL,        binary,     PREC_FACTOR},
  [TOKEN_BANG] = {unary,       NULL,       PREC_NONE},
  [TOKEN_BANG_EQUAL] = {NULL,        binary,     PREC_EQUALITY},
  [TOKEN_EQUAL] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_EQUAL_EQUAL] = {NULL,        binary,     PREC_EQUALITY},
  [TOKEN_GREATER] = {NULL,        binary,     PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,        binary,     PREC_COMPARISON},
  [TOKEN_LESS] = {NULL,        binary,     PREC_COMPARISON},
  [TOKEN_LESS_EQUAL] = {NULL,        binary,     PREC_COMPARISON},
  [TOKEN_IDENTIFIER] = {variable,    NULL,       PREC_NONE},
  [TOKEN_STRING] = {string,      NULL,       PREC_NONE},
  [TOKEN_NUMBER] = {number,      NULL,       PREC_NONE},
  [TOKEN_AND] = {NULL,        and_,       PREC_AND},
  [TOKEN_CLASS] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_ELSE] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_FALSE] = {literal,     NULL,       PREC_NONE},
  [TOKEN_FOR] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_FUN] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_IF] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_NIL] = {literal,     NULL,       PREC_NONE},
  [TOKEN_OR] = {NULL,        or_,        PREC_OR},
  [TOKEN_PRINT] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_RETURN] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_SUPER] = {super_,      NULL,       PREC_NONE},
  [TOKEN_THIS] = {this_,       NULL,       PREC_NONE},
  [TOKEN_TRUE] = {literal,     NULL,       PREC_NONE},
  [TOKEN_VAR] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_WHILE] = {NULL,        NULL,       PREC_NONE},
  [TOKEN_EOF] = {NULL,        NULL,       PREC_NONE},
};

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence) {
    advance(p, s);
    ParseFn prefixRule = getRule(p->previous.type)->prefix;
    if (prefixRule == NULL) {
        errorAt(p, &p->previous, "Expect expression."); return;
    }
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(p, s, c, cc, canAssign);

    while (precedence <= getRule(p->current.type)->precedence) {
        advance(p, s);
        ParseFn infixRule = getRule(p->previous.type)->infix;
        infixRule(p, s, c, cc, canAssign);
    }

    if (canAssign && match(p, s, TOKEN_EQUAL)) errorAt(p, &p->previous, "Invalid target.");
}

static void block(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) declaration(p, s, c, cc);
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, FunctionType type) {
    Compiler sub; initCompiler(p, &sub, c, type, c->module); beginScope(&sub);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++; uint8_t constant = parseVariable(p, s, &sub, "Expect name.");
            sub.locals[sub.localCount - 1].depth = sub.scopeDepth;
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'."); consume(p, s, TOKEN_LEFT_BRACE, "Expect '{'.");
    block(p, s, &sub, cc); ObjFunction* f = endCompiler(p, &sub);
    emitBytes(p, c, OP_CLOSURE, (uint8_t) addConstant(c->vm, &c->function->chunk, OBJ_VAL(f)));
    for (int i = 0; i < f->upvalueCount; i++) {
        emitByte(p, c, sub.upvalues[i].isLocal ? 1 : 0); emitByte(p, c, sub.upvalues[i].index);
    }
}

static void method(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect name."); uint8_t name = identifierConstant(p, c, &p->previous);
    FunctionType type = TYPE_METHOD; if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) type = TYPE_INITIALIZER;
    function(p, s, c, cc, type); emitBytes(p, c, OP_METHOD, name);
}

static void classDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect class name."); Token nameT = p->previous;
    uint8_t nameConst = identifierConstant(p, c, &p->previous);
    emitBytes(p, c, OP_CLASS, nameConst);
    ClassCompiler classC = { .enclosing = cc, .hasSuperclass = false };
    if (match(p, s, TOKEN_LESS)) {
        consume(p, s, TOKEN_IDENTIFIER, "Expect super."); variable(p, s, c, &classC, false);
        beginScope(c); Local* l = &c->locals[c->localCount++]; l->name.start = "super"; l->name.length = 5; l->depth = c->scopeDepth;
        namedVariable(p, s, c, &classC, nameT, false); emitByte(p, c, OP_INHERIT); classC.hasSuperclass = true;
    }
    namedVariable(p, s, c, &classC, nameT, false); consume(p, s, TOKEN_LEFT_BRACE, "Expect '{'.");
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) method(p, s, c, &classC);
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}'."); emitByte(p, c, OP_POP);
    if (classC.hasSuperclass) endScope(p, c);
}

static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_IMPORT)) {
        consume(p, s, TOKEN_STRING, "Expect filename."); uint8_t file = identifierConstant(p, c, &p->previous);
        consume(p, s, TOKEN_AS, "Expect 'as'."); consume(p, s, TOKEN_IDENTIFIER, "Expect alias."); uint8_t alias = identifierConstant(p, c, &p->previous);
        emitBytes(p, c, OP_IMPORT, file); emitBytes(p, c, OP_DEFINE_GLOBAL, alias); consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
    } else if (match(p, s, TOKEN_VAR)) {
        uint8_t global = identifierConstant(p, c, &p->current); advance(p, s);
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc); else emitByte(p, c, OP_NIL);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'."); emitBytes(p, c, OP_DEFINE_GLOBAL, global);
    } else statement(p, s, c, cc);
}

static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_PRINT)) {
        expression(p, s, c, cc); consume(p, s, TOKEN_SEMICOLON, "Expect ';'."); emitByte(p, c, OP_PRINT);
    } else if (match(p, s, TOKEN_BREAK)) {
        if (c->currentLoop == NULL) errorAt(p, &p->previous, "Break error.");
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > c->currentLoop->scopeDepth; i--) emitByte(p, c, OP_POP);
        int j = emitJump(p, c, OP_JUMP); c->currentLoop->breakJumps[c->currentLoop->breakCount++] = j;
    } else if (match(p, s, TOKEN_IF)) {
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('."); expression(p, s, c, cc); consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int thenJ = emitJump(p, c, OP_JUMP_IF_FALSE); emitByte(p, c, OP_POP); statement(p, s, c, cc);
        int elseJ = emitJump(p, c, OP_JUMP); patchJump(p, c, thenJ); emitByte(p, c, OP_POP);
        if (match(p, s, TOKEN_ELSE)) statement(p, s, c, cc); patchJump(p, c, elseJ);
    } else if (match(p, s, TOKEN_WHILE)) {
        int start = currentChunk(c)->count; Loop loop = { .enclosing = c->currentLoop, .start = start, .scopeDepth = c->scopeDepth, .breakCount = 0 }; c->currentLoop = &loop;
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('."); expression(p, s, c, cc); consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int exitJ = emitJump(p, c, OP_JUMP_IF_FALSE); emitByte(p, c, OP_POP); statement(p, s, c, cc); emitLoop(p, c, start);
        patchJump(p, c, exitJ); emitByte(p, c, OP_POP); for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]); c->currentLoop = loop.enclosing;
    } else if (match(p, s, TOKEN_LEFT_BRACE)) {
        beginScope(c); block(p, s, c, cc); endScope(p, c);
    } else {
        expression(p, s, c, cc); consume(p, s, TOKEN_SEMICOLON, "Expect ';'."); emitByte(p, c, OP_POP);
    }
}

ObjFunction* compile(struct VM* vm, ObjModule* module, const char* source) {
    Scanner s; initScanner(&s, source); Parser p = { .vm = vm, .hadError = false, .panicMode = false };
    Compiler c; initCompiler(&p, &c, NULL, TYPE_SCRIPT, module); advance(&p, &s);
    while (!match(&p, &s, TOKEN_EOF)) declaration(&p, &s, &c, NULL);
    return p.hadError ? NULL : endCompiler(&p, &c);
}