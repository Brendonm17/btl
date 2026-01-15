#include <stdio.h>
#include <stdlib.h>
#include "compiler.h"
#include "scanner.h"

typedef struct {
    Token current, previous; bool hadError, panicMode; VM* vm;
} Parser;
typedef enum {
    PREC_NONE, PREC_ASSIGNMENT, PREC_OR, PREC_AND, PREC_EQUALITY, PREC_COMPARISON, PREC_TERM, PREC_FACTOR, PREC_UNARY, PREC_CALL, PREC_PRIMARY
} Precedence;
typedef void (*ParseFn)(Parser* p, Scanner* s, struct Compiler* c, bool canAssign);
typedef struct {
    ParseFn prefix, infix; Precedence precedence;
} ParseRule;
typedef struct {
    Token name; int depth; bool isCaptured;
} Local;
typedef struct {
    uint8_t index; bool isLocal;
} Upvalue;
typedef enum {
    TYPE_FUNCTION, TYPE_INITIALIZER, TYPE_METHOD, TYPE_SCRIPT
} FunctionType;
typedef struct Compiler {
    struct Compiler* enclosing; ObjFunction* function; FunctionType type; Local locals[UINT8_COUNT]; int localCount, scopeDepth; Upvalue upvalues[UINT8_COUNT]; VM* vm;
} Compiler;
typedef struct ClassCompiler {
    struct ClassCompiler* enclosing; bool hasSuperclass;
} ClassCompiler;

static Compiler* currentCompiler = NULL; // Only for GC roots.

static void errorAt(Parser* p, Token* t, const char* msg) {
    if (p->panicMode) return; p->panicMode = true; p->hadError = true;
    fprintf(stderr, "[line %d] Error", t->line);
    if (t->type == TOKEN_EOF) fprintf(stderr, " at end"); else if (t->type != TOKEN_ERROR) fprintf(stderr, " at '%.*s'", t->length, t->start);
    fprintf(stderr, ": %s\n", msg);
}
static void advance(Parser* p, Scanner* s) {
    p->previous = p->current; for (;;) {
        p->current = scanToken(s); if (p->current.type != TOKEN_ERROR) break; errorAt(p, &p->current, p->current.start);
    }
}
static void consume(Parser* p, Scanner* s, TokenType t, const char* msg) {
    if (p->current.type == t) {
        advance(p, s); return;
    } errorAt(p, &p->current, msg);
}
static bool match(Parser* p, Scanner* s, TokenType t) {
    if (p->current.type != t) return false; advance(p, s); return true;
}
static void emitByte(Compiler* c, uint8_t b) {
    writeChunk(c->vm, &c->function->chunk, b, c->vm->frameCount > 0 ? 0 : 0); /* line tracking simplified */
}
static void emitBytes(Compiler* c, uint8_t b1, uint8_t b2) {
    emitByte(c, b1); emitByte(c, b2);
}
static void emitReturn(Compiler* c) {
    if (c->type == TYPE_INITIALIZER) emitBytes(c, OP_GET_LOCAL, 0); else emitByte(c, OP_NIL); emitByte(c, OP_RETURN);
}
static uint8_t makeConstant(Compiler* c, Value v) {
    int i = addConstant(c->vm, &c->function->chunk, v); if (i > UINT8_MAX) {
        errorAt(NULL, NULL, "Too many constants."); return 0;
    } return (uint8_t) i;
}
static void initCompiler(Compiler* c, Parser* p, Compiler* enc, FunctionType t) {
    c->enclosing = enc; c->function = newFunction(p->vm); c->type = t; c->localCount = 0; c->scopeDepth = 0; c->vm = p->vm; currentCompiler = c;
    if (t != TYPE_SCRIPT) c->function->name = copyString(p->vm, p->previous.start, p->previous.length);
    Local* l = &c->locals[c->localCount++]; l->depth = 0; l->isCaptured = false;
    if (t != TYPE_SCRIPT) {
        l->name.start = "this"; l->name.length = 4;
    } else {
        l->name.start = ""; l->name.length = 0;
    }
}
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, Precedence prec);
static ParseRule* getRule(TokenType t);
static void expression(Parser* p, Scanner* s, Compiler* c) {
    parsePrecedence(p, s, c, PREC_ASSIGNMENT);
}
static uint8_t identifierConstant(Compiler* c, Token* t) {
    return makeConstant(c, OBJ_VAL(copyString(c->vm, t->start, t->length)));
}
static void declareVariable(Parser* p, Compiler* c) {
    if (c->scopeDepth == 0) return;
    Token* name = &p->previous;
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* l = &c->locals[i]; if (l->depth != -1 && l->depth < c->scopeDepth) break;
        if (name->length == l->name.length && memcmp(name->start, l->name.start, name->length) == 0) errorAt(p, name, "Already a variable with this name in this scope.");
    }
    Local* l = &c->locals[c->localCount++]; l->name = *name; l->depth = -1; l->isCaptured = false;
}
static uint8_t parseVariable(Parser* p, Scanner* s, Compiler* c, const char* msg) {
    consume(p, s, TOKEN_IDENTIFIER, msg); declareVariable(p, c); if (c->scopeDepth > 0) return 0; return identifierConstant(c, &p->previous);
}
static void defineVariable(Compiler* c, uint8_t global) {
    if (c->scopeDepth > 0) {
        c->locals[c->localCount - 1].depth = c->scopeDepth; return;
    } emitBytes(c, OP_DEFINE_GLOBAL, global);
}
static void statement(Parser* p, Scanner* s, Compiler* c);
static void declaration(Parser* p, Scanner* s, Compiler* c);

static void binary(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    TokenType opType = p->previous.type; ParseRule* rule = getRule(opType); parsePrecedence(p, s, c, (Precedence) (rule->precedence + 1));
    switch (opType) {
    case TOKEN_BANG_EQUAL: emitBytes(c, OP_EQUAL, OP_NOT); break; case TOKEN_EQUAL_EQUAL: emitByte(c, OP_EQUAL); break;
    case TOKEN_GREATER: emitByte(c, OP_GREATER); break; case TOKEN_GREATER_EQUAL: emitBytes(c, OP_LESS, OP_NOT); break;
    case TOKEN_LESS: emitByte(c, OP_LESS); break; case TOKEN_LESS_EQUAL: emitBytes(c, OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS: emitByte(c, OP_ADD); break; case TOKEN_MINUS: emitByte(c, OP_SUBTRACT); break;
    case TOKEN_STAR: emitByte(c, OP_MULTIPLY); break; case TOKEN_SLASH: emitByte(c, OP_DIVIDE); break;
    default: return;
    }
}
static void literal(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    switch (p->previous.type) {
    case TOKEN_FALSE: emitByte(c, OP_FALSE); break; case TOKEN_NIL: emitByte(c, OP_NIL); break; case TOKEN_TRUE: emitByte(c, OP_TRUE); break; default: return;
    }
}
static void grouping(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    expression(p, s, c); consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}
static void number(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    emitByte(c, OP_CONSTANT); emitByte(c, makeConstant(c, NUMBER_VAL(strtod(p->previous.start, NULL))));
}
static void string(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    emitByte(c, OP_CONSTANT); emitByte(c, makeConstant(c, OBJ_VAL(copyString(p->vm, p->previous.start + 1, p->previous.length - 2))));
}
static void unary(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    TokenType op = p->previous.type; parsePrecedence(p, s, c, PREC_UNARY); switch (op) {
    case TOKEN_BANG: emitByte(c, OP_NOT); break; case TOKEN_MINUS: emitByte(c, OP_NEGATE); break; default: return;
    }
}

static int resolveLocal(Compiler* c, Token* n) {
    for (int i = c->localCount - 1; i >= 0; i--) {
        Local* l = &c->locals[i]; if (n->length == l->name.length && memcmp(n->start, l->name.start, n->length) == 0) {
            if (l->depth == -1) errorAt(NULL, n, "Can't read local variable in its own initializer."); return i;
        }
    } return -1;
}
static int addUpvalue(Compiler* c, uint8_t i, bool l) {
    int count = c->function->upvalueCount; for (int j = 0; j < count; j++) {
        Upvalue* u = &c->upvalues[j]; if (u->index == i && u->isLocal == l) return j;
    } c->upvalues[count].isLocal = l; c->upvalues[count].index = i; return c->function->upvalueCount++;
}
static int resolveUpvalue(Compiler* c, Token* n) {
    if (c->enclosing == NULL) return -1; int local = resolveLocal(c->enclosing, n); if (local != -1) {
        c->enclosing->locals[local].isCaptured = true; return addUpvalue(c, (uint8_t) local, true);
    } int upvalue = resolveUpvalue(c->enclosing, n); if (upvalue != -1) return addUpvalue(c, (uint8_t) upvalue, false); return -1;
}
static void namedVariable(Parser* p, Scanner* s, Compiler* c, Token n, bool canAssign) {
    uint8_t getOp, setOp; int arg = resolveLocal(c, &n);
    if (arg != -1) {
        getOp = OP_GET_LOCAL; setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(c, &n)) != -1) {
        getOp = OP_GET_UPVALUE; setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(c, &n); getOp = OP_GET_GLOBAL; setOp = OP_SET_GLOBAL;
    }
    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c); emitBytes(c, setOp, (uint8_t) arg);
    } else {
        emitBytes(c, getOp, (uint8_t) arg);
    }
}
static void variable(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    namedVariable(p, s, c, p->previous, canAssign);
}
static void this_(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    variable(p, s, c, false);
}
static void call(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    uint8_t argCount = 0; if (!p->current.type == TOKEN_RIGHT_PAREN) {
        do {
            expression(p, s, c); argCount++;
        } while (match(p, s, TOKEN_COMMA));
    } consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after arguments."); emitBytes(c, OP_CALL, argCount);
}
static void dot(Parser* p, Scanner* s, Compiler* c, bool canAssign) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name after '.'."); uint8_t name = identifierConstant(c, &p->previous); if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c); emitBytes(c, OP_SET_PROPERTY, name);
    } else if (match(p, s, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = 0; if (!p->current.type == TOKEN_RIGHT_PAREN) {
            do {
                expression(p, s, c); argCount++;
            } while (match(p, s, TOKEN_COMMA));
        } consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after arguments."); emitBytes(c, OP_INVOKE, name); emitByte(c, argCount);
    } else {
        emitBytes(c, OP_GET_PROPERTY, name);
    }
}

ParseRule rules [] = {
  [TOKEN_LEFT_PAREN] = {grouping, call,   PREC_CALL},[TOKEN_RIGHT_PAREN] = {NULL,     NULL,   PREC_NONE},[TOKEN_DOT] = {NULL,     dot,    PREC_CALL},[TOKEN_MINUS] = {unary,    binary, PREC_TERM},[TOKEN_PLUS] = {NULL,     binary, PREC_TERM},[TOKEN_SLASH] = {NULL,     binary, PREC_FACTOR},[TOKEN_STAR] = {NULL,     binary, PREC_FACTOR},[TOKEN_BANG] = {unary,    NULL,   PREC_NONE},[TOKEN_BANG_EQUAL] = {NULL,     binary, PREC_EQUALITY},[TOKEN_EQUAL_EQUAL] = {NULL,     binary, PREC_EQUALITY},[TOKEN_GREATER] = {NULL,     binary, PREC_COMPARISON},[TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},[TOKEN_LESS] = {NULL,     binary, PREC_COMPARISON},[TOKEN_LESS_EQUAL] = {NULL,     binary, PREC_COMPARISON},[TOKEN_IDENTIFIER] = {variable, NULL,   PREC_NONE},[TOKEN_STRING] = {string,   NULL,   PREC_NONE},[TOKEN_NUMBER] = {number,   NULL,   PREC_NONE},[TOKEN_FALSE] = {literal,  NULL,   PREC_NONE},[TOKEN_NIL] = {literal,  NULL,   PREC_NONE},[TOKEN_TRUE] = {literal,  NULL,   PREC_NONE},[TOKEN_THIS] = {this_,    NULL,   PREC_NONE},[TOKEN_EOF] = {NULL,     NULL,   PREC_NONE},
};
static ParseRule* getRule(TokenType t) {
    return &rules[t];
}
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, Precedence prec) {
    advance(p, s); ParseFn prefixRule = getRule(p->previous.type)->prefix; if (prefixRule == NULL) {
        errorAt(p, &p->previous, "Expect expression."); return;
    } bool canAssign = prec <= PREC_ASSIGNMENT; prefixRule(p, s, c, canAssign); while (prec <= getRule(p->current.type)->precedence) {
        advance(p, s); ParseFn infixRule = getRule(p->previous.type)->infix; infixRule(p, s, c, canAssign);
    }
}

static void block(Parser* p, Scanner* s, Compiler* c) {
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) declaration(p, s, c); consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}
static void function(Parser* p, Scanner* s, Compiler* c, FunctionType t) {
    Compiler sub; initCompiler(&sub, p, c, t); sub.scopeDepth++; consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(p, TOKEN_RIGHT_PAREN)) {
        do {
            sub.function->arity++; if (sub.function->arity > 255) errorAt(p, &p->current, "Can't have more than 255 parameters."); uint8_t constant = parseVariable(p, s, &sub, "Expect parameter name."); defineVariable(&sub, constant);
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')' after parameters."); consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before function body."); block(p, s, &sub); ObjFunction* f = sub.function; emitReturn(&sub); currentCompiler = c; emitBytes(c, OP_CLOSURE, makeConstant(c, OBJ_VAL(f)));
    for (int i = 0; i < f->upvalueCount; i++) {
        emitByte(c, sub.upvalues[i].isLocal ? 1 : 0); emitByte(c, sub.upvalues[i].index);
    }
}
static void method(Parser* p, Scanner* s, Compiler* c) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect method name."); uint8_t name = identifierConstant(c, &p->previous); FunctionType t = TYPE_METHOD; if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) t = TYPE_INITIALIZER; function(p, s, c, t); emitBytes(c, OP_METHOD, name);
}
static void classDeclaration(Parser* p, Scanner* s, Compiler* c) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect class name."); Token nameToken = p->previous; uint8_t nameConstant = identifierConstant(c, &p->previous); declareVariable(p, c); emitBytes(c, OP_CLASS, nameConstant); defineVariable(c, nameConstant); consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before class body."); while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) method(p, s, c); consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
}
static void funDeclaration(Parser* p, Scanner* s, Compiler* c) {
    uint8_t global = parseVariable(p, s, c, "Expect function name."); c->locals[c->localCount - 1].depth = c->scopeDepth; function(p, s, c, TYPE_FUNCTION); defineVariable(c, global);
}
static void varDeclaration(Parser* p, Scanner* s, Compiler* c) {
    uint8_t global = parseVariable(p, s, c, "Expect variable name."); if (match(p, s, TOKEN_EQUAL)) expression(p, s, c); else emitByte(c, OP_NIL); consume(p, s, TOKEN_SEMICOLON, "Expect ';' after variable declaration."); defineVariable(c, global);
}
static void expressionStatement(Parser* p, Scanner* s, Compiler* c) {
    expression(p, s, c); consume(p, s, TOKEN_SEMICOLON, "Expect ';' after expression."); emitByte(c, OP_POP);
}
static void printStatement(Parser* p, Scanner* s, Compiler* c) {
    expression(p, s, c); consume(p, s, TOKEN_SEMICOLON, "Expect ';' after value."); emitByte(c, OP_PRINT);
}
static void returnStatement(Parser* p, Scanner* s, Compiler* c) {
    if (c->type == TYPE_SCRIPT) errorAt(p, &p->previous, "Can't return from top-level code."); if (match(p, s, TOKEN_SEMICOLON)) emitReturn(c); else {
        if (c->type == TYPE_INITIALIZER) errorAt(p, &p->previous, "Can't return a value from an initializer."); expression(p, s, c); consume(p, s, TOKEN_SEMICOLON, "Expect ';' after return value."); emitByte(c, OP_RETURN);
    }
}
static void declaration(Parser* p, Scanner* s, Compiler* c) {
    if (match(p, s, TOKEN_CLASS)) classDeclaration(p, s, c); else if (match(p, s, TOKEN_FUN)) funDeclaration(p, s, c); else if (match(p, s, TOKEN_VAR)) varDeclaration(p, s, c); else statement(p, s, c); if (p->panicMode) {
        p->panicMode = false; while (p->current.type != TOKEN_EOF) {
            if (p->previous.type == TOKEN_SEMICOLON) return; switch (p->current.type) {
            case TOKEN_CLASS: case TOKEN_FUN: case TOKEN_VAR: case TOKEN_FOR: case TOKEN_IF: case TOKEN_WHILE: case TOKEN_PRINT: case TOKEN_RETURN: return; default:;
            } advance(p, s);
        }
    }
}
static void statement(Parser* p, Scanner* s, Compiler* c) {
    if (match(p, s, TOKEN_PRINT)) printStatement(p, s, c); else if (match(p, s, TOKEN_RETURN)) returnStatement(p, s, c); else if (match(p, s, TOKEN_LEFT_BRACE)) {
        c->scopeDepth++; block(p, s, c); c->scopeDepth--; while (c->localCount > 0 && c->locals[c->localCount - 1].depth > c->scopeDepth) {
            if (c->locals[c->localCount - 1].isCaptured) emitByte(c, OP_CLOSE_UPVALUE); else emitByte(c, OP_POP); c->localCount--;
        }
    } else expressionStatement(p, s, c);
}

ObjFunction* compile(VM* vm, const char* source) {
    Scanner s; initScanner(&s, source); Parser p; p.vm = vm; p.hadError = false; p.panicMode = false; Compiler c; initCompiler(&c, &p, NULL, TYPE_SCRIPT); advance(&p, &s); while (!match(&p, &s, TOKEN_EOF)) declaration(&p, &s, &c); ObjFunction* f = c.function; emitReturn(&c); currentCompiler = NULL; return p.hadError ? NULL : f;
}
void markCompilerRoots(VM* vm) {
    Compiler* c = currentCompiler; while (c != NULL) {
        markObject(vm, (Obj*) c->function); c = c->enclosing;
    }
}