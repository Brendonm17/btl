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

static void expression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc);
static void function(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, FunctionType type);
static uint8_t parseVariable(Parser* p, Scanner* s, Compiler* c, const char* errorMessage);
static void defineVariable(Parser* p, Compiler* c, uint8_t global);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Precedence precedence);

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
    writeChunk(c->vm, currentChunk(c), byte, p->previous.line);
}

static void emitBytes(Parser* p, Compiler* c, uint8_t byte1, uint8_t byte2) {
    emitByte(p, c, byte1);
    emitByte(p, c, byte2);
}

static void emitLoop(Parser* p, Compiler* c, int loopStart) {
    emitByte(p, c, OP_LOOP);
    int offset = currentChunk(c)->count - loopStart + 2;
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
        errorAt(p, &p->previous, "Too many constants in chunk.");
        return 0;
    }
    return (uint8_t) constant;
}

static void emitConstant(Parser* p, Compiler* c, Value value) {
    emitBytes(p, c, OP_CONSTANT, makeConstant(p, c, value));
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
    c->vm = p->vm;
    c->module = module;
    c->currentLoop = NULL;
    c->function = newFunction(p->vm, module);
    c->vm->compiler = (void*) c;

    if (type != TYPE_SCRIPT) {
        c->function->name = copyString(p->vm, p->previous.start, p->previous.length);
    }

    Local* local = &c->locals[c->localCount++];
    local->depth = 0;
    local->isCaptured = false;

    // Fix: Ensure slot 0 is named to prevent indexing errors in closures/recursion
    if (type != TYPE_SCRIPT) {
        if (type == TYPE_METHOD || type == TYPE_INITIALIZER) {
            local->name.start = "this";
            local->name.length = 4;
        } else {
            // Function slot 0 is named after the function itself
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
    c->vm->compiler = (void*) c->enclosing;
    return function;
}

static void beginScope(Compiler* c) {
    c->scopeDepth++;
}

static void endScope(Parser* p, Compiler* c) {
    c->scopeDepth--;
    while (c->localCount > 0 && c->locals[c->localCount - 1].depth > c->scopeDepth) {
        if (c->locals[c->localCount - 1].isCaptured) emitByte(p, c, OP_CLOSE_UPVALUE);
        else emitByte(p, c, OP_POP);
        c->localCount--;
    }
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static uint8_t identifierConstant(Parser* p, Compiler* c, Token* name) {
    (void) p;
    struct ObjString* nameString = copyString(c->vm, name->start, name->length);
    Value indexValue;
    if (tableGet(&c->module->globalNames, nameString, &indexValue)) return (uint8_t) AS_NUMBER(indexValue);
    int index = c->module->globalValues.count;
    writeValueArray(c->vm, &c->module->globalValues, EMPTY_VAL);
    tableSet(c->vm, &c->module->globalNames, nameString, NUMBER_VAL((double) index));
    return (uint8_t) index;
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

static int addUpvalue(Compiler* c, uint8_t index, bool isLocal) {
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
    int local = resolveLocal(p, c->enclosing, name);
    if (local != -1) {
        c->enclosing->locals[local].isCaptured = true;
        return addUpvalue(c, (uint8_t) local, true);
    }
    int upvalue = resolveUpvalue(p, c->enclosing, name);
    if (upvalue != -1) return addUpvalue(c, (uint8_t) upvalue, false);
    return -1;
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
    l->name = *name; l->depth = -1; l->isCaptured = false;
}

static void funExpression(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign; // Silence the unused parameter warning
    function(p, s, c, cc, TYPE_FUNCTION);
}

// Pratt Parser Prefix/Infix Functions
static void binary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
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
    emitConstant(p, c, NUMBER_VAL(value));
}

static void string(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) s; (void) cc; (void) canAssign;
    Value value = OBJ_VAL(copyString(c->vm, p->previous.start + 1, p->previous.length - 2));
    emitConstant(p, c, value);
}

static void namedVariable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(p, c, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL; setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(p, c, &name)) != -1) {
        getOp = OP_GET_UPVALUE; setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(p, c, &name); getOp = OP_GET_GLOBAL; setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(p, s, TOKEN_EQUAL)) {
        expression(p, s, c, cc);
        emitBytes(p, c, setOp, (uint8_t) arg);
    } else emitBytes(p, c, getOp, (uint8_t) arg);
}

static void variable(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    namedVariable(p, s, c, cc, p->previous, canAssign);
}

static void listLiteral(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
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
    consume(p, s, TOKEN_IDENTIFIER, "Expect property name.");
    uint8_t name = makeConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start, p->previous.length)));
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
    } else emitBytes(p, c, OP_GET_PROPERTY, name);
}

static void unary(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    TokenType op = p->previous.type;
    parsePrecedence(p, s, c, cc, PREC_UNARY);
    switch (op) {
    case TOKEN_BANG:  emitByte(p, c, OP_NOT); break;
    case TOKEN_MINUS: emitByte(p, c, OP_NEGATE); break;
    default: return;
    }
}

static void and_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int endJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    emitByte(p, c, OP_POP);
    parsePrecedence(p, s, c, cc, PREC_AND);
    patchJump(p, c, endJump);
}

static void or_(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc, bool canAssign) {
    (void) canAssign;
    int elseJump = emitJump(p, c, OP_JUMP_IF_FALSE);
    int endJump = emitJump(p, c, OP_JUMP);
    patchJump(p, c, elseJump);
    emitByte(p, c, OP_POP);
    parsePrecedence(p, s, c, cc, PREC_OR);
    patchJump(p, c, endJump);
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
    emitBytes(p, c, OP_CALL, args);
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
    uint8_t name = makeConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start, p->previous.length)));
    Token thisT = { .start = "this", .length = 4 };
    namedVariable(p, s, c, cc, thisT, false);
    Token superT = { .start = "super", .length = 5 };
    namedVariable(p, s, c, cc, superT, false);
    emitBytes(p, c, OP_GET_SUPER, name);
}

ParseRule rules [] = {
  [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
  [TOKEN_LEFT_BRACKET] = {listLiteral, subscript, PREC_CALL},
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
  [TOKEN_FUN] = {funExpression, NULL,   PREC_NONE},
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
    // If we are at assignment precedence but didn't consume the '=', 
    // it means the left-hand side wasn't a valid assignment target.
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
            uint8_t constant = parseVariable(p, s, &sub, "Expect parameter name.");
            defineVariable(p, &sub, constant);
        } while (match(p, s, TOKEN_COMMA));
    }
    consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{'.");
    block(p, s, &sub, cc);
    ObjFunction* f = endCompiler(p, &sub);
    emitBytes(p, c, OP_CLOSURE, (uint8_t) addConstant(c->vm, currentChunk(c), OBJ_VAL(f)));
    for (int i = 0; i < f->upvalueCount; i++) {
        emitByte(p, c, sub.upvalues[i].isLocal ? 1 : 0);
        emitByte(p, c, sub.upvalues[i].index);
    }
}

static void method(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t name = makeConstant(p, c, OBJ_VAL(copyString(c->vm, p->previous.start, p->previous.length)));
    FunctionType type = TYPE_METHOD;
    if (p->previous.length == 4 && memcmp(p->previous.start, "init", 4) == 0) type = TYPE_INITIALIZER;
    function(p, s, c, cc, type);
    emitBytes(p, c, OP_METHOD, name);
}

static void classDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    consume(p, s, TOKEN_IDENTIFIER, "Expect class name.");
    Token nameToken = p->previous;

    // 1. Get string constant for the VM to name the class
    uint8_t nameStringConst = makeConstant(p, c, OBJ_VAL(copyString(c->vm, nameToken.start, nameToken.length)));
    // 2. Get global table index to store the class variable
    uint8_t nameGlobalIdx = identifierConstant(p, c, &nameToken);

    declareVariable(p, c);

    // Create class and define the global variable
    emitBytes(p, c, OP_CLASS, nameStringConst); // Use the string constant index
    defineVariable(p, c, nameGlobalIdx);        // Use the global table index

    ClassCompiler classC = { .enclosing = cc, .hasSuperclass = false };

    if (match(p, s, TOKEN_LESS)) {
        consume(p, s, TOKEN_IDENTIFIER, "Expect superclass name.");

        // Push superclass onto stack
        variable(p, s, c, &classC, false);

        if (identifiersEqual(&nameToken, &p->previous)) {
            errorAt(p, &p->previous, "A class can't inherit from itself.");
        }

        beginScope(c);
        // Manual 'super' local variable
        Local* l = &c->locals[c->localCount++];
        l->name.start = "super"; l->name.length = 5; l->depth = c->scopeDepth; l->isCaptured = false;

        // Push subclass back onto stack for OP_INHERIT
        namedVariable(p, s, c, &classC, nameToken, false);
        emitByte(p, c, OP_INHERIT);
        classC.hasSuperclass = true;
    }

    // Now load the class back onto the stack to bind methods
    namedVariable(p, s, c, &classC, nameToken, false);

    consume(p, s, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(p, TOKEN_RIGHT_BRACE) && !check(p, TOKEN_EOF)) {
        method(p, s, c, &classC);
    }
    consume(p, s, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    emitByte(p, c, OP_POP); // Pop the class

    if (classC.hasSuperclass) {
        endScope(p, c);
    }
}

static void funDeclaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    // 1. Parse the function name (the variable that will hold the closure)
    uint8_t global = parseVariable(p, s, c, "Expect function name.");
    // 2. Mark it as initialized so the function can be recursive
    markInitialized(c);
    // 3. Compile the body using your helper
    function(p, s, c, cc, TYPE_FUNCTION);
    // 4. Finalize the variable
    defineVariable(p, c, global);
}

static void forStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    beginScope(c);
    consume(p, s, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    // 1. Initializer Clause
    if (match(p, s, TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(p, s, TOKEN_VAR)) {
        uint8_t global = parseVariable(p, s, c, "Expect variable name.");
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, OP_NIL);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        defineVariable(p, c, global);
    } else {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        emitByte(p, c, OP_POP);
    }

    int loopStart = currentChunk(c)->count;

    // 2. Condition Clause
    int exitJump = -1;
    if (!match(p, s, TOKEN_SEMICOLON)) {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after loop condition.");
        exitJump = emitJump(p, c, OP_JUMP_IF_FALSE);
        emitByte(p, c, OP_POP);
    }

    // 3. Increment Clause
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

    Loop loop = { .enclosing = c->currentLoop, .start = loopStart, .scopeDepth = c->scopeDepth, .breakCount = 0 };
    c->currentLoop = &loop;

    // --- SHADOWING LOGIC START ---
    // We look for the loop variable in the scope we just created in the initializer.
    int loopVar = -1;
    for (int i = c->localCount - 1; i >= 0; i--) {
        if (c->locals[i].depth != -1 && c->locals[i].depth == c->scopeDepth) {
            loopVar = i;
            break;
        }
    }

    beginScope(c); // Inner iteration scope

    if (loopVar != -1) {
        // Load the current value of the master loop variable
        emitBytes(p, c, OP_GET_LOCAL, (uint8_t) loopVar);

        // Manually create a new local variable with the same name in the inner scope
        Local* shadow = &c->locals[c->localCount++];
        shadow->name = c->locals[loopVar].name;
        shadow->depth = c->scopeDepth; // Initialize immediately
        shadow->isCaptured = false;
    }

    statement(p, s, c, cc); // Parse the loop body

    if (loopVar != -1) {
        // Copy the value from the iteration variable back to the master variable
        // so that the increment clause (e.g., i++) uses the updated value.
        emitBytes(p, c, OP_GET_LOCAL, (uint8_t) c->localCount - 1);
        emitBytes(p, c, OP_SET_LOCAL, (uint8_t) loopVar);
        emitByte(p, c, OP_POP);
    }

    endScope(p, c); // Emits OP_CLOSE_UPVALUE for the iteration-specific variable
    // --- SHADOWING LOGIC END ---

    emitLoop(p, c, loopStart);

    if (exitJump != -1) {
        patchJump(p, c, exitJump);
        emitByte(p, c, OP_POP);
    }

    for (int i = 0; i < loop.breakCount; i++) {
        patchJump(p, c, loop.breakJumps[i]);
    }

    c->currentLoop = loop.enclosing;
    endScope(p, c); // Closes the outer loop scope (including the master variable)
}
static void returnStatement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (c->type == TYPE_SCRIPT) {
        errorAt(p, &p->previous, "Can't return from top-level code.");
    }

    if (match(p, s, TOKEN_SEMICOLON)) {
        // Emit logic for 'return;'
        emitByte(p, c, OP_NIL);
        emitByte(p, c, OP_RETURN);
    } else {
        if (c->type == TYPE_INITIALIZER) errorAt(p, &p->previous, "Can't return a value from an initializer.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';' after return value.");
        // --- TCO Logic ---
        Chunk* chunk = currentChunk(c);
        // We look at the very last byte emitted by 'expression'
        // If it was OP_CALL, we swap it for OP_TAIL_CALL
        if (chunk->count >= 2 && chunk->code[chunk->count - 2] == OP_CALL) {
            chunk->code[chunk->count - 2] = OP_TAIL_CALL;
        } else {
            emitByte(p, c, OP_RETURN);
        }
    }
}

static void statement(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_PRINT)) {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        emitByte(p, c, OP_PRINT);
    } else if (match(p, s, TOKEN_FOR)) forStatement(p, s, c, cc);
    else if (match(p, s, TOKEN_IF)) {
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int thenJ = emitJump(p, c, OP_JUMP_IF_FALSE);
        emitByte(p, c, OP_POP);
        statement(p, s, c, cc);
        int elseJ = emitJump(p, c, OP_JUMP);
        patchJump(p, c, thenJ);
        emitByte(p, c, OP_POP);
        if (match(p, s, TOKEN_ELSE)) statement(p, s, c, cc);
        patchJump(p, c, elseJ);
    } else if (match(p, s, TOKEN_RETURN)) returnStatement(p, s, c, cc);
    else if (match(p, s, TOKEN_WHILE)) {
        int start = currentChunk(c)->count;
        Loop loop = { .enclosing = c->currentLoop, .start = start, .scopeDepth = c->scopeDepth, .breakCount = 0 };
        c->currentLoop = &loop;
        consume(p, s, TOKEN_LEFT_PAREN, "Expect '('.");
        expression(p, s, c, cc);
        consume(p, s, TOKEN_RIGHT_PAREN, "Expect ')'.");
        int exitJ = emitJump(p, c, OP_JUMP_IF_FALSE);
        emitByte(p, c, OP_POP);
        statement(p, s, c, cc);
        emitLoop(p, c, start);
        patchJump(p, c, exitJ);
        emitByte(p, c, OP_POP);
        for (int i = 0; i < loop.breakCount; i++) patchJump(p, c, loop.breakJumps[i]);
        c->currentLoop = loop.enclosing;
    } else if (match(p, s, TOKEN_BREAK)) {
        if (c->currentLoop == NULL) errorAt(p, &p->previous, "Can't use 'break' outside loop.");
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > c->currentLoop->scopeDepth; i--) emitByte(p, c, OP_POP);
        int jump = emitJump(p, c, OP_JUMP);
        c->currentLoop->breakJumps[c->currentLoop->breakCount++] = jump;
    } else if (match(p, s, TOKEN_LEFT_BRACE)) {
        beginScope(c);
        block(p, s, c, cc);
        endScope(p, c);
    } else {
        expression(p, s, c, cc);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        emitByte(p, c, OP_POP);
    }
}

static void defineVariable(Parser* p, Compiler* c, uint8_t global) {
    if (c->scopeDepth > 0) {
        markInitialized(c); return;
    }
    emitBytes(p, c, OP_DEFINE_GLOBAL, global);
}

static uint8_t parseVariable(Parser* p, Scanner* s, Compiler* c, const char* msg) {
    consume(p, s, TOKEN_IDENTIFIER, msg);
    declareVariable(p, c);
    if (c->scopeDepth > 0) return 0;
    return identifierConstant(p, c, &p->previous);
}

static void declaration(Parser* p, Scanner* s, Compiler* c, ClassCompiler* cc) {
    if (match(p, s, TOKEN_CLASS)) classDeclaration(p, s, c, cc);
    else if (match(p, s, TOKEN_FUN)) funDeclaration(p, s, c, cc);
    else if (match(p, s, TOKEN_VAR)) {
        uint8_t global = parseVariable(p, s, c, "Expect variable name.");
        if (match(p, s, TOKEN_EQUAL)) expression(p, s, c, cc);
        else emitByte(p, c, OP_NIL);
        consume(p, s, TOKEN_SEMICOLON, "Expect ';'.");
        defineVariable(p, c, global);
    } else if (match(p, s, TOKEN_IMPORT)) {
        consume(p, s, TOKEN_STRING, "Expect filename.");
        Token pathToken = p->previous;

        // 1. Path constant for the VM
        uint8_t file = makeConstant(p, c, OBJ_VAL(copyString(c->vm, pathToken.start + 1, pathToken.length - 2)));

        uint8_t alias;
        if (match(p, s, TOKEN_AS)) {
            // Case: import "libs/math" as Math;
            alias = parseVariable(p, s, c, "Expect variable name after 'as'.");
        } else {
            // Case: import "libs/math.btl"; -> extract "math"
            const char* path = pathToken.start + 1;
            int length = pathToken.length - 2;

            // Find the start of the filename (after last / or \)
            const char* filename = path;
            for (int i = 0; i < length; i++) {
                if (path[i] == '/' || path[i] == '\\') filename = path + i + 1;
            }

            // Determine length until the first dot
            int nameLength = (int) (path + length - filename);
            for (int i = 0; i < nameLength; i++) {
                if (filename[i] == '.') {
                    nameLength = i;
                    break;
                }
            }

            // Create a synthetic token for the "math" variable
            Token nameToken;
            nameToken.start = filename;
            nameToken.length = nameLength;

            declareVariable(p, c);
            alias = identifierConstant(p, c, &nameToken);
        }

        emitBytes(p, c, OP_IMPORT, file);
        defineVariable(p, c, alias);
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
    Compiler c; initCompiler(&p, &c, NULL, TYPE_SCRIPT, module);
    advance(&p, &s);
    while (!match(&p, &s, TOKEN_EOF)) declaration(&p, &s, &c, NULL);
    ObjFunction* function = endCompiler(&p, &c);
    return p.hadError ? NULL : function;
}

void markCompilerRoots(struct VM* vm) {
    Compiler* c = (Compiler*) vm->compiler;
    while (c) {
        markObject(vm, (Obj*) c->function); c = c->enclosing;
    }
}