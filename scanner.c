#include <string.h>
#include "scanner.h"
#include "common.h"

void initScanner(Scanner* s, const char* source) {
    s->start = source; s->current = source; s->line = 1;
}
static bool isAtEnd(Scanner* s) {
    return *s->current == '\0';
}
static char advance(Scanner* s) {
    s->current++; return s->current[-1];
}
static char peek(Scanner* s) {
    return *s->current;
}
static char peekNext(Scanner* s) {
    if (isAtEnd(s)) return '\0'; return s->current[1];
}
static bool match(Scanner* s, char expected) {
    if (isAtEnd(s) || *s->current != expected) return false; s->current++; return true;
}
static Token makeToken(Scanner* s, TokenType type) {
    Token t; t.type = type; t.start = s->start; t.length = (int) (s->current - s->start); t.line = s->line; return t;
}
static Token errorToken(Scanner* s, const char* msg) {
    Token t; t.type = TOKEN_ERROR; t.start = msg; t.length = (int) strlen(msg); t.line = s->line; return t;
}
static void skipWhitespace(Scanner* s) {
    for (;;) {
        char c = peek(s);
        switch (c) {
        case ' ': case '\r': case '\t': advance(s); break;
        case '\n': s->line++; advance(s); break;
        case '/': if (peekNext(s) == '/') {
            while (peek(s) != '\n' && !isAtEnd(s)) advance(s);
        } else return; break;
        default: return;
        }
    }
}
static TokenType checkKeyword(Scanner* s, int start, int len, const char* rest, TokenType type) {
    if (s->current - s->start == start + len && memcmp(s->start + start, rest, len) == 0) return type;
    return TOKEN_IDENTIFIER;
}
static TokenType identifierType(Scanner* s) {
    switch (s->start[0]) {
    case 'a': return checkKeyword(s, 1, 2, "nd", TOKEN_AND);
    case 'c': return checkKeyword(s, 1, 4, "lass", TOKEN_CLASS);
    case 'e': return checkKeyword(s, 1, 3, "lse", TOKEN_ELSE);
    case 'f': if (s->current - s->start > 1) {
        switch (s->start[1]) {
        case 'a': return checkKeyword(s, 2, 3, "lse", TOKEN_FALSE); case 'o': return checkKeyword(s, 2, 1, "r", TOKEN_FOR); case 'u': return checkKeyword(s, 2, 1, "n", TOKEN_FUN);
        }
    } break;
    case 'i': return checkKeyword(s, 1, 1, "f", TOKEN_IF);
    case 'n': return checkKeyword(s, 1, 2, "il", TOKEN_NIL);
    case 'o': return checkKeyword(s, 1, 1, "r", TOKEN_OR);
    case 'p': return checkKeyword(s, 1, 4, "rint", TOKEN_PRINT);
    case 'r': return checkKeyword(s, 1, 5, "eturn", TOKEN_RETURN);
    case 's': return checkKeyword(s, 1, 4, "uper", TOKEN_SUPER);
    case 't': if (s->current - s->start > 1) {
        switch (s->start[1]) {
        case 'h': return checkKeyword(s, 2, 2, "is", TOKEN_THIS); case 'r': return checkKeyword(s, 2, 2, "ue", TOKEN_TRUE);
        }
    } break;
    case 'v': return checkKeyword(s, 1, 2, "ar", TOKEN_VAR);
    case 'w': return checkKeyword(s, 1, 4, "hile", TOKEN_WHILE);
    }
    return TOKEN_IDENTIFIER;
}
static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}
static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
Token scanToken(Scanner* s) {
    skipWhitespace(s); s->start = s->current;
    if (isAtEnd(s)) return makeToken(s, TOKEN_EOF);
    char c = advance(s);
    if (isAlpha(c)) {
        while (isAlpha(peek(s)) || isDigit(peek(s))) advance(s); return makeToken(s, identifierType(s));
    }
    if (isDigit(c)) {
        while (isDigit(peek(s))) advance(s); if (peek(s) == '.' && isDigit(peekNext(s))) {
            advance(s); while (isDigit(peek(s))) advance(s);
        } return makeToken(s, TOKEN_NUMBER);
    }
    switch (c) {
    case '(': return makeToken(s, TOKEN_LEFT_PAREN); case ')': return makeToken(s, TOKEN_RIGHT_PAREN); case '{': return makeToken(s, TOKEN_LEFT_BRACE); case '}': return makeToken(s, TOKEN_RIGHT_BRACE); case ';': return makeToken(s, TOKEN_SEMICOLON); case ',': return makeToken(s, TOKEN_COMMA); case '.': return makeToken(s, TOKEN_DOT); case '-': return makeToken(s, TOKEN_MINUS); case '+': return makeToken(s, TOKEN_PLUS); case '/': return makeToken(s, TOKEN_SLASH); case '*': return makeToken(s, TOKEN_STAR);
    case '!': return makeToken(s, match(s, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG); case '=': return makeToken(s, match(s, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL); case '<': return makeToken(s, match(s, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS); case '>': return makeToken(s, match(s, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '"': while (peek(s) != '"' && !isAtEnd(s)) {
        if (peek(s) == '\n') s->line++; advance(s);
    } if (isAtEnd(s)) return errorToken(s, "Unterminated string."); advance(s); return makeToken(s, TOKEN_STRING);
    }
    return errorToken(s, "Unexpected character.");
}