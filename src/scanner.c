// Trie-style keyword recognition: dispatch on first char, then memcmp the rest.

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

void btl_scanner_init(BtlScanner* scanner, const char* source) {
    scanner->start = source;
    scanner->current = source;
    scanner->line = 1;
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c == '_';
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isHexDigit(char c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

static bool isBinDigit(char c) {
    return c == '0' || c == '1';
}

static bool isAtEnd(BtlScanner* scanner) {
    return *scanner->current == '\0';
}

static char advance(BtlScanner* scanner) {
    scanner->current++;
    return scanner->current[-1];
}

static char peek(BtlScanner* scanner) {
    return *scanner->current;
}

static char peekNext(BtlScanner* scanner) {
    if (isAtEnd(scanner)) return '\0';
    return scanner->current[1];
}

static bool match(BtlScanner* scanner, char expected) {
    if (isAtEnd(scanner)) return false;
    if (*scanner->current != expected) return false;
    scanner->current++;
    return true;
}

static BtlToken makeToken(BtlScanner* scanner, BtlTokenType type) {
    BtlToken token;
    token.type = type;
    token.start = scanner->start;
    token.length = (int)(scanner->current - scanner->start);
    token.line = scanner->line;
    return token;
}

static BtlToken errorToken(BtlScanner* scanner, const char* message) {
    BtlToken token;
    token.type = BTL_TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = scanner->line;
    return token;
}

static void skipWhitespace(BtlScanner* scanner) {
    for (;;) {
        char c = peek(scanner);
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance(scanner);
                break;

            case '\n':
                scanner->line++;
                advance(scanner);
                break;

            case '/':
                if (peekNext(scanner) == '/') {
                    while (peek(scanner) != '\n' && !isAtEnd(scanner)) {
                        advance(scanner);
                    }
                } else {
                    return;
                }
                break;

            default:
                return;
        }
    }
}

static BtlTokenType checkKeyword(BtlScanner* scanner, int start, int length,
                                  const char* rest, BtlTokenType type) {
    if (scanner->current - scanner->start == start + length &&
        memcmp(scanner->start + start, rest, length) == 0) {
        return type;
    }
    return BTL_TOKEN_IDENTIFIER;
}

static BtlTokenType identifierType(BtlScanner* scanner) {
    switch (scanner->start[0]) {
        case 'a':
            // "and", "as"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'n': return checkKeyword(scanner, 2, 1, "d", BTL_TOKEN_AND);
                    case 's': return checkKeyword(scanner, 2, 0, "", BTL_TOKEN_AS);
                }
            }
            break;

        case 'b':
            // "break"
            return checkKeyword(scanner, 1, 4, "reak", BTL_TOKEN_BREAK);

        case 'c':
            // "case", "class", "continue"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'a': return checkKeyword(scanner, 2, 2, "se", BTL_TOKEN_CASE);
                    case 'l': return checkKeyword(scanner, 2, 3, "ass", BTL_TOKEN_CLASS);
                    case 'o': return checkKeyword(scanner, 2, 6, "ntinue", BTL_TOKEN_CONTINUE);
                }
            }
            break;

        case 'd':
            // "do", "default"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'o': return checkKeyword(scanner, 2, 0, "", BTL_TOKEN_DO);
                    case 'e': return checkKeyword(scanner, 2, 5, "fault", BTL_TOKEN_DEFAULT);
                }
            }
            break;

        case 'e':
            // "else"
            return checkKeyword(scanner, 1, 3, "lse", BTL_TOKEN_ELSE);

        case 'f':
            // "false", "for", "func"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'a': return checkKeyword(scanner, 2, 3, "lse", BTL_TOKEN_FALSE);
                    case 'o': return checkKeyword(scanner, 2, 1, "r", BTL_TOKEN_FOR);
                    case 'u': return checkKeyword(scanner, 2, 2, "nc", BTL_TOKEN_FUNC);
                }
            }
            break;

        case 'i':
            // "if", "import", "in"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'f': return checkKeyword(scanner, 2, 0, "", BTL_TOKEN_IF);
                    case 'm': return checkKeyword(scanner, 2, 4, "port", BTL_TOKEN_IMPORT);
                    case 'n': return checkKeyword(scanner, 2, 0, "", BTL_TOKEN_IN);
                }
            }
            break;

        case 'n':
            // "null"
            return checkKeyword(scanner, 1, 3, "ull", BTL_TOKEN_NULL);

        case 'o':
            // "or"
            return checkKeyword(scanner, 1, 1, "r", BTL_TOKEN_OR);

        case 'r':
            // "return"
            return checkKeyword(scanner, 1, 5, "eturn", BTL_TOKEN_RETURN);

        case 's':
            // "super", "switch"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'u':
                        if (scanner->current - scanner->start > 2) {
                            switch (scanner->start[2]) {
                                case 'p': return checkKeyword(scanner, 3, 2, "er", BTL_TOKEN_SUPER);
                            }
                        }
                        break;
                    case 'w': return checkKeyword(scanner, 2, 4, "itch", BTL_TOKEN_SWITCH);
                }
            }
            break;

        case 't':
            // "this", "true"
            if (scanner->current - scanner->start > 1) {
                switch (scanner->start[1]) {
                    case 'h': return checkKeyword(scanner, 2, 2, "is", BTL_TOKEN_THIS);
                    case 'r': return checkKeyword(scanner, 2, 2, "ue", BTL_TOKEN_TRUE);
                }
            }
            break;

        case 'v':
            // "var"
            return checkKeyword(scanner, 1, 2, "ar", BTL_TOKEN_VAR);

        case 'w':
            // "while"
            return checkKeyword(scanner, 1, 4, "hile", BTL_TOKEN_WHILE);
    }

    return BTL_TOKEN_IDENTIFIER;
}

BtlToken btl_scanner_scan_token(BtlScanner* scanner) {
    skipWhitespace(scanner);
    scanner->start = scanner->current;

    if (isAtEnd(scanner)) {
        return makeToken(scanner, BTL_TOKEN_EOF);
    }

    char c = advance(scanner);

    if (isAlpha(c)) {
        while (isAlpha(peek(scanner)) || isDigit(peek(scanner))) {
            advance(scanner);
        }
        return makeToken(scanner, identifierType(scanner));
    }

    if (isDigit(c)) {
        if (c == '0' && (peek(scanner) == 'x' || peek(scanner) == 'X')) {
            // Hex literal: 0xFF
            advance(scanner);
            while (isHexDigit(peek(scanner))) {
                advance(scanner);
            }
        } else if (c == '0' && (peek(scanner) == 'b' || peek(scanner) == 'B')) {
            // Binary literal: 0b1010
            advance(scanner);
            while (isBinDigit(peek(scanner))) {
                advance(scanner);
            }
        } else {
            while (isDigit(peek(scanner))) {
                advance(scanner);
            }
            if (peek(scanner) == '.' && isDigit(peekNext(scanner))) {
                advance(scanner);
                while (isDigit(peek(scanner))) {
                    advance(scanner);
                }
            }
        }
        return makeToken(scanner, BTL_TOKEN_NUMBER);
    }

    switch (c) {
        case '(': return makeToken(scanner, BTL_TOKEN_LEFT_PAREN);
        case ')': return makeToken(scanner, BTL_TOKEN_RIGHT_PAREN);
        case '{': return makeToken(scanner, BTL_TOKEN_LEFT_BRACE);
        case '}': return makeToken(scanner, BTL_TOKEN_RIGHT_BRACE);
        case '[': return makeToken(scanner, BTL_TOKEN_LEFT_BRACKET);
        case ']': return makeToken(scanner, BTL_TOKEN_RIGHT_BRACKET);
        case ';': return makeToken(scanner, BTL_TOKEN_SEMICOLON);
        case ',': return makeToken(scanner, BTL_TOKEN_COMMA);
        case '.': return makeToken(scanner, BTL_TOKEN_DOT);
        case ':': return makeToken(scanner, BTL_TOKEN_COLON);

        case '+':
            if (match(scanner, '+')) return makeToken(scanner, BTL_TOKEN_PLUS_PLUS);
            if (match(scanner, '=')) return makeToken(scanner, BTL_TOKEN_PLUS_EQUAL);
            return makeToken(scanner, BTL_TOKEN_PLUS);

        case '-':
            if (match(scanner, '-')) return makeToken(scanner, BTL_TOKEN_MINUS_MINUS);
            if (match(scanner, '=')) return makeToken(scanner, BTL_TOKEN_MINUS_EQUAL);
            return makeToken(scanner, BTL_TOKEN_MINUS);

        case '*':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_STAR_EQUAL : BTL_TOKEN_STAR);

        case '/':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_SLASH_EQUAL : BTL_TOKEN_SLASH);

        case '%':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_PERCENT_EQUAL : BTL_TOKEN_PERCENT);

        case '!':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_BANG_EQUAL : BTL_TOKEN_BANG);

        case '=':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_EQUAL_EQUAL : BTL_TOKEN_EQUAL);

        case '<':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_LESS_EQUAL : BTL_TOKEN_LESS);

        case '>':
            return makeToken(scanner, match(scanner, '=') ? BTL_TOKEN_GREATER_EQUAL : BTL_TOKEN_GREATER);

        case '"': {
            while (peek(scanner) != '"' && !isAtEnd(scanner)) {
                if (peek(scanner) == '\n') {
                    scanner->line++;
                }
                advance(scanner);
            }

            if (isAtEnd(scanner)) {
                return errorToken(scanner, "Unterminated string.");
            }

            advance(scanner);
            return makeToken(scanner, BTL_TOKEN_STRING);
        }
    }

    return errorToken(scanner, "Unexpected character.");
}
