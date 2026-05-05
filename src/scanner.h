// On-demand lexer. Produces tokens lazily for single-pass compilation;
// no full token list is ever built.

#ifndef btl_scanner_h
#define btl_scanner_h

typedef enum {
    // Single-character tokens
    BTL_TOKEN_LEFT_PAREN,      // (
    BTL_TOKEN_RIGHT_PAREN,     // )
    BTL_TOKEN_LEFT_BRACE,      // {
    BTL_TOKEN_RIGHT_BRACE,     // }
    BTL_TOKEN_LEFT_BRACKET,    // [
    BTL_TOKEN_RIGHT_BRACKET,   // ]
    BTL_TOKEN_COMMA,           // ,
    BTL_TOKEN_DOT,             // .
    BTL_TOKEN_MINUS,           // -
    BTL_TOKEN_PLUS,            // +
    BTL_TOKEN_SEMICOLON,       // ;
    BTL_TOKEN_SLASH,           // /
    BTL_TOKEN_STAR,            // *
    BTL_TOKEN_COLON,           // :
    BTL_TOKEN_PERCENT,         // %

    // One or two character tokens
    BTL_TOKEN_BANG,            // !
    BTL_TOKEN_BANG_EQUAL,      // !=
    BTL_TOKEN_EQUAL,           // =
    BTL_TOKEN_EQUAL_EQUAL,     // ==
    BTL_TOKEN_GREATER,         // >
    BTL_TOKEN_GREATER_EQUAL,   // >=
    BTL_TOKEN_LESS,            // <
    BTL_TOKEN_LESS_EQUAL,      // <=
    BTL_TOKEN_PLUS_PLUS,       // ++
    BTL_TOKEN_MINUS_MINUS,     // --
    BTL_TOKEN_PLUS_EQUAL,      // +=
    BTL_TOKEN_MINUS_EQUAL,     // -=
    BTL_TOKEN_STAR_EQUAL,      // *=
    BTL_TOKEN_SLASH_EQUAL,     // /=
    BTL_TOKEN_PERCENT_EQUAL,   // %=

    // Literals
    BTL_TOKEN_IDENTIFIER,      // variable/function names
    BTL_TOKEN_STRING,          // "string literal"
    BTL_TOKEN_NUMBER,          // 123, 45.67

    // Keywords
    BTL_TOKEN_AND,             // and
    BTL_TOKEN_AS,              // as
    BTL_TOKEN_BREAK,           // break
    BTL_TOKEN_CASE,            // case
    BTL_TOKEN_CLASS,           // class
    BTL_TOKEN_CONTINUE,        // continue
    BTL_TOKEN_DEFAULT,         // default
    BTL_TOKEN_DO,              // do
    BTL_TOKEN_ELSE,            // else
    BTL_TOKEN_FALSE,           // false
    BTL_TOKEN_FOR,             // for
    BTL_TOKEN_FUNC,            // func
    BTL_TOKEN_IF,              // if
    BTL_TOKEN_IMPORT,          // import
    BTL_TOKEN_IN,              // in
    BTL_TOKEN_NULL,            // null
    BTL_TOKEN_OR,              // or
    BTL_TOKEN_RETURN,          // return
    BTL_TOKEN_SUPER,           // super
    BTL_TOKEN_SWITCH,          // switch
    BTL_TOKEN_THIS,            // this
    BTL_TOKEN_TRUE,            // true
    BTL_TOKEN_VAR,             // var
    BTL_TOKEN_WHILE,           // while

    BTL_TOKEN_ERROR,
    BTL_TOKEN_EOF,
} BtlTokenType;

// Tokens reference the source string directly; no copying.
typedef struct {
    BtlTokenType type;
    const char* start;
    int length;
    int line;
} BtlToken;

typedef struct {
    const char* start;
    const char* current;
    int line;
} BtlScanner;

void btl_scanner_init(BtlScanner* scanner, const char* source);
BtlToken btl_scanner_scan_token(BtlScanner* scanner);

#endif
