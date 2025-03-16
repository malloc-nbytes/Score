#ifndef TOKEN_HXX
#define TOKEN_HXX

#include <stddef.h>

typedef enum {
        TOKEN_TYPE_EOF = 0,
        TOKEN_TYPE_INTEGER_LITERAL,
        TOKEN_TYPE_STRING_LITERAL,
        TOKEN_TYPE_IDENTIFIER,
        TOKEN_TYPE_KEYWORD,
        TOKEN_TYPE_TYPE,
        TOKEN_TYPE_LEFT_PARENTHESIS,
        TOKEN_TYPE_RIGHT_PARENTHESIS,
        TOKEN_TYPE_LEFT_CURLY_BRACKET,
        TOKEN_TYPE_RIGHT_CURLY_BRACKET,
        TOKEN_TYPE_LEFT_SQUARE_BRACKET,
        TOKEN_TYPE_RIGHT_SQUARE_BRACKET,
        TOKEN_TYPE_EQUALS,
        TOKEN_TYPE_SEMICOLON,
        TOKEN_TYPE_COLON,
        TOKEN_TYPE_ASTERISK,
        TOKEN_TYPE_DOUBLE_AMPERSAND,
        TOKEN_TYPE_DOUBLE_PIPE,
        TOKEN_TYPE_DOUBLE_EQUALS,
        TOKEN_TYPE_GREATERTHAN_EQUALS,
        TOKEN_TYPE_GREATERTHAN,
        TOKEN_TYPE_LESSTHAN_EQUALS,
        TOKEN_TYPE_LESSTHAN,
        TOKEN_TYPE_BANG_EQUALS,
        TOKEN_TYPE_PLUS,
        TOKEN_TYPE_MINUS,
        TOKEN_TYPE_FORWARD_SLASH,
        TOKEN_TYPE_PERCENT,
        TOKEN_TYPE_BANG,
        TOKEN_TYPE_COMMA,
        TOKEN_TYPE_TRIPLE_PERIOD,
        TOKEN_TYPE_PLUS_EQUALS,
        TOKEN_TYPE_MINUS_EQUALS,
        TOKEN_TYPE_ASTERISK_EQUALS,
        TOKEN_TYPE_FORWARD_SLASH_EQUALS,
        TOKEN_TYPE_PERCENT_EQUALS,
        TOKEN_TYPE_AMPERSAND_EQUALS,
        TOKEN_TYPE_PIPE_EQUALS,
        TOKEN_TYPE_CARET_EQUALS,
        TOKEN_TYPE_PERIOD,

        TOKEN_TYPE_UNKNOWN,
} Token_Type;

typedef struct Token {
        char *lx;
        Token_Type ty;
        size_t r, c;
        const char *fp;
        Token *next;

        ~Token(void);
} Token;

const char *token_type_to_cstr(Token_Type ty);
void token_dump(Token *t);
Token *token_alloc(char *sstart,
                   size_t send,
                   Token_Type ty,
                   size_t r,
                   size_t c,
                   const char *fp);

#endif // TOKEN_HXX
