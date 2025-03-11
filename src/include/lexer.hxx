#ifndef LEXER_HXX
#define LEXER_HXX

#include "token.hxx"

typedef struct {
        Token *hd;
        Token *tl;
} Lexer;

#define lexer_speek(l, i) lexer_peek((l), 0) && lexer_peek((l), 0)

Lexer lexer_init(const char *fp, char *src);
void lexer_append(Lexer *lexer, Token *tok);
Token *lexer_peek(Lexer *lexer, size_t p = 0);
Token *lexer_next(Lexer *lexer);
void lexer_discard(Lexer *lexer);
void lexer_dbg_dump(const Lexer *lexer);

#endif // LEXER_HXX
