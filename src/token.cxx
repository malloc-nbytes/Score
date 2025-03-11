#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "token.hxx"
#include "mem.hxx"
#include "err.hxx"
#include "utils.hxx"

const char *token_type_to_cstr(Token_Type ty) {
        switch (ty) {
        case TOKEN_TYPE_EOF: return "TOKEN_TYPE_EOF";
        case TOKEN_TYPE_INTEGER_LITERAL: return "TOKEN_TYPE_INTEGER_LITERAL";
        case TOKEN_TYPE_STRING_LITERAL: return "TOKEN_TYPE_STRING_LITERAL";
        case TOKEN_TYPE_IDENTIFIER: return "TOKEN_TYPE_IDENTIFIER";
        case TOKEN_TYPE_KEYWORD: return "TOKEN_TYPE_KEYWORD";
        case TOKEN_TYPE_TYPE: return "TOKEN_TYPE_TYPE";
        case TOKEN_TYPE_LEFT_PARENTHESIS: return "TOKEN_TYPE_LEFT_PARENTHESIS";
        case TOKEN_TYPE_RIGHT_PARENTHESIS: return "TOKEN_TYPE_RIGHT_PARENTHESIS";
        case TOKEN_TYPE_LEFT_CURLY_BRACKET: return "TOKEN_TYPE_LEFT_CURLY_BRACKET";
        case TOKEN_TYPE_RIGHT_CURLY_BRACKET: return "TOKEN_TYPE_RIGHT_CURLY_BRACKET";
        case TOKEN_TYPE_LEFT_SQUARE_BRACKET: return "TOKEN_TYPE_LEFT_SQUARE_BRACKET";
        case TOKEN_TYPE_RIGHT_SQUARE_BRACKET: return "TOKEN_TYPE_RIGHT_SQUARE_BRACKET";
        case TOKEN_TYPE_EQUALS: return "TOKEN_TYPE_EQUALS";
        case TOKEN_TYPE_SEMICOLON: return "TOKEN_TYPE_SEMICOLON";
        case TOKEN_TYPE_UNKNOWN: return "TOKEN_TYPE_UNKNOWN";
        case TOKEN_TYPE_COLON: return "TOKEN_TYPE_COLON";
        case TOKEN_TYPE_ASTERISK: return "TOKEN_TYPE_ASTERISK";
        case TOKEN_TYPE_DOUBLE_AMPERSAND: return "TOKEN_TYPE_DOUBLE_AMPERSAND";
        case TOKEN_TYPE_DOUBLE_PIPE: return "TOKEN_TYPE_DOUBLE_PIPE";
        case TOKEN_TYPE_DOUBLE_EQUALS: return "TOKEN_TYPE_DOUBLE_EQUALS";
        case TOKEN_TYPE_GREATERTHAN_EQUALS: return "TOKEN_TYPE_GREATERTHAN_EQUALS";
        case TOKEN_TYPE_GREATERTHAN: return "TOKEN_TYPE_GREATERTHAN";
        case TOKEN_TYPE_LESSTHAN_EQUALS: return "TOKEN_TYPE_LESSTHAN_EQUALS";
        case TOKEN_TYPE_LESSTHAN: return "TOKEN_TYPE_LESSTHAN";
        case TOKEN_TYPE_BANG_EQUALS: return "TOKEN_TYPE_BANG_EQUALS";
        case TOKEN_TYPE_PLUS: return "TOKEN_TYPE_PLUS";
        case TOKEN_TYPE_MINUS: return "TOKEN_TYPE_MINUS";
        case TOKEN_TYPE_FORWARD_SLASH: return "TOKEN_TYPE_FORWARD_SLASH";
        case TOKEN_TYPE_PERCENT: return "TOKEN_TYPE_PERCENT";
        case TOKEN_TYPE_BANG: return "TOKEN_TYPE_BANG";
        case TOKEN_TYPE_COMMA: return "TOKEN_TYPE_COMMA";
        case TOKEN_TYPE_TRIPLE_PERIOD: return "TOKEN_TYPE_TRIPLE_PERIOD";
        default: {
                err_wargs("unknown token type: %d", (int)ty);
        } break;
        }
}

void token_dump(Token *t) {
        printf("[Token (lx='%s', ty=%s, r=%zu, c=%zu)]\n",
               t->lx, token_type_to_cstr(t->ty), t->r, t->c);
}

Token::~Token(void) {
        free(this->lx);
}

Token *token_alloc(char *sstart,
                   size_t send,
                   Token_Type ty,
                   size_t r,
                   size_t c,
                   const char *fp) {
        Token *tok = new Token;

        struct {
                char *data;
                size_t len, cap;
        } buf = { NULL, 0, 0 };

        size_t i = 0;
        while (*(sstart + i) && i < send) {
                da_append(buf.data, buf.len, buf.cap, char, *(sstart + i));
                ++i;
        }
        da_append(buf.data, buf.len, buf.cap, char, '\0');

        tok->lx = strdup(buf.data);
        tok->ty = ty;
        tok->r = r;
        tok->c = c;
        tok->fp = fp;
        tok->next = NULL;

        free(buf.data);

        return tok;
}
