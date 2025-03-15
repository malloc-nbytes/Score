#include <functional>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "lexer.hxx"
#include "token.hxx"
#include "ds/umap.hxx"
#include "keywords.hxx"
#include "err.hxx"
#include "utils.hxx"

static Umap<const char *, Token_Type> operators([](const char* s0, const char* s1) {
        return !strcmp(s0, s1);
});

static void init_operators(void) {
        operators.add("(", TOKEN_TYPE_LEFT_PARENTHESIS);
        operators.add(")", TOKEN_TYPE_RIGHT_PARENTHESIS);
        operators.add("{", TOKEN_TYPE_LEFT_CURLY_BRACKET);
        operators.add("}", TOKEN_TYPE_RIGHT_CURLY_BRACKET);
        operators.add("[", TOKEN_TYPE_LEFT_SQUARE_BRACKET);
        operators.add("]", TOKEN_TYPE_RIGHT_SQUARE_BRACKET);
        operators.add("=", TOKEN_TYPE_EQUALS);
        operators.add(";", TOKEN_TYPE_SEMICOLON);
        operators.add(":", TOKEN_TYPE_COLON);
        operators.add("*", TOKEN_TYPE_ASTERISK);
        operators.add("+", TOKEN_TYPE_PLUS);
        operators.add("-", TOKEN_TYPE_MINUS);
        operators.add("/", TOKEN_TYPE_FORWARD_SLASH);
        operators.add(",", TOKEN_TYPE_COMMA);
        operators.add("...", TOKEN_TYPE_TRIPLE_PERIOD);
        operators.add("==", TOKEN_TYPE_DOUBLE_EQUALS);
        operators.add("+=", TOKEN_TYPE_PLUS_EQUALS);
        operators.add("-=", TOKEN_TYPE_MINUS_EQUALS);
        operators.add("/=", TOKEN_TYPE_FORWARD_SLASH_EQUALS);
        operators.add("%=", TOKEN_TYPE_PERCENT_EQUALS);
        operators.add("&=", TOKEN_TYPE_AMPERSAND_EQUALS);
        operators.add("|=", TOKEN_TYPE_PIPE_EQUALS);
        operators.add("^=", TOKEN_TYPE_CARET_EQUALS);
}

static Token_Type determine_operator_type(const char *s, size_t end, size_t *len) {
        *len = 0;

        struct {
                char *data;
                size_t len, cap;
        } buf = {nullptr, 0, 0};

        for (size_t i = 0; i < end; ++i) {
                da_append(buf.data, buf.len, buf.cap, char, s[i]);
        }
        da_append(buf.data, buf.len, buf.cap, char, '\0');
        --buf.len;

        while (buf.len > 0) {
                Token_Type *ty = operators.get(buf.data);
                if (ty) {
                        *len = buf.len;
                        return *ty;
                } else {
                        buf.data[--buf.len] = '\0';
                }
        }

        *len = end;
        return TOKEN_TYPE_UNKNOWN;
}

void lexer_dbg_dump(const Lexer *lexer) {
        Token *hd = lexer->hd;
        while (hd) {
                token_dump(hd);
                hd = hd->next;
        }
}

void lexer_append(Lexer *lexer, Token *tok) {
        if (!lexer->hd || !lexer->tl) {
                lexer->hd = lexer->tl = tok;
        } else {
                Token *tmp = lexer->tl;
                lexer->tl = tok;
                tmp->next = lexer->tl;
        }
}

void lexer_discard(Lexer *lexer) {
        if (!lexer->hd) return;
        lexer->hd = lexer->hd->next;
}

Token *lexer_next(Lexer *lexer) {
        Token *t = lexer->hd;
        if (!t) return NULL;
        lexer->hd = lexer->hd->next;
        return t;
}

Token *lexer_peek(Lexer *lexer, size_t p) {
        Token *it = lexer->hd;
        for (size_t i = 0; i < p && it; ++i) it = it->next;
        return it;
}

static size_t consume_while(char *s, std::function<int(int)> pred) {
        size_t i = 0;
        bool skip = false;

        for (i = 0; s[i]; ++i) {
                if (!skip && !pred(s[i])) {
                        return i;
                }
                if (skip && s[i] == '\\') {
                        skip = false;
                } else if (s[i] == '\\') {
                        skip = true;
                } else {
                        skip = false;
                }
        }
        return i;
}

Lexer lexer_init(const char *fp, char *src) {
        init_operators();

        Lexer lexer = { NULL, NULL };

        size_t row = 1, col = 1, i = 0;

        while (src[i]) {
                char ch = src[i];

                if (ch == '-' && src[i+1] && src[i+1] == '-') {
                        ++i, ++col; // #
                        size_t len = consume_while(src + i, [](int c) {
                                return c != '\n';
                        });
                        i += len, col += len;
                } else if (ch == ' ' || ch == '\t') {
                        ++col, ++i;
                } else if (ch == '\n' || ch == '\r') {
                        col = 1, ++row, ++i;
                } else if (ch == '"') {
                        ++i, ++col; // "
                        size_t len = consume_while(src + i, [](int c) {
                                return c != '"';
                        });

                        Token *tok = token_alloc(src + i, len,
                                     TOKEN_TYPE_STRING_LITERAL,
                                     row, col, fp);
                        lexer_append(&lexer, tok);

                        i += len; col += len; // string literal
                        ++i, ++col; // "
                } else if (ch == '\'') {
                        assert(0 && "unimplemented");
                } else if (isdigit(ch)) {
                        size_t len = consume_while(src + i, [](int c) {
                                return isdigit(c);
                        });
                        Token *tok = token_alloc(src + i, len,
                                     TOKEN_TYPE_INTEGER_LITERAL,
                                     row, col, fp);
                        lexer_append(&lexer, tok);
                        i += len, col += len;
                } else if (isalpha(ch) || ch == '_') {
                        size_t len = consume_while(src + i, [](int c) {
                                return isalnum(c);
                        });

                        Token *tok = token_alloc(src + i, len,
                                                 TOKEN_TYPE_IDENTIFIER, row, col, fp);
                        if (is_keyword(tok->lx)) {
                                tok->ty = TOKEN_TYPE_KEYWORD;
                        } else if (is_type(tok->lx)) {
                                tok->ty = TOKEN_TYPE_TYPE;
                        }
                        lexer_append(&lexer, tok);

                        i += len, col += len;
                } else {
                        size_t end = consume_while(src + i, [](int c) {
                                return !isalnum(c) && c != '_' && c != ' ' && c != '\t' && c != '\n';
                        });
                        size_t len = 0;
                        Token_Type ty = determine_operator_type(src + i, end, &len);
                        Token *tok = token_alloc(src + i, len,
                                                 ty, row, col, fp);

                        if (ty == TOKEN_TYPE_UNKNOWN) {
                                err_wargs("unknown operator `%s`", tok->lx);
                        }

                        lexer_append(&lexer, tok);
                        i += len, col += len;
                }
        }

        Token *tok = token_alloc(src, 0, TOKEN_TYPE_EOF, row, col, fp);
        lexer_append(&lexer, tok);

        return lexer;
}
