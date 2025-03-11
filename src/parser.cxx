#include <utility>

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "parser.hxx"
#include "lexer.hxx"
#include "token.hxx"
#include "grammar.hxx"
#include "types.hxx"
#include "keywords.hxx"
#include "err.hxx"
#include "mem.hxx"
#include "utils.hxx"

static Stmt *parse_stmt(Lexer *lexer);
static Expr *parse_expr(Lexer *lexer);

static Token *expect(Lexer *lexer, Token_Type exp) {
        Token *hd = lexer_next(lexer);
        if (!hd) {
                err_wargs("exptected %s but got nothing",
                          token_type_to_cstr(exp));
        }
        if (hd->ty != exp) {
                err_wargs("exptected %s but got %s",
                          token_type_to_cstr(exp), token_type_to_cstr(hd->ty));
        }
        return hd;
}

static void expect_wo_eat(Lexer *lexer, Token_Type exp) {
        Token *hd = lexer_peek(lexer);
        if (!hd) {
                err_wargs("exptected %s but got nothing",
                          token_type_to_cstr(exp));
        }
        if (hd->ty != exp) {
                err_wargs("exptected %s but got %s",
                          token_type_to_cstr(exp), token_type_to_cstr(hd->ty));
        }
}

static Token *expectkw(Lexer *lexer, const char *kw) {
        Token *hd = lexer_next(lexer);
        if (!hd) {
                err_wargs("exptected keyword %s but got nothing", kw);
        }
        if (hd->ty != TOKEN_TYPE_KEYWORD || strcmp(hd->lx, kw) != 0) {
                err_wargs("exptected keyword %s but got %s",
                          kw, token_type_to_cstr(hd->ty));
        }
        return hd;
}

static Scr_Base_Type get_base_type(const Token *tok) {
        if (!strcmp(tok->lx, PRIMITIVE_I8)) return SCR_BASE_TYPE_I8;
        if (!strcmp(tok->lx, PRIMITIVE_I16)) return SCR_BASE_TYPE_I16;
        if (!strcmp(tok->lx, PRIMITIVE_I32)) return SCR_BASE_TYPE_I32;
        if (!strcmp(tok->lx, PRIMITIVE_I64)) return SCR_BASE_TYPE_I64;
        if (!strcmp(tok->lx, PRIMITIVE_U8)) return SCR_BASE_TYPE_U8;
        if (!strcmp(tok->lx, PRIMITIVE_U16)) return SCR_BASE_TYPE_U16;
        if (!strcmp(tok->lx, PRIMITIVE_U32)) return SCR_BASE_TYPE_U32;
        if (!strcmp(tok->lx, PRIMITIVE_U64)) return SCR_BASE_TYPE_U64;
        if (!strcmp(tok->lx, PRIMITIVE_U64)) return SCR_BASE_TYPE_U64;
        if (!strcmp(tok->lx, PRIMITIVE_STR)) return SCR_BASE_TYPE_STR;
        if (!strcmp(tok->lx, PRIMITIVE_VOID)) return SCR_BASE_TYPE_VOID;
        err_wargs("invalid type %s", tok->lx);
}

static void type_to_ptr(Scr_Type *ty) {
        Scr_Base_Type old_base = ty->base;
        Scr_Type *old_ptrn = ty->ptrn;
        ty->base = SCR_BASE_TYPE_PTR;
        ty->ptrn = new Scr_Type;
        ty->ptrn->base = old_base;
        ty->ptrn->ptrn = old_ptrn;
}

static Scr_Type parse_type(Lexer *lexer) {
        Scr_Base_Type basety = get_base_type(expect(lexer, TOKEN_TYPE_TYPE));
        Scr_Type type;
        type.base = basety;
        type.ptrn = nullptr;

        while (true) {
                if (lexer_speek(lexer, 0)->ty == TOKEN_TYPE_ASTERISK) {
                        type_to_ptr(&type);
                        lexer_discard(lexer); // *
                } else if (lexer_speek(lexer, 0)->ty == TOKEN_TYPE_LEFT_SQUARE_BRACKET) {
                        assert(0 && "array type parsing unimplemented");
                } else {
                        break;
                }
        }

        return type;
}

// Does not check for opening paren and does not consume
// the closing paren. These jobs are for the caller.
static Expr **parse_comma_sep_exprs(Lexer *lexer, size_t *len, size_t *cap, Token_Type closing_brace) {
        struct {
                Expr **data;
                size_t len, cap;
        } exprs = { nullptr, 0, 0 };

        while (lexer_speek(lexer, 0)->ty != closing_brace) {
                Expr *e = parse_expr(lexer);
                da_append(exprs.data, exprs.len, exprs.cap, Expr *, e);
                if (lexer_speek(lexer, 0)->ty == TOKEN_TYPE_COMMA) {
                        (void)lexer_discard(lexer); // ,
                } else {
                        expect_wo_eat(lexer, closing_brace);
                        break;
                }
        }

        *len = exprs.len;
        *cap = exprs.cap;
        return exprs.data;
}

static Expr *parse_primary_expr(Lexer *lexer) {
        Expr *left = nullptr;

        while (lexer_peek(lexer, 0)
               && (lexer_peek(lexer)->ty == TOKEN_TYPE_MINUS
               || lexer_peek(lexer)->ty == TOKEN_TYPE_BANG)) {
                assert(0 && "parsing unary expressions are unimplemented");
        }

        while (true) {
                Token *cur = lexer_peek(lexer);
                if (!cur) return left;

                switch (cur->ty) {
                case TOKEN_TYPE_IDENTIFIER: {
                        left = (Expr *)expr_ident_alloc(lexer_next(lexer));
                } break;
                case TOKEN_TYPE_LEFT_PARENTHESIS: {
                        lexer_discard(lexer); // (
                        if (left) {
                                size_t len = 0, cap = 0;
                                Expr **exprs = parse_comma_sep_exprs(lexer, &len, &cap, TOKEN_TYPE_RIGHT_PARENTHESIS);
                                left = (Expr *)expr_proc_call_alloc(left, exprs, len, cap);
                        } else {
                                left = parse_expr(lexer);
                        }
                        (void)expect(lexer, TOKEN_TYPE_RIGHT_PARENTHESIS);
                } break;
                case TOKEN_TYPE_INTEGER_LITERAL: {
                        left = (Expr *)expr_int_lit_alloc(lexer_next(lexer));
                } break;
                case TOKEN_TYPE_STRING_LITERAL: {
                        left = (Expr *)expr_str_lit_alloc(lexer_next(lexer));
                } break;
                case TOKEN_TYPE_KEYWORD: {
                        assert(0 && "keywords unimplemented");
                } break;
                default: return left;
                }
        }

        assert(0);
}

static Expr *parse_multiplicitate_expr(Lexer *lexer) {
        Expr *lhs = parse_primary_expr(lexer);
        Token *cur = lexer_peek(lexer, 0);
        while (cur && (cur->ty == TOKEN_TYPE_ASTERISK
                       || cur->ty == TOKEN_TYPE_FORWARD_SLASH
                       || cur->ty == TOKEN_TYPE_PERCENT)) {
                Token *op = lexer_next(lexer);
                Expr *rhs = parse_primary_expr(lexer);
                Expr_Bin *bin = expr_bin_alloc(lhs, op, rhs);
                lhs = (Expr *)bin;
                cur = lexer_peek(lexer, 0);
        }
        return lhs;
}

static Expr *parse_additive_expr(Lexer *lexer) {
        Expr *lhs = parse_multiplicitate_expr(lexer);
        Token *cur = lexer_peek(lexer, 0);
        while (cur && (cur->ty == TOKEN_TYPE_PLUS
                       || cur->ty == TOKEN_TYPE_MINUS)) {
                Token *op = lexer_next(lexer);
                Expr *rhs = parse_multiplicitate_expr(lexer);
                Expr_Bin *bin = expr_bin_alloc(lhs, op, rhs);
                lhs = (Expr *)bin;
                cur = lexer_peek(lexer, 0);
        }
        return lhs;
}

static Expr *parse_equalitative_expr(Lexer *lexer) {
        Expr *lhs = parse_additive_expr(lexer);
        Token *cur = lexer_peek(lexer, 0);
        while (cur && (cur->ty == TOKEN_TYPE_DOUBLE_EQUALS
                       || cur->ty == TOKEN_TYPE_GREATERTHAN_EQUALS
                       || cur->ty == TOKEN_TYPE_GREATERTHAN
                       || cur->ty == TOKEN_TYPE_LESSTHAN_EQUALS
                       || cur->ty == TOKEN_TYPE_LESSTHAN
                       || cur->ty == TOKEN_TYPE_BANG_EQUALS)) {
                Token *op = lexer_next(lexer);
                Expr *rhs = parse_additive_expr(lexer);
                Expr_Bin *bin = expr_bin_alloc(lhs, op, rhs);
                lhs = (Expr *)bin;
                cur = lexer_peek(lexer, 0);
        }
        return lhs;
}

static Expr *parse_logical_expr(Lexer *lexer) {
    Expr *lhs = parse_equalitative_expr(lexer);
    Token *cur = lexer_peek(lexer, 0);
    while (cur && (cur->ty == TOKEN_TYPE_DOUBLE_AMPERSAND
                   || cur->ty == TOKEN_TYPE_DOUBLE_PIPE)) {
        Token *op = lexer_next(lexer);
        Expr *rhs = parse_equalitative_expr(lexer);
        Expr_Bin *bin = expr_bin_alloc(lhs, op, rhs);
        lhs = (Expr *)bin;
        cur = lexer_peek(lexer, 0);
    }
    return lhs;
}

static Expr *parse_bitwise_expr(Lexer *lexer) {
        (void)lexer;
        assert(0 && "todo");
}

static Expr *parse_expr(Lexer *lexer) {
        return parse_logical_expr(lexer);
}

static Stmt_Let *parse_stmt_let(Lexer *lexer) {
        (void)expectkw(lexer, KEYWORD_LET);
        Token *id = expect(lexer, TOKEN_TYPE_IDENTIFIER);
        (void)expect(lexer, TOKEN_TYPE_COLON);
        Scr_Type type = parse_type(lexer);
        (void)expect(lexer, TOKEN_TYPE_EQUALS);
        Expr *e = parse_expr(lexer);
        (void)expect(lexer, TOKEN_TYPE_SEMICOLON);
        return stmt_let_alloc(id, type, e);
}

static void parse_function_args(Lexer *lexer,
                                Token ***ids,
                                size_t *ids_len,
                                size_t *ids_cap,
                                Scr_Type **types,
                                size_t *types_len,
                                size_t *types_cap) {
        expect(lexer, TOKEN_TYPE_LEFT_PARENTHESIS);

        if (lexer_speek(lexer, 0)->ty == TOKEN_TYPE_RIGHT_PARENTHESIS) {
                err("a proc accepting no args must have `void`");
        }

        if (lexer_speek(lexer, 0)->ty == TOKEN_TYPE_TYPE
            && !strcmp(lexer_peek(lexer)->lx, PRIMITIVE_VOID)) {
                lexer_discard(lexer); // void
                (void)expect(lexer, TOKEN_TYPE_RIGHT_PARENTHESIS);
                return;
        }

        while (lexer_speek(lexer, 0)->ty != TOKEN_TYPE_RIGHT_PARENTHESIS) {
                Token *id = expect(lexer, TOKEN_TYPE_IDENTIFIER);
                da_append(*ids, *ids_len, *ids_cap, Token *, id);

                (void)expect(lexer, TOKEN_TYPE_COLON);
                Scr_Type ty = parse_type(lexer);

                da_append(*types, *types_len, *types_cap, Scr_Type, ty);

                if (lexer_speek(lexer, 0)->ty != TOKEN_TYPE_COMMA) {
                        (void)expect(lexer, TOKEN_TYPE_RIGHT_PARENTHESIS);
                        break;
                } else {
                        (void)expect(lexer, TOKEN_TYPE_COMMA);
                }
        }
}

static Stmt_Block *parse_stmt_block(Lexer *lexer) {
        struct {
                Stmt **data = nullptr;
                size_t len, cap;
        } stmts = {nullptr, 0, 0};

        (void)expect(lexer, TOKEN_TYPE_LEFT_CURLY_BRACKET);

        while (lexer_speek(lexer, 0)->ty != TOKEN_TYPE_RIGHT_CURLY_BRACKET) {
                Stmt *stmt = parse_stmt(lexer);
                da_append(stmts.data, stmts.len, stmts.cap, Stmt *, stmt);
        }

        (void)expect(lexer, TOKEN_TYPE_RIGHT_CURLY_BRACKET);

        return stmt_block_alloc(stmts.data, stmts.len, stmts.cap);
}

static Stmt_Proc *parse_stmt_proc(Lexer *lexer) {
        lexer_discard(lexer); // proc
        Token *id = expect(lexer, TOKEN_TYPE_IDENTIFIER);

        struct {
                Token **data;
                size_t len, cap;
        } ids = { nullptr, 0, 0 };

        struct {
                Scr_Type *data;
                size_t len, cap;
        } types = { nullptr, 0, 0 };

        parse_function_args(lexer, &ids.data, &ids.len, &ids.cap,
                            &types.data, &types.len, &types.cap);

        (void)expect(lexer, TOKEN_TYPE_COLON);
        Scr_Type rtype = parse_type(lexer);
        Stmt_Block *block = parse_stmt_block(lexer);

        return stmt_proc_alloc(id, ids.data, types.data,
                               ids.len, ids.cap, rtype, block);
}

static Stmt *parse_stmt_from_keyword(Lexer *lexer) {
        Token *hd = lexer_peek(lexer);
        if (!strcmp(hd->lx, KEYWORD_LET)) {
                return (Stmt *)parse_stmt_let(lexer);
        } else if (!strcmp(hd->lx, KEYWORD_PROC)) {
                return (Stmt *)parse_stmt_proc(lexer);
        }
        assert(0);
}

static Stmt_Expr *parse_stmt_expr(Lexer *lexer) {
        Expr *e = parse_expr(lexer);
        (void)expect(lexer, TOKEN_TYPE_SEMICOLON);
        return stmt_expr_alloc(e);
}

static Stmt *parse_stmt(Lexer *lexer) {
        Token *hd = lexer_peek(lexer);

        switch (hd->ty) {
        case TOKEN_TYPE_KEYWORD: {
                return parse_stmt_from_keyword(lexer);
        } break;
        default: {
                return (Stmt *)parse_stmt_expr(lexer);
        } break;
        }
}

Program parse(Lexer *lexer) {
        (void)parse_bitwise_expr;

        Program p = { nullptr, 0, 0 };

        while (lexer_speek(lexer, 0)->ty != TOKEN_TYPE_EOF) {
                Stmt *s = parse_stmt(lexer);

                if (!IS_TOPLVL_STMT(*s)) {
                        err_wargs("statement %d is not a top level statement", (int)s->ty);
                }

                da_append(p.stmts, p.len, p.cap, Stmt *, s);
        }

        return p;
}
