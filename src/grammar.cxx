#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "grammar.hxx"
#include "types.hxx"
#include "token.hxx"
#include "mem.hxx"
#include "err.hxx"

///////////////////
// Statements /////
///////////////////

Stmt_Def *stmt_def_alloc(Stmt_Proc *proto) {
        Stmt_Def *d = new Stmt_Def;
        d->base.ty = STMT_TYPE_DEF;
        d->proto = proto;
        return d;
}

Stmt_Return *stmt_return_alloc(Expr *e) {
        Stmt_Return *r = new Stmt_Return;
        r->base.ty = STMT_TYPE_RETURN;
        r->e = e;
        return r;
}

Stmt_Expr *stmt_expr_alloc(Expr *e) {
        Stmt_Expr *s = new Stmt_Expr;
        s->base.ty = STMT_TYPE_EXPR;
        s->e = e;
        return s;
}

Stmt_Block *stmt_block_alloc(Stmt **stmts, size_t len, size_t cap) {
        Stmt_Block *b = new Stmt_Block;
        b->base.ty = STMT_TYPE_BLOCK;
        b->stmts = stmts;
        b->len = len;
        b->cap = cap;
        return b;
}

Stmt_Proc *stmt_proc_alloc(Token *id,
                           Token **ids,
                           Scr_Type *id_types,
                           size_t len,
                           size_t cap,
                           Scr_Type rtype,
                           Stmt_Block *block,
                           bool variadic) {
        Stmt_Proc *p = new Stmt_Proc;
        p->base.ty = STMT_TYPE_PROC;
        p->id = id;
        p->args.ids = ids;
        p->args.types = id_types;
        p->args.len = len;
        p->args.cap = cap;
        p->rtype = rtype;
        p->block = block;
        p->variadic = variadic;
        return p;
}

Stmt_Let *stmt_let_alloc(Token *id, Scr_Type type, Expr *e) {
        Stmt_Let *s = new Stmt_Let;
        s->base.ty = STMT_TYPE_LET;
        s->id = id;
        s->type = type;
        s->e = e;
        return s;
}

///////////////////
// Expressions ////
///////////////////

Expr_Proc_Call *expr_proc_call_alloc(Expr *left,
                                     Expr **exprs,
                                     size_t len,
                                     size_t cap) {
        Expr_Proc_Call *e = new Expr_Proc_Call;
        e->base.ty = EXPR_TYPE_PROC_CALL;
        e->left = left;
        e->args.exprs = exprs;
        e->args.len = len;
        e->args.cap = cap;
        return e;
}

Expr_Ident *expr_ident_alloc(Token *id) {
        Expr_Ident *ident = new Expr_Ident;
        ident->base.ty = EXPR_TYPE_IDENT;
        ident->id = id;
        return ident;
}

Expr_Str_Lit *expr_str_lit_alloc(Token *s) {
        Expr_Str_Lit *str = new Expr_Str_Lit;
        str->base.ty = EXPR_TYPE_STR_LIT;
        str->s = s;
        return str;
}

Expr_Int_Lit *expr_int_lit_alloc(Token *i) {
        int x = atoi(i->lx);
        Expr_Int_Lit *il = new Expr_Int_Lit;
        il->base.ty = EXPR_TYPE_INT_LIT;
        il->i = x;
        return il;
}

Expr_Bin *expr_bin_alloc(Expr *l, Token *op, Expr *r) {
        Expr_Bin *b = new Expr_Bin;
        b->base.ty = EXPR_TYPE_BIN;
        b->l = l;
        b->op = op;
        b->r = r;
        return b;
}

/*** AST Debug Printing ***/

static void dump_expr(Expr *e);
static void dump_stmt(Stmt *s, int pad);

void spaces(int pad) {
        for (int i = 0; i < pad*2; ++i) putchar(' ');
}

static void dump_expr_str_lit(Expr_Ident *e) {
        printf("%s", e->id->lx);
}

static void dump_expr_str_lit(Expr_Str_Lit *e) {
        printf("\"%s\"", e->s->lx);
}

static void dump_expr_int_lit(Expr_Int_Lit *e) {
        printf("%d", e->i);
}

static void dump_expr_bin(Expr_Bin *e) {
        printf("(");
        dump_expr(e->l);
        printf(" %s ", e->op->lx);
        dump_expr(e->r);
        printf(")");
}

static void dump_expr_proc_call(Expr_Proc_Call *e) {
        dump_expr(e->left);
        putchar('(');
        for (size_t i = 0; i < e->args.len; ++i) {
                if (i != 0) {
                        printf(", ");
                }
                dump_expr(e->args.exprs[i]);
        }
        putchar(')');
}

static void dump_expr(Expr *e) {
        switch (e->ty) {
        case EXPR_TYPE_BIN: {
                dump_expr_bin((Expr_Bin *)e);
        } break;
        case EXPR_TYPE_UNARY: {
                assert(0);
        } break;
        case EXPR_TYPE_MUT: {
                assert(0);
        } break;
        case EXPR_TYPE_IDENT: {
                dump_expr_str_lit((Expr_Ident *)e);
        } break;
        case EXPR_TYPE_STR_LIT: {
                dump_expr_str_lit((Expr_Str_Lit *)e);
        } break;
        case EXPR_TYPE_INT_LIT: {
                dump_expr_int_lit((Expr_Int_Lit *)e);
        } break;
        case EXPR_TYPE_PROC_CALL: {
                dump_expr_proc_call((Expr_Proc_Call *)e);
        } break;
        default: {
                err_wargs("unhandled expr type %d", (int)e->ty);
        } break;
        }
}

static void dump_stmt_let(Stmt_Let *s, int pad) {
        (void)pad;
        printf("LET %s: ", s->id->lx);
        scr_type_dump(&s->type, false);
        printf(" = ");
        dump_expr(s->e);
}

static void dump_stmt_block(Stmt_Block *s, int pad) {
        printf("{\n");
        for (size_t i = 0; i < s->len; ++i) {
                dump_stmt(s->stmts[i], pad+1);
        }
        printf("}\n");
}

static void dump_stmt_proc(Stmt_Proc *s, int pad) {
        (void)pad;
        printf("proc %s(", s->id->lx);
        for (size_t i = 0; i < s->args.len; ++i) {
                if (i != 0) {
                        printf(", ");
                }
                printf("%s: ", s->args.ids[i]->lx);
                scr_type_dump(&s->args.types[i], false);
        }
        if (s->variadic) {
                printf(", ...");
        }
        printf("): ");
        scr_type_dump(&s->rtype, false);
        putchar(' ');
        if (s->block) {
                dump_stmt_block(s->block, pad);
        }
}

static void dump_stmt_expr(Stmt_Expr *s, int pad) {
        (void)pad;
        dump_expr(s->e);
}

static void dump_stmt_return(Stmt_Return *s, int pad) {
        (void)pad;
        printf("RETURN ");
        dump_expr(s->e);
}

static void dump_stmt_def(Stmt_Def *s, int pad) {
        printf("DEF ");
        dump_stmt_proc(s->proto, pad);
}

static void dump_stmt(Stmt *s, int pad) {
        spaces(pad);

        switch (s->ty) {
        case STMT_TYPE_EXPR: {
                dump_stmt_expr((Stmt_Expr *)s, pad);
        } break;
        case STMT_TYPE_LET: {
                dump_stmt_let((Stmt_Let *)s, pad);
        } break;
        case STMT_TYPE_PROC: {
                dump_stmt_proc((Stmt_Proc *)s, pad);
        } break;
        case STMT_TYPE_BLOCK: {
                dump_stmt_block((Stmt_Block *)s, pad);
        } break;
        case STMT_TYPE_RETURN: {
                dump_stmt_return((Stmt_Return *)s, pad);
        } break;
        case STMT_TYPE_DEF: {
                dump_stmt_def((Stmt_Def *)s, pad);
        } break;
        default: {
                err_wargs("unhandled stmt type %d", (int)s->ty);
        } break;
        }
        putchar('\n');
}

void program_dump(Program *p) {
        for (size_t i = 0; i < p->len; ++i) {
                dump_stmt(p->stmts[i], 0);
        }
}
