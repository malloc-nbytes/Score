#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "codegen.hxx"
#include "types.hxx"
#include "err.hxx"
#include "utils.hxx"
#include "ds/array.hxx"
#include "ds/umap.hxx"

typedef struct {
        Token *id;
        Scr_Type rty;
        llvm::Function *value;
} Proc;

typedef struct {
        Token *id;
        Scr_Type ty;
} Var;

typedef struct {
        Array<Umap<char *, Var>> vs;
        Array<Umap<char *, Proc>> ps;
        llvm::LLVMContext *llctx;
        llvm::IRBuilder<> *bl;
        llvm::Module *md;
} Context;

static bool proc_in_scope(char *id, Context *ctx) {
        for (int i = (int)ctx->ps.length()-1; i >= 0; --i) {
                if (ctx->ps[i].has(id)) {
                        return true;
                }
        }
        return false;
}

static bool var_in_scope(char *id, Context *ctx) {
        for (int i = (int)ctx->vs.length()-1; i >= 0; --i) {
                if (ctx->vs[i].has(id)) {
                        return true;
                }
        }
        return false;
}

static void assert_identifier_in_scope(char *id, Context *ctx) {
        if (!var_in_scope(id, ctx)) {
                if (!proc_in_scope(id, ctx)) {
                        err_wargs("identifier %s is not declared", id);
                }
        }
}

static void assert_identifier_not_in_scope(char *id, Context *ctx) {
        if (var_in_scope(id, ctx) || proc_in_scope(id, ctx)) {
                err_wargs("identifier %s is already declared", id);
        }
}

static void assert_var_in_scope(char *id, Context *ctx) {
        if (!var_in_scope(id, ctx)) {
                err_wargs("identifier %s is not declared", id);
        }
}

static void assert_var_not_in_scope(char *id, Context *ctx) {
        if (var_in_scope(id, ctx)) {
                err_wargs("identifier %s is already declared", id);
        }
}

static void add_var_to_scope(Var *v, Context *ctx) {
        ctx->vs.back().add(v->id->lx, *v);
}

static void push_scope(Context *ctx) {
        auto m = Umap<char *, Var>([](char *s0, char *s1) {
                return !strcmp(s0, s1);
        });
        ctx->vs.add(m);
}

static void compile_stmt_block(Stmt_Block *s, Context *ctx) {
        (void)s;
        (void)ctx;
        assert(false);
}

static void compile_stmt_proc(Stmt_Proc *s, Context *ctx) {
        (void)s;
        (void)ctx;
        assert(0);
}

static void compile_stmt_let(Stmt_Let *s, Context *ctx) {
        (void)s;
        (void)ctx;
        assert(0);
}

static void compile_stmt_return(Stmt_Return *s, Context *ctx) {
        (void)s;
        (void)ctx;
        assert(0);
}

static void compile_stmt(Stmt *s, Context *ctx) {
        switch (s->ty) {
        case STMT_TYPE_LET: {
                compile_stmt_let((Stmt_Let *)s, ctx);
        } break;
        case STMT_TYPE_PROC: {
                compile_stmt_proc((Stmt_Proc *)s, ctx);
        } break;
        case STMT_TYPE_BLOCK: {
                compile_stmt_block((Stmt_Block *)s, ctx);
        } break;
        case STMT_TYPE_RETURN: {
                compile_stmt_return((Stmt_Return *)s, ctx);
        } break;
        default: {
                err_wargs("unknown statement: %d", (int)s->ty);
        } break;
        }
}

void codegen(Program *p) {
        Context *ctx = new Context;
        ctx->llctx = new llvm::LLVMContext();
        ctx->md = new llvm::Module("main", *ctx->llctx);
        ctx->bl = new llvm::IRBuilder<>(*ctx->llctx);

        for (size_t i = 0; i < p->len; ++i) {
                // Guaranteed to be top level statements
                // (guaranteed from the parser).
                compile_stmt(p->stmts[i], ctx);
        }
}
