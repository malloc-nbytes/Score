#include <vector>

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
        llvm::Value *value;
} Var;

typedef struct {
        Array<Umap<char *, Var>> vs;
        Array<Umap<char *, Proc>> ps;
        llvm::LLVMContext *llctx;
        llvm::IRBuilder<> *bl;
        llvm::Module *md;
} Context;

static void compile_stmt(Stmt *s, Context *ctx);

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

static void pop_scope(Context *ctx) {
        if (ctx->vs.length() == 0) {
                err("cannot pop scope, out of length");
        }
        ctx->vs.pop_back();
}

static llvm::Type *scr_type_to_llvm_type(Scr_Type ty, Context *ctx) {
        switch (ty.base) {
        case SCR_BASE_TYPE_I32:
                return llvm::Type::getInt32Ty(*(ctx->llctx));
        case SCR_BASE_TYPE_STR:
                return llvm::Type::getInt8PtrTy(*(ctx->llctx));
        default: {
                err_wargs("unhandled type %d", (int)ty.base);
        }
        }
        return nullptr; // unreachable
}

static llvm::Function *gen_proc_proto(Stmt_Proc *s, Context *ctx) {
        // std::vector sad :(
        std::vector<llvm::Type *> types;

        for (size_t i = 0; i < s->args.len; ++i) {
                llvm::Type *llty = scr_type_to_llvm_type(s->rtype, ctx);
                types.push_back(llty);
        }

        // If the function is variadic, set the last argument type to be a pointer type (e.g., void*)
        llvm::FunctionType *ft = llvm::FunctionType::get(scr_type_to_llvm_type(s->rtype, ctx),
                                                         types, s->variadic);

        llvm::Function *f = llvm::Function::Create(
                ft, llvm::Function::ExternalLinkage, s->id->lx, ctx->md);

        size_t idx = 0;
        for (auto &a : f->args()) {
                a.setName(s->args.ids[idx++]->lx);
        }

        return f;
}

static llvm::Value *gen_expr_int_lit(Expr_Int_Lit *e, Context *ctx) {
        return llvm::ConstantInt::get(*(ctx->llctx),
                                      llvm::APInt(/*bits=*/32, (uint32_t)e->i));
}

static llvm::Value *compile_expr(Expr *e, Context *ctx) {
        switch (e->ty) {
        case EXPR_TYPE_UNARY: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_MUT: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_IDENT: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_STR_LIT: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_INT_LIT: {
                return gen_expr_int_lit((Expr_Int_Lit *)e, ctx);
        } break;
        case EXPR_TYPE_PROC_CALL: {
                assert(0 && "todo");
        } break;
        default: {
                err_wargs("unknown expression type %d", (int)e->ty);
        } break;
        }
}

static void compile_stmt_block(Stmt_Block *s, Context *ctx) {
        push_scope(ctx);
        for (size_t i = 0; i < s->len; ++i) {
                compile_stmt(s->stmts[i], ctx);
        }
        pop_scope(ctx);
}

static llvm::Function *compile_stmt_proc(Stmt_Proc *s, Context *ctx) {
        llvm::Function *existing_function = ctx->md->getFunction(s->id->lx);

        if (!existing_function) {
                existing_function = gen_proc_proto(s, ctx);
        }
        if (!existing_function) {
                return nullptr;
        }
        if (!existing_function->empty()) {
                err_wargs("function %s cannot be redefined", s->id->lx);
        }

        llvm::BasicBlock *bb = llvm::BasicBlock::Create(*(ctx->llctx), "entry", existing_function);
        ctx->bl->SetInsertPoint(bb);

        push_scope(ctx);

        for (auto &arg : existing_function->args()) {
                Var v = { s->args.ids[arg.getArgNo()], s->args.types[arg.getArgNo()], nullptr };
                add_var_to_scope(&v, ctx);
        }

        compile_stmt_block(s->block, ctx);

        pop_scope(ctx);

        llvm::verifyFunction(*existing_function);

        return existing_function;
}

static void compile_stmt_let(Stmt_Let *s, Context *ctx) {
        llvm::Type *llty = scr_type_to_llvm_type(s->type, ctx);

        // Create an alloca instruction for the new variable
        llvm::AllocaInst *alloca_inst = ctx->bl->CreateAlloca(llty, nullptr, s->id->lx);

        // Gen code for initial value expression
        llvm::Value *init_value = compile_expr(s->e, ctx);
        if (!init_value) {
                err_wargs("failed to compile expression for identifier %s", s->id->lx);
        }

        Var new_var = { s->id, s->type, init_value };
        add_var_to_scope(&new_var, ctx);

        ctx->bl->CreateStore(init_value, alloca_inst);
}

static void compile_stmt_return(Stmt_Return *s, Context *ctx) {
        llvm::Value *v = compile_expr(s->e, ctx);
        ctx->bl->CreateRet(v);
}

static llvm::Function *compile_stmt_def(Stmt_Def *s, Context *ctx) {
        return gen_proc_proto(s->proto, ctx);
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
        case STMT_TYPE_DEF: {
                compile_stmt_def((Stmt_Def *)s, ctx);
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

        llvm::verifyModule(*(ctx->md));
        llvm::errs() << "Module contents";
        ctx->md->print(llvm::errs(), nullptr);
}
