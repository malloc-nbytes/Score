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
static llvm::Value *compile_expr(Expr *e, Context *ctx);

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
                llvm::Type *llty = scr_type_to_llvm_type(s->args.types[i], ctx);
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

static llvm::Value *compile_expr_int_lit(Expr_Int_Lit *e, Context *ctx) {
        return llvm::ConstantInt::get(*(ctx->llctx),
                                      llvm::APInt(/*bits=*/32, (uint32_t)e->i));
}

static llvm::Value *compile_expr_proc_call(Expr_Proc_Call *e, Context *ctx) {
        llvm::Value *callee = compile_expr(e->left, ctx);
        if (!callee) {
                err("failed to compile function expression in procedure call");
        }

        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < e->args.len; ++i) {
                llvm::Value *arg = compile_expr(e->args.exprs[i], ctx);
                if (!arg) {
                        err_wargs("failed to compile argument %zu in procedure call", i);
                }
                args.push_back(arg);
        }

        llvm::Function *func = llvm::dyn_cast<llvm::Function>(callee);
        if (!func) {
                err("callee is not a directly callable function");
        }

        return ctx->bl->CreateCall(func, args);
}

static llvm::Value *compile_expr_ident(Expr_Ident *e, Context *ctx) {
        char *id = e->id->lx;

        // Check variables in scope
        for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                if (ctx->vs[i].has(id)) {
                        Var *var = ctx->vs[i].get(id);
                        // Return the pointer (e.g., AllocaInst*) directly, not the loaded value
                        return var->value;  // Should be the allocation or parameter pointer
                }
        }

        // Check procedures in scope
        for (int i = (int)ctx->ps.length() - 1; i >= 0; --i) {
                if (ctx->ps[i].has(id)) {
                        Proc *proc = ctx->ps[i].get(id);
                        return proc->value;  // Function pointer
                }
        }

        // Check module-level functions
        if (llvm::Function *func = ctx->md->getFunction(id)) {
                return func;
        }

        err_wargs("undefined identifier '%s'", id);
        return nullptr;
}

static llvm::Value *compile_expr_str_lit(Expr_Str_Lit *e, Context *ctx) {
        const char *str = e->s->lx;

        // Create a global constant string
        llvm::Constant *str_constant = llvm::ConstantDataArray::getString(
                *ctx->llctx, str,
                true  /* Add null terminator */);

        // Create a global variable to hold the string
        llvm::GlobalVariable *gv = new llvm::GlobalVariable(
                *ctx->md,                          // Module
                str_constant->getType(),            // Type of the string array
                true,                              // isConstant
                llvm::GlobalValue::PrivateLinkage, // Linkage
                str_constant,                       // Initializer
                ""                                 // Name
                );

        // Get a pointer to the start of the string (i8*)
        llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), 0);
        std::vector<llvm::Constant*> indices = {zero, zero};
        llvm::Constant *str_ptr = llvm::ConstantExpr::getGetElementPtr(str_constant->getType(), gv, indices);

        return str_ptr;
}

static llvm::Value *compile_expr_mut(Expr_Mut *e, Context *ctx) {
        llvm::Value *left = nullptr;
        if (e->l->ty == EXPR_TYPE_IDENT) {
                left = compile_expr_ident((Expr_Ident *)e->l, ctx);
        } else {
                // For now, only support identifiers as lvalues; TODO: extend later
                left = compile_expr(e->l, ctx);
        }
        if (!left) {
                err("failed to compile left operand of assignment");
                return nullptr;
        }

        // Ensure left is a pointer (lvalue) that we can store to
        if (!left->getType()->isPointerTy()) {
                err("left operand of assignment must be an lvalue");
                return nullptr;
        }

        // Compile the right-hand side (the value to assign)
        llvm::Value *right = compile_expr(e->r, ctx);
        if (!right) {
                err("failed to compile right operand of assignment");
                return nullptr;
        }

        const char *op = e->op->lx;

        if (!strcmp(op, "=")) {
                ctx->bl->CreateStore(right, left);
                return right;
        } else {
                // For compound assignments, we need to load the current value
                llvm::Type *valType = nullptr;
                if (e->l->ty == EXPR_TYPE_IDENT) {
                        char *id = ((Expr_Ident *)e->l)->id->lx;
                        for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                                if (ctx->vs[i].has(id)) {
                                        Var *var = ctx->vs[i].get(id);
                                        valType = scr_type_to_llvm_type(var->ty, ctx);
                                        break;
                                }
                        }
                        if (!valType) {
                                err_wargs("could not determine type of identifier '%s' for compound assignment", id);
                                return nullptr;
                        }
                } else {
                        err("compound assignment only supported for identifiers with known types");
                        return nullptr;
                }

                llvm::Value *current = ctx->bl->CreateLoad(valType, left, "loadtmp");

                llvm::Value *result = nullptr;
                if (!strcmp(op, "+=")) {
                        result = ctx->bl->CreateAdd(current, right, "addtmp");
                } else if (!strcmp(op, "-=")) {
                        result = ctx->bl->CreateSub(current, right, "subtmp");
                } else if (!strcmp(op, "*=")) {
                        result = ctx->bl->CreateMul(current, right, "multmp");
                } else if (!strcmp(op, "/=")) {
                        result = ctx->bl->CreateSDiv(current, right, "divtmp");
                } else {
                        err_wargs("unsupported assignment operator '%s'", op);
                        return nullptr;
                }

                ctx->bl->CreateStore(result, left);
                return result;
        }

        assert(0 && "unreachable");
}

static llvm::Value *compile_expr_bin(Expr_Bin *e, Context *ctx) {
        llvm::Value *left = compile_expr(e->l, ctx);
        if (!left) {
                err("failed to compile left operand of binary expression");
        }

        llvm::Value *right = compile_expr(e->r, ctx);
        if (!right) {
                err("failed to compile right operand of binary expression");
        }

        const char *op = e->op->lx;

        // For now, assume both operands are i32; TODO: extend type checking later
        llvm::Type *i32Ty = llvm::Type::getInt32Ty(*(ctx->llctx));
        if (left->getType() != i32Ty || right->getType() != i32Ty) {
                err_wargs("binary operator '%s' requires i32 operands", op);
        }

        if (!strcmp(op, "+")) {
                return ctx->bl->CreateAdd(left, right, "addtmp");
        } else if (!strcmp(op, "-")) {
                return ctx->bl->CreateSub(left, right, "subtmp");
        } else if (!strcmp(op, "*")) {
                return ctx->bl->CreateMul(left, right, "multmp");
        } else if (!strcmp(op, "/")) {
                return ctx->bl->CreateSDiv(left, right, "sdivtmp");
        } else {
                err_wargs("unsupported binary operator '%s'", op);
        }

        assert(0 && "unreachable");
        return nullptr;
}

static llvm::Value *compile_expr(Expr *e, Context *ctx) {
        switch (e->ty) {
        case EXPR_TYPE_UNARY: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_BIN: {
                return compile_expr_bin((Expr_Bin *)e, ctx);
        } break;
        case EXPR_TYPE_MUT: {
                return compile_expr_mut((Expr_Mut *)e, ctx);
        } break;
        case EXPR_TYPE_IDENT: {
                llvm::Value *val = compile_expr_ident((Expr_Ident *)e, ctx);
                if (!val) return nullptr;
                // Check if it's a variable (needs loading) or a function (return directly)
                if (val->getType()->isPointerTy()) {
                        char *id = ((Expr_Ident *)e)->id->lx;
                        // Check if it's a variable in scope
                        for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                                if (ctx->vs[i].has(id)) {
                                        Var *var = ctx->vs[i].get(id);
                                        return ctx->bl->CreateLoad(scr_type_to_llvm_type(var->ty, ctx), val, id);
                                }
                        }
                        // If not a variable, it’s likely a function pointer—return it directly
                        return val;
                }
                return val; // Non-pointer values
        } break;
        case EXPR_TYPE_STR_LIT: {
                return compile_expr_str_lit((Expr_Str_Lit *)e, ctx);
        } break;
        case EXPR_TYPE_INT_LIT: {
                return compile_expr_int_lit((Expr_Int_Lit *)e, ctx);
        } break;
        case EXPR_TYPE_PROC_CALL: {
                return compile_expr_proc_call((Expr_Proc_Call *)e, ctx);
        } break;
        default: {
                err_wargs("unknown expression type %d", (int)e->ty);
        } break;
        }
        assert(0 && "unreachable");
        return nullptr;
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
        llvm::AllocaInst *alloca_inst = ctx->bl->CreateAlloca(llty, nullptr, s->id->lx);

        llvm::Value *init_value = compile_expr(s->e, ctx);
        if (!init_value) {
                err_wargs("failed to compile expression for identifier %s", s->id->lx);
        }

        Var new_var = { s->id, s->type, alloca_inst };
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
        case STMT_TYPE_EXPR: {
                (void)compile_expr(((Stmt_Expr *)s)->e, ctx);
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
