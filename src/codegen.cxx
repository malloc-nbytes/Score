#include <cassert>
#include <iostream>
#include <llvm/IR/Instructions.h>
#include <variant>
#include <map>

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

#include "scope.hxx"
#include "codegen.hxx"
#include "grammar.hxx"
#include "types.hxx"
#include "utils.hxx"

struct fun {
    un_ptr<token::t> id;
    un_ptr<scr_type::t> rettype;
    llvm::Function *value;
};

struct var {
    sh_ptr<token::t> id;
    un_ptr<scr_type::t> ty;
    llvm::Value *value;
    std::string mod;

    var(sh_ptr<token::t> id, un_ptr<scr_type::t> ty, llvm::Value *value, std::string mod)
        : id(std::move(id)), ty(std::move(ty)), value(value), mod(mod) {}
};

struct context {
    un_ptr<llvm::LLVMContext> ctx;
    un_ptr<llvm::IRBuilder<>> bl;
    un_ptr<llvm::Module> md;
    vec<map<str, un_ptr<var>>> var_tbl;
    vec<map<str, un_ptr<fun>>> fun_tbl;

    context() {
        ctx = std::make_unique<llvm::LLVMContext>();
        md = std::make_unique<llvm::Module>("main", *ctx);
        bl = std::make_unique<llvm::IRBuilder<>>(*ctx);
    }
};

static llvm::Value *gen_expr(expr::t *expr, context &context);
static void gen_stmt(stmt::t *stmt, context &context);

void scope_push(context &context) {
    context.var_tbl.emplace_back();
    context.fun_tbl.emplace_back();
}

void scope_pop(context &context) {
    if (context.var_tbl.size() == 0) {
        std::cerr << "var table is size 0 when trying to pop" << std::endl;
        std::exit(1);
    }
    if (context.fun_tbl.size() == 0) {
        std::cerr << "fun table is size 0 when trying to pop" << std::endl;
        std::exit(1);
    }

    context.var_tbl.pop_back();
    context.fun_tbl.pop_back();
}

/*** FUNCTIONS */

bool scope_contains_function(const std::string &id, context &context) {
    for (auto it = context.fun_tbl.rbegin(); it != context.fun_tbl.rend(); ++it) {
        auto &map = *it;
        if (map.find(id) != map.end())
            return true;
    }
    return false;
}

fun *scope_get_function(const std::string &id, context &context) {
    for (auto it = context.fun_tbl.rbegin(); it != context.fun_tbl.rend(); ++it) {
        auto &map = *it;
        auto found = map.find(id);
        if (found != map.end())
            return found->second.get();
    }
    std::cerr << "tried to get function `" << id << "` when it does not exist" << std::endl;
    std::exit(1);
    return nullptr; // unreachable
}

void scope_add_function(un_ptr<fun> f, context &context) {
    if (scope_contains_function(f->id->lx, context)) {
        std::cerr << "tried to add function `" << f->id->lx << "` when it already exists in scope" << std::endl;
        std::exit(1);
    }
    context.fun_tbl.back().emplace(f->id->lx, std::move(f));
}

/*** VARIABLES */

bool scope_contains_var(const std::string &id, context &context) {
    for (auto it = context.var_tbl.rbegin(); it != context.var_tbl.rend(); ++it) {
        auto &map = *it;
        if (map.find(id) != map.end())
            return true;
    }
    return false;
}

void scope_add_var(un_ptr<var> v, context &context) {
    if (scope_contains_var(v->id->lx, context)) {
        std::cerr << "variable " << v->id->lx << " already exists in scope" << std::endl;
        std::exit(1);
    }
    context.var_tbl.back().emplace(v->id->lx, std::move(v));
}

var *scope_get_var(const std::string &id, context &context) {
    for (auto it = context.var_tbl.rbegin(); it != context.var_tbl.rend(); ++it) {
        auto &map = *it;
        auto found = map.find(id);
        if (found != map.end())
            return found->second.get();
    }
    std::cerr << "tried to get var `" << id << "` when it does not exist" << std::endl;
    std::exit(1);
    return nullptr; // unreachable
}

static llvm::Type *scr_type_to_llvm_type(scr_type::t *ty, context &context) {
    switch (ty->base) {
    case scr_type::base::I32:
      return llvm::Type::getInt32Ty(*(context.ctx));
    case scr_type::base::Str:
        return llvm::Type::getInt8PtrTy(*(context.ctx));
    default: {
        std::cerr << "unhandled type `" << scr_type::to_cxxstr(ty) << "`" << std::endl;
        std::exit(1);
    }
    }
    return nullptr; // unreachable
}

static llvm::Value *gen_expr_str_literal(expr::term::str_literal *strlit, context &context) {
    // Create an LLVM string constant
    llvm::ArrayType *arrayType = llvm::ArrayType::get(context.bl->getInt8Ty(), strlit->tok->lx.size() + 1); // +1 for null terminator
    llvm::Constant *stringConstant = llvm::ConstantDataArray::getString(*context.ctx, strlit->tok->lx, true); // true for null terminator

    // Create a global variable for the string
    llvm::GlobalVariable *stringVar = new llvm::GlobalVariable(
        *context.md,
        arrayType,
        true, // isConstant
        llvm::GlobalValue::PrivateLinkage,
        stringConstant,
        ".str" // Name of the variable
    );

    // Get the pointer to the first element of the array (string)
    llvm::Value *stringPtr = context.bl->CreateConstGEP2_32(arrayType, stringVar, 0, 0, "str_ptr");

    return stringPtr;
}

static llvm::Value *gen_expr_int_literal(expr::term::int_literal *intlit, context &context) {
  return llvm::ConstantInt::get(
      *(context.ctx),
      llvm::APInt(/*bits=*/32,
                  static_cast<uint32_t>(std::stoi(intlit->tok->lx))));
}

static llvm::Value *gen_expr_identifier(expr::term::identifier *id, context &context) {
    if (!scope_contains_var(id->tok->lx, context)) {
        std::cerr << "variable `" << id->tok->lx << "` does not exist" << std::endl;
        std::exit(1);
    }
    var *v = scope_get_var(id->tok->lx, context);
    return v->value;
}

static llvm::Value *gen_expr_proc_call(expr::term::proc_call *pc, context &context) {
    llvm::Function *callee = context.md->getFunction(pc->id);
    if (!callee) {
        std::cerr << "function `" << pc->id << "` does not exist";
        std::exit(1);
    }

    if (callee->arg_size() != pc->args.size() && !callee->isVarArg()) {
        std::cerr << "passed wrong number of args to function `" << pc->id << "`" << std::endl;
        std::exit(1);
    }

    vec<llvm::Value *> argsv = {};
    for (size_t i = 0; i < pc->args.size(); ++i) {
        auto expr = gen_expr(pc->args[i].get(), context);
        argsv.push_back(expr);
        if (!argsv.back()) {
            std::cerr << "codegen failed" << std::endl;
            std::exit(1);
        }
    }

    return context.bl->CreateCall(callee, std::move(argsv), "calltmp");
}

static llvm::Value *gen_expr_term(expr::term::t *term, context &context) {
    return std::visit([&](auto &&tm) {
        using T = std::decay_t<decltype(tm)>;
        if constexpr (std::is_same_v<T, un_ptr<expr::term::identifier>>) {
            return gen_expr_identifier(tm.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<expr::term::int_literal>>) {
            return gen_expr_int_literal(tm.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<expr::term::str_literal>>) {
            return gen_expr_str_literal(tm.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<expr::term::proc_call>>) {
            return gen_expr_proc_call(tm.get(), context);
        } else {
            assert(false && "unhandled expression term type");
        }
    }, term->actual);
    return nullptr; // unreachable
}

static llvm::Value *gen_expr_unary(expr::unary::t *un, context &context) {
    assert(false);
}

static llvm::Value *gen_expr_binary(expr::binary::t *bin, context &context) {
    llvm::Value *l = gen_expr(bin->lhs.get(), context);
    llvm::Value *r = gen_expr(bin->rhs.get(), context);
    if (!l || !r) {
        return nullptr;
    }
    switch (bin->op->ty) {
    case token::type::Plus:     return context.bl->CreateAdd(l, r, "addtmp");
    case token::type::Minus:    return context.bl->CreateSub(l, r, "subtmp");
    case token::type::Asterisk: return context.bl->CreateMul(l, r, "multmp");
    default: {
        std::cerr << "unhandled binop `" << bin->op->lx << "`";
        std::exit(1);
    }
    }
    return nullptr; // unreachable
}

static llvm::Value *gen_expr(expr::t *expr, context &context) {
    return std::visit([&](auto &&ex) {
        using T = std::decay_t<decltype(ex)>;
        if constexpr (std::is_same_v<T, un_ptr<expr::term::t>>) {
            return gen_expr_term(ex.get(), context);
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::binary::t>>) {
            return gen_expr_binary(ex.get(), context);
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::unary::t>>) {
            return gen_expr_unary(ex.get(), context);
        }
        else {
            assert(false && "unhandled expression type");
        }
    }, expr->actual);
    return nullptr; // unreachable
}

static llvm::Function *gen_proc_prototype(token::t *id,
                                          vec<un_ptr<stmt::parameter>> &params,
                                          scr_type::t *rettype,
                                          bool variadic,
                                          context &context) {
    vec<llvm::Type *> types;

    for (auto &p : params) {
        types.push_back(scr_type_to_llvm_type(p->ty.get(), context));
    }

    // If the function is variadic, set the last argument type to be a pointer type (e.g., void*)
    llvm::FunctionType *ft = llvm::FunctionType::get(
        scr_type_to_llvm_type(rettype, context), types, variadic);

    llvm::Function *f = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, id->lx, context.md.get());

    size_t idx = 0;
    for (auto &a : f->args()) {
        a.setName(params.at(idx++)->id->lx);
    }

    return f;
}

static llvm::Function *gen_stmt_def(stmt::def *stmt, context &context) {
    return gen_proc_prototype(stmt->id.get(), stmt->params, stmt->rettype.get(), stmt->variadic, context);
}

static void gen_stmt_let(stmt::let *stmt, context &context) {
    const std::string &id = stmt->id->lx;
    llvm::Value *value = gen_expr(stmt->expr.get(), context);
    auto v = std::make_unique<var>(stmt->id, std::move(stmt->ty), value, "");
    scope_add_var(std::move(v), context);
}

static void gen_stmt_block(stmt::block *stmt, context &context) {
    for (auto &s : stmt->stmts)
        gen_stmt(s.get(), context);
}

static llvm::Function *gen_stmt_proc(stmt::proc *stmt, context &context) {
    llvm::Function *existing_function = context.md->getFunction(stmt->id->lx);

    if (!existing_function) {
      existing_function =
          gen_proc_prototype(stmt->id.get(), stmt->params, stmt->rettype.get(),
                             stmt->variadic, context);
    }
    if (!existing_function) {
        return nullptr;
    }
    if (!existing_function->empty()) {
        std::cerr << "function `" << stmt->id->lx << "` cannot be redefined" << std::endl;
        std::exit(1);
    }

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*(context.ctx), "entry", existing_function);
    context.bl->SetInsertPoint(bb);

    scope_push(context);

    for (auto &arg : existing_function->args()) {
        un_ptr<var> v = std::make_unique<var>(
            stmt->params.at(arg.getArgNo())->id,
            std::move(stmt->params.at(arg.getArgNo())->ty), &arg, "");

        scope_add_var(std::move(v), context);
    }

    gen_stmt_block(stmt->block.get(), context);

    scope_pop(context);

    llvm::verifyFunction(*existing_function);

    return existing_function;
}

static void gen_stmt_module(stmt::_module *stmt, context &context) {
    assert(false);
}

static void gen_stmt_mut(stmt::mut *stmt, context &context) {
    assert(false);
}

static void gen_stmt_while(stmt::_while *stmt, context &context) {
    assert(false);
}

static void gen_stmt_for(stmt::_for *stmt, context &context) {
    // Step 1: Generate the initialization statement.
    gen_stmt(stmt->init.get(), context);

    // Step 2: Create the basic block for the loop condition.
    llvm::BasicBlock *cond_bb = llvm::BasicBlock::Create(*(context.ctx), "cond", context.bl->GetInsertBlock()->getParent());
    llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(*(context.ctx), "body", context.bl->GetInsertBlock()->getParent());
    llvm::BasicBlock *after_bb = llvm::BasicBlock::Create(*(context.ctx), "after", context.bl->GetInsertBlock()->getParent());

    // Step 3: Insert the branch to condition block after initialization.
    context.bl->CreateBr(cond_bb);

    // Step 4: Generate the loop condition.
    context.bl->SetInsertPoint(cond_bb);
    llvm::Value *cond = gen_expr(stmt->cond.get(), context);
    if (!cond) {
        std::cerr << "failed to generate condition expression for `for` statement" << std::endl;
        std::exit(1);
    }
    // Ensure condition is of type i1 (boolean).
    cond = context.bl->CreateICmpNE(cond, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*(context.ctx)), 0), "condtmp");

    // Step 5: Create the conditional branch (if condition is true, jump to body, else to after).
    context.bl->CreateCondBr(cond, body_bb, after_bb);

    // Step 6: Generate the body of the loop.
    context.bl->SetInsertPoint(body_bb);
    gen_stmt_block(stmt->block.get(), context);

    // Step 7: Generate the after-statement (e.g., incrementing the loop counter).
    if (stmt->after) {
        gen_stmt(stmt->after.get(), context);
    }

    // Step 8: After executing the body and after-statement, branch back to the condition block.
    context.bl->CreateBr(cond_bb);

    // Step 9: Set the insert point to the after block, which is the exit of the loop.
    context.bl->SetInsertPoint(after_bb);
}

static void gen_stmt_return(stmt::_return *stmt, context &context) {
    llvm::Value *value = gen_expr(stmt->expr.get(), context);
    context.bl->CreateRet(value);
}

static llvm::Value *gen_stmt_if(stmt::_if *stmt, context &context) {
    // Step 1: Generate the condition expression
    llvm::Value *cond = gen_expr(stmt->cond.get(), context);
    if (!cond) {
        std::cerr << "failed to generate condition expression for `if` statement" << std::endl;
        std::exit(1);
    }

    // Ensure the condition is of boolean type (i1)
    cond = context.bl->CreateICmpNE(cond, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*(context.ctx)), 0), "condtmp");

    // Step 2: Create basic blocks for the 'then' and 'else' branches
    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(*(context.ctx), "then", context.bl->GetInsertBlock()->getParent());
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(*(context.ctx), "else", context.bl->GetInsertBlock()->getParent());
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(*(context.ctx), "ifcont", context.bl->GetInsertBlock()->getParent());

    // Step 3: Create the branch based on the condition
    context.bl->CreateCondBr(cond, then_bb, else_bb);

    // Step 4: Generate the 'then' block
    context.bl->SetInsertPoint(then_bb);
    gen_stmt_block(stmt->block.get(), context);  // Generates the body of the 'then' block
    context.bl->CreateBr(merge_bb);  // Jump to merge block after 'then' block

    // Step 5: Generate the 'else' block (if it exists)
    context.bl->SetInsertPoint(else_bb);
    llvm::Value *elseResult = nullptr;
    if (stmt->_else.has_value())
        gen_stmt_block(stmt->_else.value().get(), context);
    context.bl->CreateBr(merge_bb);  // Jump to merge block after 'else' block

    // Step 6: Merge the control flow from both branches
    context.bl->SetInsertPoint(merge_bb);

    return nullptr;
}

static void gen_stmt(stmt::t *stmt, context &context) {
    std::visit([&](auto &&st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, un_ptr<stmt::proc>>) {
            gen_stmt_proc(st.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::def>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::let>>) {
            gen_stmt_let(st.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::block>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::_module>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::mut>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::_if>>) {
            gen_stmt_if(st.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::_while>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::_for>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::_return>>) {
            gen_stmt_return(st.get(), context);
        } else if constexpr (std::is_same_v<T, un_ptr<expr::t>>) {
            gen_expr(st.get(), context);
        }
        else {
            assert(false && "unhandled statement type");
        }
    }, stmt->actual);
}

void gen_toplvl_stmt(stmt::t *stmt, context &context) {
    std::visit([&](auto &&st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, un_ptr<stmt::proc>>) {
            gen_stmt(stmt, context);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::let>>) {
            assert(false);
        } else if constexpr (std::is_same_v<T, un_ptr<stmt::def>>) {
            gen_stmt_def(st.get(), context);
        } else {
            std::cerr << "invalid toplvl stmt" << std::endl;
            std::exit(1);
        }
    }, stmt->actual);
}

void codegen::gen(un_ptr<program::t> program) {
    context context;

    for (auto &stmt : program->stmts)
        (void)gen_toplvl_stmt(stmt.get(), context);

    llvm::verifyModule(*(context.md));
    llvm::errs() << "Module contents";
    context.md->print(llvm::errs(), nullptr);
}
