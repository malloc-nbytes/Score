#include <iostream>
#include <cassert>
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
#include "utils.hxx"

static un_ptr<llvm::LLVMContext> llvm_context;
static un_ptr<llvm::IRBuilder<>> llvm_builder;
static un_ptr<llvm::Module> llvm_module;
static scope::t<str, llvm::Value *> named_values;

static llvm::Value *gen_expr(expr::t *expr);

static llvm::Value *log_error_v(std::string s) {
    std::cerr << "error: " << s << std::endl;
    return nullptr;
}

static llvm::Value *gen_expr_str_literal(expr::term::str_literal *strlit) {
    assert(false);
}

static llvm::Value *gen_expr_int_literal(expr::term::int_literal *intlit) {
    int value = std::stoi(intlit->tok->lx);
    return llvm::ConstantInt::get(
        *llvm_context,
        llvm::APInt(32, static_cast<uint64_t>(value))
    );
}

static llvm::Value *gen_expr_identifier(expr::term::identifier *id) {
    if (!scope::contains(named_values, id->tok->lx)) {
        std::cerr << "identifier `" << id->tok->lx << "` does not exist" << std::endl;
        std::exit(1);
    }
    llvm::Value *v = scope::get(named_values, id->tok->lx);
    return v;
}

static llvm::Value *gen_expr_proc_call(expr::term::proc_call *pc) {
    llvm::Function *callee = llvm_module->getFunction(pc->id);

    if (!callee)
        return log_error_v("unknown function referenced");
    if (callee->arg_size() != pc->args.size())
        return log_error_v("incorrect amount of args passed");

    vec<llvm::Value *> args = {};
    for (size_t i = 0; i < pc->args.size(); ++i) {
        args.push_back(gen_expr(pc->args.at(i).get()));
        if (!args.back())
            return nullptr;
    }

    return llvm_builder->CreateCall(callee, args, "calltmp");
}

static llvm::Value *gen_expr_term(expr::term::t *term) {
    std::visit([&](auto &&tm) {
        using T = std::decay_t<decltype(tm)>;
        if constexpr (std::is_same_v<T, expr::term::identifier>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, expr::term::int_literal>) {
            return gen_expr_int_literal(tm);
        }
        else if constexpr (std::is_same_v<T, expr::term::str_literal>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, expr::term::proc_call>) {
            assert(false);
        }
        else {
            assert(false && "unhandled expression term type");
        }
    }, term->actual);
    return nullptr; // unreachable
}

static llvm::Value *gen_expr_unary(expr::unary::t *un) {
    assert(false);
}

static llvm::Value *gen_expr_binary(expr::binary::t *bin) {
    llvm::Value *lhs = gen_expr(bin->lhs.get());
    llvm::Value *rhs = gen_expr(bin->rhs.get());
    if (!lhs || !rhs)
        return nullptr;
    switch (bin->op->ty) {
        case token::type::Plus: return llvm_builder->CreateAnd(lhs, rhs, "addtmp");
        default: {
            return log_error_v("invalid binop `"+bin->op->lx+"`");
        } break;
    }
}

static llvm::Value *gen_expr(expr::t *expr) {
    std::visit([&](auto &&ex) {
        using T = std::decay_t<decltype(ex)>;
        if constexpr (std::is_same_v<T, un_ptr<expr::term::t>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::binary::t>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::unary::t>>) {
            assert(false);
        }
        else {
            assert(false && "unhandled expression type");
        }
    }, expr->actual);
    return nullptr; // unreachable
}

static void gen_stmt_proc(stmt::proc *stmt) {
    assert(false);
}

static void gen_stmt_let(stmt::let *stmt) {
    assert(false);
}

static void gen_stmt_block(stmt::block *stmt) {
    assert(false);
}

static void gen_stmt_module(stmt::_module *stmt) {
    assert(false);
}

static void gen_stmt_mut(stmt::mut *stmt) {
    assert(false);
}

static void gen_stmt_while(stmt::_while *stmt) {
    assert(false);
}

static void gen_stmt_for(stmt::_for *stmt) {
    assert(false);
}

static void gen_stmt_return(stmt::_return *stmt) {
    assert(false);
}

static void gen_stmt_if(stmt::_if *stmt) {
    assert(false);
}

static void gen_stmt(stmt::t *stmt) {
    std::visit([&](auto &&st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, un_ptr<stmt::proc>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::let>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::block>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::_module>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::mut>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::_if>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::_while>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::_for>>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::_return>>) {
            assert(false);
        }
        else {
            assert(false && "unhandled statement type");
        }
    }, stmt->actual);
}

void gen_toplvl_stmt(stmt::t *stmt) {
    std::visit([&](auto &&st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, un_ptr<stmt::proc>>) {
            gen_stmt(stmt);
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::let>>) {
            gen_stmt(stmt);
        }
        else {
            std::cerr << "invalid toplvl stmt" << std::endl;
            std::exit(1);
        }
    }, stmt->actual);
}

void codegen::gen(un_ptr<program::t> program) {
    assert(false);
}


