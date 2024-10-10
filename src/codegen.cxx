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
#include "types.hxx"
#include "utils.hxx"

// struct fun {
//     un_ptr<token::t> id;
//     un_ptr<scr_type::t> rettype;
//
// };
//
// struct var {
//     sh_ptr<token::t> id;
//     un_ptr<scr_type::t> ty;
//     llvm::Value *value;
//     std::string mod;
// };
//
// struct context {
//     un_ptr<llvm::LLVMContext> ctx;
//     un_ptr<llvm::IRBuilder<>> bl;
//     un_ptr<llvm::Module> md;
// };

static un_ptr<llvm::LLVMContext> llvm_context;
static un_ptr<llvm::IRBuilder<>> llvm_builder;
static un_ptr<llvm::Module> llvm_module;
// static scope::t<str, llvm::Value *> named_values;
static map<str, llvm::Value *> named_values;

static llvm::Value *gen_expr(expr::t *expr);
static void gen_stmt(stmt::t *stmt);

static llvm::Value *log_error_v(std::string s) {
    std::cerr << "error: " << s << std::endl;
    return nullptr;
}

static llvm::Type *scr_type_to_llvm_type(scr_type::t *ty) {
    switch (ty->base) {
        case scr_type::base::I32: return llvm::Type::getInt32Ty(*llvm_context);
        default: {
            std::cerr << "unhandled type `" << scr_type::to_cxxstr(ty) << "`" << std::endl;
            std::exit(1);
        }
    }
    return nullptr; // unreachable
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
    assert(false);
    // if (!scope::contains(named_values, id->tok->lx)) {
    //     std::cerr << "identifier `" << id->tok->lx << "` does not exist" << std::endl;
    //     std::exit(1);
    // }
    // llvm::Value *v = scope::get(named_values, id->tok->lx);
    // return v;
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
    return std::visit([&](auto &&tm) {
        using T = std::decay_t<decltype(tm)>;
        if constexpr (std::is_same_v<T, un_ptr<expr::term::identifier>>) {
            return gen_expr_identifier(tm.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::term::int_literal>>) {
            return gen_expr_int_literal(tm.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::term::str_literal>>) {
            return gen_expr_str_literal(tm.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::term::proc_call>>) {
            return gen_expr_proc_call(tm.get());
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
        case token::type::Plus: return llvm_builder->CreateAdd(lhs, rhs, "addtmp");
        case token::type::Minus: return llvm_builder->CreateSub(lhs, rhs, "subtmp");
        default: {
            return log_error_v("invalid binop `"+bin->op->lx+"`");
        } break;
    }
}

static llvm::Value *gen_expr(expr::t *expr) {
    return std::visit([&](auto &&ex) {
        using T = std::decay_t<decltype(ex)>;
        if constexpr (std::is_same_v<T, un_ptr<expr::term::t>>) {
            return gen_expr_term(ex.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::binary::t>>) {
            return gen_expr_binary(ex.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<expr::unary::t>>) {
            return gen_expr_unary(ex.get());
        }
        else {
            assert(false && "unhandled expression type");
        }
    }, expr->actual);
    return nullptr; // unreachable
}

static llvm::Function *gen_proc_prototype(token::t *id,
                                          vec<un_ptr<stmt::parameter>> &params,
                                          scr_type::t *rettype) {
    vec<llvm::Type *> types = {};
    for (auto &p : params)
        types.push_back(scr_type_to_llvm_type(p->ty.get()));

    llvm::FunctionType *ft = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*llvm_context),
        types,
        false
    );

    llvm::Function *f = llvm::Function::Create(
        ft,
        llvm::Function::ExternalLinkage,
        id->lx,
        llvm_module.get()
    );

    size_t idx = 0;
    for (auto &a : f->args())
        a.setName(params.at(idx++)->id->lx);

    return f;
}

static llvm::Function *gen_stmt_def(stmt::def *stmt) {
    return gen_proc_prototype(stmt->id.get(), stmt->params, stmt->rettype.get());
}

static void gen_stmt_let(stmt::let *stmt) {
    assert(false);
}

static void gen_stmt_block(stmt::block *stmt) {
    for (auto &s : stmt->stmts)
        gen_stmt(s.get());
}

static llvm::Function *gen_stmt_proc(stmt::proc *stmt) {
    llvm::Function *fun = llvm_module->getFunction(stmt->id->lx);
    if (!fun)
        fun = gen_proc_prototype(stmt->id.get(), stmt->params, stmt->rettype.get());
    if (!fun)
        return nullptr;
    if (!fun->empty())
        return (llvm::Function *)log_error_v("Function cannot be redefined");

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*llvm_context, "entry", fun);
    llvm_builder->SetInsertPoint(bb);

    // scope::push(named_values);
    named_values.clear();
    for (auto &a : fun->args())
        named_values[std::string(a.getName())] = &a;

    // if (llvm::Value *retval = gen_stmt_block(stmt->block.get())) {
    //     llvm_builder->CreateRet(retval);
    //     llvm::verifyFunction(*fun);
    //     return fun;
    // }

    gen_stmt_block(stmt->block.get());
    llvm::verifyFunction(*fun);
    return fun;
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
    llvm::Value *value = gen_expr(stmt->expr.get());
    assert(value && "value cannot be null when returning");
    llvm_builder->CreateRet(value);
}

static void gen_stmt_if(stmt::_if *stmt) {
    assert(false);
}

static void gen_stmt(stmt::t *stmt) {
    std::visit([&](auto &&st) {
        using T = std::decay_t<decltype(st)>;
        if constexpr (std::is_same_v<T, un_ptr<stmt::proc>>) {
            gen_stmt_proc(st.get());
        }
        else if constexpr (std::is_same_v<T, un_ptr<stmt::def>>) {
            gen_stmt_def(st.get());
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
            gen_stmt_return(st.get());
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
        else if constexpr (std::is_same_v<T, un_ptr<stmt::def>>) {
            gen_stmt(stmt);
        }
        else {
            std::cerr << "invalid toplvl stmt" << std::endl;
            std::exit(1);
        }
    }, stmt->actual);
}

void codegen::gen(un_ptr<program::t> program) {
    llvm_context = std::make_unique<llvm::LLVMContext>();
    llvm_module = std::make_unique<llvm::Module>("main", *llvm_context);
    llvm_builder = std::make_unique<llvm::IRBuilder<>>(*llvm_context);

    for (auto &stmt : program->stmts)
        gen_toplvl_stmt(stmt.get());

    llvm::verifyModule(*llvm_module);

    llvm::errs() << "Module contents";
    llvm_module->print(llvm::errs(), nullptr);
    assert(false && "done");
}


