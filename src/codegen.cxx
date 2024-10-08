#include <cassert>
#include <variant>

#include "codegen.hxx"
#include "grammar.hxx"
#include "utils.hxx"

static void gen_expr(expr::t *expr);

static void gen_expr_str_literal(expr::term::str_literal *strlit) {
    assert(false);
}

static void gen_expr_int_literal(expr::term::int_literal *intlit) {
    assert(false);
}

static void gen_expr_identifier(expr::term::identifier *id) {
    assert(false);
}

static void gen_expr_term(expr::term::t *term) {
    std::visit([&](auto &tm) {
        using T = std::decay_t<decltype(tm)>;
        if constexpr (std::is_same_v<T, expr::term::identifier>) {
            assert(false);
        }
        else if constexpr (std::is_same_v<T, expr::term::int_literal>) {
            assert(false);
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
}

static void gen_expr_unary(expr::unary::t *un) {
    assert(false);
}

static void gen_expr_binary(expr::binary::t *bin) {
    assert(false);
}

static void gen_expr(expr::t *expr) {
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

void codegen::gen(un_ptr<program::t> program) {
    assert(false);
}


