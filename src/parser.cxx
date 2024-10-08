#include <cstdlib>
#include <iostream>
#include <cassert>
#include <memory>
#include <variant>

#include "grammar.hxx"
#include "lexer.hxx"
#include "parser.hxx"
#include "token.hxx"
#include "types.hxx"
#include "utils.hxx"
#include "err.hxx"
#include "common.hxx"

static un_ptr<expr::t> parse_expr(lexer::t &lexer);
static un_ptr<stmt::t> parse_stmt(lexer::t &lexer);

static sh_ptr<token::t> expect(lexer::t &lexer, token::type ty) { 
    if (!lexer::peek(lexer))
        ERR("expect: ran out of tokens");

    auto t = lexer::next(lexer);

    if (t->ty != ty) {
        const str got = token::type_to_cxxstr(t->ty),
                  exp = token::type_to_cxxstr(ty);
        err::wtok(t.get());
        ERRW("expected %s but got %s", exp.c_str(), got.c_str());
    }

    return t;
}

static sh_ptr<token::t> expect_keyword(lexer::t &lexer, const std::string &kw) {
    if (!lexer::peek(lexer))
        ERR("expect_keyword: ran out of tokens");

    auto t = lexer::next(lexer);

    if (t->ty != token::type::Keyword) {
        const str got = token::type_to_cxxstr(t->ty),
                  exp = token::type_to_cxxstr(token::type::Keyword);
        ERRW("expected %s but got %s", exp.c_str(), got.c_str());
    }

    if (t->lx != kw)
        ERRW("expected keyword `%s` but got keyword `%s`", kw.c_str(), t->lx.c_str());

    return t;
}

/*
 * Expects and parses a valid Score type.
 */
static un_ptr<scr_type::t> expect_type(lexer::t &lexer) {
    auto next_ty = lexer::peek(lexer)->ty;
    if (next_ty != token::type::Type) {
        const str got = token::type_to_cxxstr(next_ty),
                  exp = token::type_to_cxxstr(token::type::Type);
        ERRW("expected %s but got %s", exp.c_str(), got.c_str());
    }
    return scr_type::parse(lexer);
}

/*
 * Will parse parameters in the form of:
 *   `(x1: <type>, x2: <type>, ..., xN: <type>)`.
 */
static vec<un_ptr<stmt::parameter>> parse_proc_parameters(lexer::t &lexer) {
    ignore(expect(lexer, token::type::LParen));

    vec<un_ptr<stmt::parameter>> params = {};

    if (lexer_speek(lexer)->ty == token::type::Type) {
        auto ty = expect_type(lexer);
        if (scr_type::is_void(ty.get()))
            goto done;
        else
            ERRW("expected `void` but got `%s`", scr_type::to_cxxstr(ty.get()).c_str());
    }

    while (lexer_speek(lexer)->ty != token::type::RParen) {
        auto id = expect(lexer, token::type::Ident);

        ignore(expect(lexer, token::type::Colon));

        auto ty = expect_type(lexer);
        auto param = std::make_unique<stmt::parameter>(
            std::move(id),
            std::move(ty)
        );

        params.push_back(std::move(param));

        if (lexer_speek(lexer)->ty == token::type::Comma)
            lexer::discard(lexer); // ,
        else
            break;
    }

 done:
    ignore(expect(lexer, token::type::RParen));
    return std::move(params);
}

static vec<un_ptr<expr::t>> parse_comma_sep_exprs(lexer::t &lexer,
                                                  token::type start,
                                                  token::type end) {
    vec<un_ptr<expr::t>> exprs = {};
    lexer::discard(lexer); // (, [, {, etc

    while (lexer::peek(lexer)->ty != end) {
        un_ptr<expr::t> expr = parse_expr(lexer);
        exprs.push_back(std::move(expr));
        if (lexer_speek(lexer)->ty == token::type::Comma) {
            lexer::discard(lexer);
        }
        else {
            ignore(expect(lexer, end));
            break;
        }
    }

    return exprs;
}

static un_ptr<expr::t> parse_primary_expr(lexer::t &lexer) {
    un_ptr<expr::t> left;

    while (true) {
        if (!lexer::peek(lexer))
            return std::move(left);
        switch (lexer::peek(lexer)->ty) {
        case token::type::Ident: {
            auto ident = std::make_unique<expr::term::identifier>(lexer::next(lexer));
            auto term = std::make_unique<expr::term::t>(std::move(ident), expr::term::type::Ident);
            left = std::make_unique<expr::t>(std::move(term), expr::type::Term);
        } break;
        case token::type::Intlit: {
            auto intlit = std::make_unique<expr::term::int_literal>(lexer::next(lexer));
            auto term = std::make_unique<expr::term::t>(std::move(intlit), expr::term::type::Ident);
            left = std::make_unique<expr::t>(std::move(term), expr::type::Term);
        } break;
        case token::type::Strlit: {
            auto strlit = std::make_unique<expr::term::str_literal>(lexer::next(lexer));
            auto term = std::make_unique<expr::term::t>(std::move(strlit), expr::term::type::Ident);
            left = std::make_unique<expr::t>(std::move(term), expr::type::Term);
        } break;
        case token::type::LParen: {
            // Math
            if (!left) {
                auto lhs = parse_expr(lexer);
                auto op = lexer::next(lexer); // op checking done in codegen
                auto rhs = parse_expr(lexer);
                ignore(expect(lexer, token::type::RParen));
                auto bin = std::make_unique<expr::binary::t>(std::move(lhs),
                                                             std::move(rhs),
                                                             std::move(op));
                left = std::make_unique<expr::t>(std::move(bin), expr::type::Binary);
            }
            // Proc Call
            else {
                str id = "";
                std::visit([&](auto &&f) {
                    using T = std::decay_t<decltype(f)>;
                    if constexpr (std::is_same_v<T, un_ptr<expr::term::str_literal>>) {
                        id = f->tok->lx;
                    }
                    else {
                        std::cerr << "proc calls can only be used with identifiers right now" << std::endl;
                        std::exit(1);
                    }
                }, left->actual);
                auto args = parse_comma_sep_exprs(lexer, token::type::LParen, token::type::RParen);
                auto proc_call = std::make_unique<expr::term::proc_call>(std::move(id), std::move(args));
                auto term = std::make_unique<expr::term::t>(std::move(proc_call), expr::term::type::Proc_Call);
                left = std::make_unique<expr::t>(std::move(term), expr::type::Term);
            }
        } break;
        default: return std::move(left);
        }
    }

    return nullptr; // unreachable
}

static un_ptr<expr::t> parse_multiplicative_expr(lexer::t &lexer) {
    auto lhs = parse_primary_expr(lexer);
    auto cur = lexer::peek(lexer);

    while (cur && (cur->ty == token::type::Asterisk
                   || cur->ty == token::type::Forwardslash
                   || cur->ty == token::type::Percent)) {
        auto op = lexer::next(lexer);
        auto rhs = parse_primary_expr(lexer);
        auto bin = std::make_unique<expr::binary::t>(
            std::move(lhs),
            std::move(rhs),
            std::move(op)
        );
        lhs = std::make_unique<expr::t>(std::move(bin), expr::type::Binary);
        cur = lexer::peek(lexer);
    }

    return lhs;
}

static un_ptr<expr::t> parse_additive_expr(lexer::t &lexer) {
    auto lhs = parse_multiplicative_expr(lexer);
    auto cur = lexer::peek(lexer);

    while (cur && (cur->ty == token::type::Plus
                   || cur->ty == token::type::Minus)) {
        auto op = lexer::next(lexer);
        auto rhs = parse_multiplicative_expr(lexer);
        auto bin = std::make_unique<expr::binary::t>(
            std::move(lhs),
            std::move(rhs),
            std::move(op)
        );
        lhs = std::make_unique<expr::t>(std::move(bin), expr::type::Binary);
        cur = lexer::peek(lexer);
    }

    return lhs;
}

static un_ptr<expr::t> parse_equalitive_expr(lexer::t &lexer) {
    auto lhs = parse_additive_expr(lexer);
    auto cur = lexer::peek(lexer);

    while (cur && (cur->ty == token::type::Double_Equals
                   || cur->ty == token::type::Greaterthan_Equals
                   || cur->ty == token::type::Greaterthan
                   || cur->ty == token::type::Lessthan_Equals
                   || cur->ty == token::type::Lessthan
                   || cur->ty == token::type::Bang_Equals)) {
        auto op = lexer::next(lexer);
        auto rhs = parse_additive_expr(lexer);
        auto bin = std::make_unique<expr::binary::t>(
            std::move(lhs),
            std::move(rhs),
            std::move(op)
        );
        lhs = std::make_unique<expr::t>(std::move(bin), expr::type::Binary);
        cur = lexer::peek(lexer);
    }

    return lhs;
}

static un_ptr<expr::t> parse_logical_expr(lexer::t &lexer) {
    auto lhs = parse_equalitive_expr(lexer);
    auto cur = lexer::peek(lexer);

    while (cur && (cur->ty == token::type::Double_Ampersand
                   || cur->ty == token::type::Double_Pipe)) {
        auto op = lexer::next(lexer);
        auto rhs = parse_equalitive_expr(lexer);
        auto bin = std::make_unique<expr::binary::t>(
            std::move(lhs),
            std::move(rhs),
            std::move(op)
        );
        lhs = std::make_unique<expr::t>(std::move(bin), expr::type::Binary);
        cur = lexer::peek(lexer);
    }

    return lhs;
}

static un_ptr<expr::t> parse_expr(lexer::t &lexer) {
    return parse_logical_expr(lexer);
}

static un_ptr<stmt::block> parse_stmt_block(lexer::t &lexer) {
    ignore(expect(lexer, token::type::LBrace));

    vec<un_ptr<stmt::t>> stmts = {};

    while (lexer_speek(lexer)->ty != token::type::RBrace)
        stmts.push_back(parse_stmt(lexer));

    ignore(expect(lexer, token::type::RBrace));
    return std::make_unique<stmt::block>(std::move(stmts));
}

static un_ptr<stmt::proc> parse_stmt_proc(lexer::t &lexer) {
    lexer::discard(lexer); // proc
    auto id = expect(lexer, token::type::Ident);
    auto params = parse_proc_parameters(lexer);
    ignore(expect(lexer, token::type::Colon));
    auto rettype = expect_type(lexer);
    auto block = parse_stmt_block(lexer);
    return std::make_unique<stmt::proc>(std::move(id),
                                        std::move(params),
                                        std::move(rettype),
                                        std::move(block));
}

static un_ptr<stmt::def> parse_stmt_def(lexer::t &lexer) {
    lexer::discard(lexer); // def
    auto id = expect(lexer, token::type::Ident);
    auto params = parse_proc_parameters(lexer);
    ignore(expect(lexer, token::type::Colon));
    auto rettype = expect_type(lexer);
    ignore(expect(lexer, token::type::Semicolon));
    return std::make_unique<stmt::def>(
        std::move(id),
        std::move(params),
        std::move(rettype)
    );
}

static un_ptr<stmt::let> parse_stmt_let(lexer::t &lexer) {
    lexer::discard(lexer); // let
    auto id = expect(lexer, token::type::Ident);
    ignore(expect(lexer, token::type::Colon));
    auto ty = expect_type(lexer);
    ignore(expect(lexer, token::type::Equals));
    auto expr = parse_expr(lexer);
    ignore(expect(lexer, token::type::Semicolon));

    std::cout << scr_type::to_cxxstr(ty.get()) << std::endl;

    return std::make_unique<stmt::let>(std::move(id), std::move(expr));
}

static un_ptr<stmt::_return> parse_stmt_return(lexer::t &lexer) {
    lexer::discard(lexer); // return
    auto expr = parse_expr(lexer);
    ignore(expect(lexer, token::type::Semicolon));
    return std::make_unique<stmt::_return>(std::move(expr));
}

static un_ptr<stmt::_module> parse_stmt_module(lexer::t &lexer) {
    lexer::discard(lexer); // module
    auto id = expect(lexer, token::type::Ident);
    ignore(expect_keyword(lexer, COMMON_SCR_WHERE));
    return std::make_unique<stmt::_module>(std::move(id));
}

static un_ptr<stmt::t> parse_stmt(lexer::t &lexer) {
    auto top = lexer::peek(lexer);
    switch (top->ty) {
        case token::type::Keyword: {
            if (top->lx == COMMON_SCR_LET)
                return std::make_unique<stmt::t>(parse_stmt_let(lexer), stmt::type::Let);
            if (top->lx == COMMON_SCR_PROC)
                return std::make_unique<stmt::t>(parse_stmt_proc(lexer), stmt::type::Proc);
            if (top->lx == COMMON_SCR_DEF)
                return std::make_unique<stmt::t>(parse_stmt_def(lexer), stmt::type::Def);
            if (top->lx == COMMON_SCR_RETURN)
                return std::make_unique<stmt::t>(parse_stmt_return(lexer), stmt::type::Return);
            if (top->lx == COMMON_SCR_MODULE)
                return std::make_unique<stmt::t>(parse_stmt_module(lexer), stmt::type::Module);
            ERRW("invalid statement: `%s`", top->lx.c_str());
        } break;
        default: {
            std::cerr << "invalid statement at token: " << token::type_to_cxxstr(lexer::peek(lexer)->ty) << std::endl;
            std::exit(EXIT_FAILURE);
        } break;
    }
    return nullptr; // unreachable
}

un_ptr<program::t> parser::parse(lexer::t &lexer) {
    vec<un_ptr<stmt::t>> stmts = {};

    while (lexer_speek(lexer)->ty != token::type::Eof)
        stmts.push_back(parse_stmt(lexer));

    return std::make_unique<program::t>(std::move(stmts));
}
