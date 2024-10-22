#include "grammar.hxx"
#include "utils.hxx"

stmt::def::def(sh_ptr<token::t> id, vec<un_ptr<stmt::parameter>> params,
               un_ptr<scr_type::t> rettype, bool variadic)
    : id(std::move(id)), params(std::move(params)), rettype(std::move(rettype)),
      variadic(variadic) {}

stmt::_return::_return(un_ptr<expr::t> expr)
    : expr(std::move(expr)) {}

stmt::let::let(sh_ptr<token::t> id, un_ptr<expr::t> expr, un_ptr<scr_type::t> ty)
    : id(std::move(id)), expr(std::move(expr)), ty(std::move(ty)) {}

stmt::mut::mut(un_ptr<expr::t> lhs, un_ptr<expr::t> rhs, sh_ptr<token::t> op)
    : lhs(std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

stmt::proc::proc(sh_ptr<token::t> id, vec<un_ptr<parameter>> params,
                 sh_ptr<scr_type::t> rettype, un_ptr<stmt::block> block,
                 bool variadic)
    : id(std::move(id)), params(std::move(params)), rettype(std::move(rettype)),
      block(std::move(block)), variadic(variadic) {}

stmt::parameter::parameter(sh_ptr<token::t> id, un_ptr<scr_type::t> ty)
    : id(std::move(id)), ty(std::move(ty)) {}

stmt::_module::_module(sh_ptr<token::t> tok)
    : tok(std::move(tok)) {}

stmt::_if::_if(un_ptr<expr::t> cond, un_ptr<stmt::block> block, optional<un_ptr<stmt::block>> _else)
    : cond(std::move(cond)), block(std::move(block)), _else(std::move(_else)) {}

stmt::_while::_while(un_ptr<expr::t> cond, un_ptr<stmt::block> block)
    : cond(std::move(cond)), block(std::move(block)) {}

stmt::_for::_for(un_ptr<stmt::t> init, un_ptr<expr::t> cond,
                 un_ptr<stmt::t> after, un_ptr<stmt::block> block)
    : init(std::move(init)), cond(std::move(cond)),
      after(std::move(after)), block(std::move(block)) {}

stmt::block::block(vec<un_ptr<stmt::t>> stmts)
    : stmts(std::move(stmts)) {}

stmt::t::t(vt actual, stmt::type ty)
    : actual(std::move(actual)), ty(ty) {}

expr::unary::t::t(un_ptr<expr::t> rhs, sh_ptr<token::t> op)
    : rhs(std::move(rhs)), op(std::move(op)) {}

expr::binary::t::t(un_ptr<expr::t> lhs, un_ptr<expr::t> rhs, sh_ptr<token::t> op)
    : lhs(std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

expr::term::proc_call::proc_call(str id, vec<un_ptr<expr::t>> args)
    : id(std::move(id)), args(std::move(args)) {}

expr::term::identifier::identifier(sh_ptr<token::t> tok)
    : tok(std::move(tok)) {}

expr::term::str_literal::str_literal(sh_ptr<token::t> tok)
    : tok(std::move(tok)) {}

expr::term::int_literal::int_literal(sh_ptr<token::t> tok)
    : tok(std::move(tok)) {}

expr::term::t::t(vt actual, expr::term::type ty)
    : actual(std::move(actual)), ty(ty) {}

expr::t::t(vt actual, expr::type ty)
    : actual(std::move(actual)), ty(ty) {}

program::t::t(vec<un_ptr<stmt::t>> stmts)
    : stmts(std::move(stmts)) {}
