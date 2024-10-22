#ifndef GRAMMAR_HXX
#define GRAMMAR_HXX

#include <optional>
#include <variant>

#include "token.hxx"
#include "utils.hxx"
#include "types.hxx"

namespace stmt { struct t; };

namespace expr {
    struct t;
    namespace term { struct t; };
    namespace binary { struct t; };
    namespace unary { struct t; };
}

namespace stmt {
    enum class type {
        Proc = 0,
        Let,
        Mut,
        If,
        While,
        For,
        Block,
        Return,
        Module,
        Def,
    };

    struct parameter {
        sh_ptr<token::t> id;
        un_ptr<scr_type::t> ty;
        parameter(sh_ptr<token::t> id, un_ptr<scr_type::t> ty);
        ~parameter() = default;
    };

    struct def {
        sh_ptr<token::t> id;
        vec<un_ptr<stmt::parameter>> params;
        un_ptr<scr_type::t> rettype;
        def(sh_ptr<token::t> id,
            vec<un_ptr<stmt::parameter>> params,
            un_ptr<scr_type::t> rettype);
        ~def() = default;
    };

    struct _return {
        un_ptr<expr::t> expr;
        _return(un_ptr<expr::t> expr);
        ~_return() = default;
    };

    struct _module {
        sh_ptr<token::t> tok;
        _module(sh_ptr<token::t> tok);
        ~_module() = default;
    };

    struct block {
        vec<un_ptr<stmt::t>> stmts;
        block(vec<un_ptr<stmt::t>> stmts);
        ~block() = default;
    };

    struct _for {
        un_ptr<stmt::t> init;
        un_ptr<expr::t> cond;
        un_ptr<stmt::t> after;
        un_ptr<stmt::block> block;
        _for(un_ptr<stmt::t> init,
             un_ptr<expr::t> cond,
             un_ptr<stmt::t> after,
             un_ptr<stmt::block> block);
        ~_for() = default;
    };

    struct _while {
        un_ptr<expr::t> cond;
        un_ptr<stmt::block> block;
        _while(un_ptr<expr::t> cond, un_ptr<stmt::block> block);
        ~_while() = default;
    };

    struct _if {
        un_ptr<expr::t> cond;
        un_ptr<stmt::block> block;
        optional<un_ptr<stmt::block>> _else;
        _if(un_ptr<expr::t> cond,
            un_ptr<stmt::block> block,
            optional<un_ptr<stmt::block>> _else);
        ~_if() = default;
    };

    struct mut {
        un_ptr<expr::t> lhs;
        un_ptr<expr::t> rhs;
        sh_ptr<token::t> op;
        mut(un_ptr<expr::t> lhs, un_ptr<expr::t> rhs, sh_ptr<token::t> op);
        ~mut() = default;
    };

    struct let {
        sh_ptr<token::t> id;
        un_ptr<expr::t> expr;
        un_ptr<scr_type::t> ty;
        let(sh_ptr<token::t> id, un_ptr<expr::t> expr, un_ptr<scr_type::t> ty);
        ~let() = default;
    };

    struct proc {
        sh_ptr<token::t> id;
        vec<un_ptr<parameter>> params;
        sh_ptr<scr_type::t> rettype;
        un_ptr<stmt::block> block;

        proc(sh_ptr<token::t> id,
             vec<un_ptr<parameter>> params,
             sh_ptr<scr_type::t> rettype,
             un_ptr<stmt::block> block);
        ~proc() = default;
    };

    using vt = std::variant<un_ptr<stmt::proc>,
                            un_ptr<stmt::let>,
                            un_ptr<stmt::mut>,
                            un_ptr<stmt::_if>,
                            un_ptr<stmt::_while>,
                            un_ptr<stmt::_for>,
                            un_ptr<stmt::block>,
                            un_ptr<stmt::_module>,
                            un_ptr<stmt::_return>,
                            un_ptr<stmt::def>>;
    struct t {
        vt actual;
        stmt::type ty;
        t(vt actual, stmt::type ty);
        ~t() = default;
    };
};

namespace expr::unary {
    struct t {
        un_ptr<expr::t> rhs;
        sh_ptr<token::t> op;
        t(un_ptr<expr::t> rhs, sh_ptr<token::t> op);
        ~t() = default;
    };
};

namespace expr::binary {
    struct t {
        un_ptr<expr::t> lhs;
        un_ptr<expr::t> rhs;
        sh_ptr<token::t> op;
        t(un_ptr<expr::t> lhs, un_ptr<expr::t> rhs, sh_ptr<token::t> op);
        ~t() = default;
    };
};

namespace expr::term {
    enum class type {
        Ident,
        Int_Literal,
        Str_Literal,
        Proc_Call,
    };

    struct proc_call {
        str id;
        vec<un_ptr<expr::t>> args;
        proc_call(str id, vec<un_ptr<expr::t>> args);
        ~proc_call() = default;
    };

    struct identifier {
        sh_ptr<token::t> tok;
        identifier(sh_ptr<token::t> tok);
        ~identifier() = default;
    };

    struct str_literal {
        sh_ptr<token::t> tok;
        str_literal(sh_ptr<token::t> tok);
        ~str_literal() = default;
    };

    struct int_literal {
        sh_ptr<token::t> tok;
        int_literal(sh_ptr<token::t> tok);
        ~int_literal() = default;
    };

    using vt = std::variant<un_ptr<expr::term::identifier>,
                            un_ptr<expr::term::str_literal>,
                            un_ptr<expr::term::int_literal>,
                            un_ptr<expr::term::proc_call>>;
    struct t {
        vt actual;
        expr::term::type ty;
        t(vt actual, expr::term::type ty);
        ~t() = default;
    };
};

namespace expr {
    enum class type {
        Term,
        Binary,
        Unary,
    };

    using vt = std::variant<un_ptr<expr::term::t>,
                            un_ptr<expr::binary::t>,
                            un_ptr<expr::unary::t>>;

    struct t {
        expr::vt actual;
        expr::type ty;
        t(vt actual, expr::type ty);
        ~t() = default;
    };
};

namespace program {
    struct t {
        vec<un_ptr<stmt::t>> stmts;
        t(vec<un_ptr<stmt::t>> stmts);
        ~t() = default;
    };
};

#endif // GRAMMAR_HXX
