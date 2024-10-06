#ifndef GRAMMAR_HXX
#define GRAMMAR_HXX

#include <optional>
#include <variant>

#include "token.hxx"
#include "utils.hxx"

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
        std::optional<un_ptr<stmt::block>> _else;
        _if(un_ptr<expr::t> cond,
            un_ptr<stmt::block> block,
            std::optional<un_ptr<stmt::block>> _else);
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
        let(sh_ptr<token::t> id, un_ptr<expr::t> expr);
        ~let() = default;
    };

    struct proc {
        struct parameter {
            sh_ptr<token::t> id;
            sh_ptr<token::t> ty;
            parameter(sh_ptr<token::t> id, sh_ptr<token::t> ty);
            ~parameter() = default;
        };

        sh_ptr<token::t> id;
        vec<parameter> params;
        sh_ptr<token::t> rettype;

        proc(sh_ptr<token::t> id,
             vec<parameter> params,
             sh_ptr<token::t> rettype);
        ~proc() = default;
    };

    using vt = std::variant<sh_ptr<stmt::proc>,
                            sh_ptr<stmt::let>,
                            sh_ptr<stmt::mut>,
                            sh_ptr<stmt::_if>,
                            sh_ptr<stmt::_while>,
                            sh_ptr<stmt::_for>,
                            sh_ptr<stmt::block>>;
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

    using vt = std::variant<sh_ptr<expr::term::identifier>,
                            sh_ptr<expr::term::str_literal>,
                            sh_ptr<expr::term::int_literal>>;
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

    using vt = std::variant<sh_ptr<expr::term::t>,
                            sh_ptr<expr::binary::t>,
                            sh_ptr<expr::unary::t>>;

    struct t {
        vt actual;
        expr::type ty;
        t(vt actual, expr::type ty);
        ~t() = default;
    };
};

#endif // GRAMMAR_HXX
