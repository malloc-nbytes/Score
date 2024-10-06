#ifndef TOKEN_HXX
#define TOKEN_HXX

#include <string>

#include "utils.hxx"

namespace token {
    enum class type {
        LParen,
        RParen,
        LBracket,
        RBracket,
        LBrace,
        RBrace,
        Hash,
        Period,
        Semicolon,
        Comma,
        Greaterthan,
        Lessthan,
        Equals,
        Ampersand,
        Asterisk,
        Plus,
        Minus,
        Forwardslash,
        Pipe,
        Caret,
        Questionmark,
        Backwardslash,
        Bang,
        At,
        Dollarsign,
        Percent,
        Backtick,
        Tilde,
        Colon,

        Double_Ampersand,
        Double_Pipe,
        Greaterthan_Equals,
        Lessthan_Equals,
        Double_Equals,
        Bang_Equals,
        Double_Colon,

        // Other
        Eof,
        Intlit,
        Strlit,
        Charlit,
        Floatlit,
        Ident,
        Keyword,
        Type,
    };

    struct t {
        str lx;
        token::type ty;
        unsigned row;
        unsigned col;
        str fp;
        sh_ptr<t> next;

        t(str lx, token::type ty, unsigned row, unsigned col, str fp);
    };

    str type_to_cxxstr(type ty);
}

#endif // TOKEN_HXX
