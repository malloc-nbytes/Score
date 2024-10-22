#include <string>

#include "utils.hxx"
#include "token.hxx"
#include "err.hxx"

str token::type_to_cxxstr(token::type ty) {
    switch (ty) {
    case token::type::LParen:             return "LParen";
    case token::type::RParen:             return "RParen";
    case token::type::LBracket:           return "LBracket";
    case token::type::RBracket:           return "RBracket";
    case token::type::LBrace:             return "LBrace";
    case token::type::RBrace:             return "RBrace";
    case token::type::Hash:               return "Hash";
    case token::type::Period:             return "Period";
    case token::type::Semicolon:          return "Semicolon";
    case token::type::Comma:              return "Comma";
    case token::type::Greaterthan:        return "Greaterthan";
    case token::type::Lessthan:           return "Lessthan";
    case token::type::Equals:             return "Equals";
    case token::type::Ampersand:          return "Ampersand";
    case token::type::Asterisk:           return "Asterisk";
    case token::type::Plus:               return "Plus";
    case token::type::Minus:              return "Minus";
    case token::type::Forwardslash:       return "Forwardslash";
    case token::type::Pipe:               return "Pipe";
    case token::type::Caret:              return "Caret";
    case token::type::Questionmark:       return "Questionmark";
    case token::type::Backwardslash:      return "Backwardslash";
    case token::type::Bang:               return "Bang";
    case token::type::At:                 return "At";
    case token::type::Dollarsign:         return "Dollarsign";
    case token::type::Percent:            return "Percent";
    case token::type::Backtick:           return "Backtick";
    case token::type::Tilde:              return "Tilde";
    case token::type::Colon:              return "Colon";
    case token::type::Double_Ampersand:   return "Double_Ampersand";
    case token::type::Double_Pipe:        return "Double_Pipe";
    case token::type::Greaterthan_Equals: return "Greaterthan_Equals";
    case token::type::Lessthan_Equals:    return "Lessthan_Equals";
    case token::type::Double_Equals:      return "Double_Equals";
    case token::type::Bang_Equals:        return "Bang_Equals";
    case token::type::Double_Colon:       return "Double_Colon";
    case token::type::Eof:                return "Eof";
    case token::type::Intlit:             return "Intlit";
    case token::type::Strlit:             return "Strlit";
    case token::type::Charlit:            return "Charlit";
    case token::type::Floatlit:           return "Floatlit";
    case token::type::Ident:              return "Ident";
    case token::type::Keyword:            return "Keyword";
    case token::type::Type:               return "Type";
    case token::type::TriplePeriod:       return "TriplePeriod";
    default: ERRW("unknown token type `%d`", ty);
    }
    return ""; // unreachable
}

token::t::t(str lx, token::type ty, unsigned row, unsigned col, str fp)
    : lx(lx), ty(ty), row(row), col(col), fp(fp), next(nullptr) {}

