#ifndef LEXER_HXX
#define LEXER_HXX

#include <string>

#include "token.hxx"
#include "utils.hxx"

#define lexer_speek(l) lexer::peek(l) && lexer::peek(l)

namespace lexer {
    struct t {
        sh_ptr<token::t> hd;
        token::t *tl;
        t();
    };

    token::t *peek(t &lexer);
    void append(t &lexer, sh_ptr<token::t> tok);
    sh_ptr<token::t> next(t &lexer);
    void discard(t &lexer);
    void dump(t &lexer);

    std::string file_to_str(const std::string &file_path);
    lexer::t lex(str &src_code, str fp);
}

#endif // LEXER_HXX
