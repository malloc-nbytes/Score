#ifndef PARSER_HXX
#define PARSER_HXX

#include "grammar.hxx"
#include "lexer.hxx"

namespace parser {
    un_ptr<program::t> parse(lexer::t &lexer);
};

#endif // PARSER_HXX
