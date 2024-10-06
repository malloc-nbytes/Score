#include <iostream>

#include "lexer.hxx"
#include "grammar.hxx"
#include "parser.hxx"

int main(void) {
    std::string fp = "input.scr";
    std::string content = lexer::file_to_str(fp);

    lexer::t lexer = lexer::lex(content, fp);
    auto program = parser::parse(lexer);

    return 0;
}
