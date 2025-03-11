#include <stdio.h>

#include "lexer.hxx"
#include "token.hxx"
#include "parser.hxx"
#include "codegen.hxx"
#include "grammar.hxx"
#include "utils.hxx"

int main(void) {
        const char *fp = "./input.scr";
        char *src = file_to_cstr(fp);

        Lexer lexer = lexer_init(fp, src);
        //lexer_dbg_dump(&lexer);
        Program program = parse(&lexer);
        program_dump(&program);
        codegen(&program);

        return 0;
}
