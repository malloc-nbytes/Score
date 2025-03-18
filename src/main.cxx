#include <stdio.h>
#include <stdlib.h>

#include "lexer.hxx"
#include "token.hxx"
#include "parser.hxx"
#include "codegen.hxx"
#include "grammar.hxx"
#include "utils.hxx"

int main(int argc, char **argv) {
        if (argc < 2) {
                printf("Usage: scr <filepath>\n");
                exit(1);
        }
        --argc, ++argv;

        const char *fp = *argv;
        char *src = file_to_cstr(fp);

        Lexer lexer = lexer_init(fp, src);
        //lexer_dbg_dump(&lexer);
        Program program = parse(&lexer);
        //program_dump(&program);
        codegen(&program);

        return 0;
}
