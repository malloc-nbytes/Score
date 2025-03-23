import std.stdio;
import std.file : readText;

import lexer;
import token;
import grammar;
import parser;
import visitor;
import semanticSymbols;

int main() {
        const string fp = "./input.scr";
        const string src = readText(fp);

        Lexer lexer = lexFile(src, fp);
        lexerDump(&lexer);

        Program p = parseProgram(&lexer);
        semSymCheck(&p);

        return 0;
}
