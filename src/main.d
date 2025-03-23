import std.stdio;
import std.file : readText;

import lexer;
import token;
import grammar;
import parser;
import visitor;
import semanticSymbols;
import codegen;

int main() {
        const string fp = "./input.scr";
        const string src = readText(fp);

        Lexer lexer = lexFile(src, fp);
        Program p = parseProgram(&lexer);

        semSymCheck(&p);
        gen(&p);

        return 0;
}
