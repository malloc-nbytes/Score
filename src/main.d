import std.stdio;
import std.file : readText;

import lexer;
import token;
import grammar;
import parser;

int main() {
        const string fp = "./input.scr";
        const string src = readText(fp);

        Lexer lexer = lexFile(src, fp);
        lexerDump(&lexer);

        Program p = parseProgram(&lexer);

        return 0;
}
