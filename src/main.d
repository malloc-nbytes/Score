import std.stdio;
import std.file : readText;
import core.stdc.stdlib : exit;

import lexer;
import token;
import grammar;
import parser;
import visitor;
import semanticSymbols;
import codegen;

void usage() {
        writeln("Usage: scr [paths...]");
        exit(0);
}

int main(string[] args) {
        if (args.length < 2) {
                usage();
        }

        string[] paths = [];
        Lexer[] lexers = [];
        Program[] programs = [];

        for (size_t i = 1; i < args.length; ++i) {
                paths ~= args[i];
                const string src = readText(args[i]);
                lexers ~= lexFile(src, args[i]);
                programs ~= parseProgram(&lexers[i-1]);
                semSymCheck(&programs[i-1]);
        }

        // gen(&p);

        return 0;
}
