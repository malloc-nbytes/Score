import std.stdio;
//import std.file : readText, write, exists, remove;
import std.file : readText, getSize;
import std.process : execute;
import core.stdc.stdlib : exit;

import utils;
import lexer;
import token;
import grammar;
import parser;
import visitor;
import flag;
import semantic;
import types;
import codegen;
import assemble;

void usage() {
        writeln("Usage: scr [paths...] [options...]");
        writeln("Options:");
        writeln("    ", FLAG_2HYPH_HELP, ",       ", FLAG_1HYPH_HELP, "   show this message");
        writeln("    ", FLAG_2HYPH_OUTPUT, ",     ", FLAG_1HYPH_OUTPUT, "   change the output file name");
        writeln("    ", FLAG_2HYPH_SHOW_ASM, ",   ", FLAG_1HYPH_SHOW_ASM, "   print all generated assembly instructions");
        writeln("    ", FLAG_2HYPH_NO_CLEANUP, ", ", FLAG_1HYPH_NO_CLEANUP, "   do not clean up files generated");
        exit(0);
}

int main(string[] args) {
        if (args.length < 2) {
                usage();
        }

        args = args[1..$];

        FlagParser flagParser = handleArgs(args);
        assert(flagParser.paths.length == 1);

        string outputName = flagParser.outputName;
        if (outputName.length == 0) {
                outputName = "output";
        }

        const string fp = flagParser.paths[0];
        const string src = readText(fp);

        write("[      ] Lexing...\r"); stdout.flush();
        Lexer l = lexFile(src, fp);

        write("[*     ] Parsing...\r"); stdout.flush();
        Program p = parseProgram(&l);

        write("[**    ] Semantic...\r"); stdout.flush();
        SemanticAnalyzer ana = semanticAnalyze(p);

        write("[***   ] Codegen...\r"); stdout.flush();
        gen(p, outputName);

        write("[****  ] Assembling...\r"); stdout.flush();
        nasm_assemble(outputName);

        write("[***** ] Linking Executable...\r"); stdout.flush();
        ld(outputName);

        ulong size = getSize(outputName);
        writeln("                             \r[******] ", fp, " ok (", size, " bytes)");
        return 0;
}

