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
        if (args.length < 2) { usage(); }

        args = args[1..$];

        FlagParser flagParser = handleArgs(args);
        assert(flagParser.paths.length == 1);

        string outputName = flagParser.outputName;
        if (outputName.length == 0) {
                outputName = "output";
        }

        const string fp = flagParser.paths[0];
        const string src = readText(fp);

        write("[      ] ", fp, " Lexing...\r"), stdout.flush();
        Lexer l = lexFile(src, fp);

        write("\033[K[*     ] ", fp, " Parsing...\r"), stdout.flush();
        Program p = parseProgram(&l);

        write("\033[K[**    ] ", fp, " Semantic...\r"), stdout.flush();
        SemanticAnalyzer ana = semanticAnalyze(p);

        write("\033[K[***   ] ", fp, " Codegen...\r"), stdout.flush();
        gen(p, outputName);

        write("\033[K[****  ] ", fp, " Assembling...\r"), stdout.flush();
        nasm_assemble(outputName);

        write("\033[K[***** ] ", fp, " Linking Executable...\r"), stdout.flush();
        ld(outputName, (flagParser.flags & FlagType.Lc) != 0);

        ulong size = getSize(outputName);
        writeln("\033[K[******] ", fp, " ok (", size, " bytes)"), stdout.flush();
        return 0;
}

