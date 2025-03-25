import std.stdio;
import std.file : readText, write, exists, remove;
import std.process : execute;
import core.stdc.stdlib : exit;

import utils;
import lexer;
import token;
import grammar;
import parser;
import visitor;
import semanticSymbols;
import gatherIdentifiers;
import codegen;
import flag;

static string[] gAsmFiles = [];
static string[] gObjFiles = [];

void usage() {
        writeln("Usage: scr [paths...] [options...]");
        writeln("Options:");
        writeln("    ", FLAG_2HYPH_HELP, ",       ", FLAG_1HYPH_HELP, "   show this message");
        writeln("    ", FLAG_2HYPH_OUTPUT, ",     ", FLAG_1HYPH_OUTPUT, "   change the output file name");
        writeln("    ", FLAG_2HYPH_SHOW_ASM, ",   ", FLAG_1HYPH_SHOW_ASM, "   print all generated assembly instructions");
        writeln("    ", FLAG_2HYPH_NO_CLEANUP, ", ", FLAG_1HYPH_NO_CLEANUP, "   do not clean up files generated");
        exit(0);
}

void assembleX86_64AsmFiles(FlagParser fp) {
        assert(gAsmFiles.length == gObjFiles.length);

        string outputName = fp.outputName.length == 0 ? "a.out" : fp.outputName;
        string asmFiles = "";
        string objFiles = "";

        for (size_t i = 0; i < gAsmFiles.length; ++i) {
                if (i != 0) {
                        asmFiles ~= " ";
                        objFiles ~= " ";
                }
                asmFiles ~= gAsmFiles[i];
                objFiles ~= gObjFiles[i];
        }

        string[] nasmArgs = ["nasm", "-f", "elf64", asmFiles, "-o", objFiles, "-g", "-F dwarf"];
        string[] linkArgs = ["gcc", "-no-pie", objFiles, "-o", outputName, "-g"];

        auto nasmResult = execute(nasmArgs);
        if (nasmResult.status != 0) {
                writeln("Assembly failed:");
                writeln(nasmResult.output);
        } else {
                auto linkResult = execute(linkArgs);
                if (linkResult.status != 0) {
                        writeln("Linking failed:");
                        writeln(linkResult.output);
                } else {
                        writeln("Successfully compiled and linked to ", outputName);
                }
        }

        if ((fp.flags & FlagType.NoCleanup) == 0) {
                for (size_t i = 0; i < gAsmFiles.length; ++i) {
                        if (exists(gAsmFiles[i])) {
                                remove(gAsmFiles[i]);
                        }
                }

                for (size_t i = 0; i < gObjFiles.length; ++i) {
                        if (exists(gObjFiles[i])) {
                                remove(gObjFiles[i]);
                        }
                }
        }
}

void writeX86_64AsmFile(const ref char[] asm_, const ref string name) {
        string asmFile = name~".asm";
        string objFile = name~".o";
        write(asmFile, asm_);
        gAsmFiles ~= asmFile;
        gObjFiles ~= objFile;
}

int main(string[] args) {
        if (args.length < 2) {
                usage();
        }

        args = args[1..$];

        FlagParser fp = handleArgs(args);
        Lexer[] lexers = [];
        Program[] programs = [];
        IdentGatherer[] igs = [];
        SymTblChecker[] symTbls = [];

        if (fp.flags & FlagType.Help) {
                usage();
        }

        // Perform all pre-codegen analysis
        for (size_t i = 0; i < fp.paths.length; ++i) {
                const string src = readText(fp.paths[i]);
                lexers   ~= lexFile(src, fp.paths[i]);
                programs ~= parseProgram(&lexers[i]);
                igs      ~= getIdents(&programs[i]);
                symTbls  ~= semSymCheck(&programs[i]);
        }

        assert(fp.paths.length == lexers.length);
        assert(fp.paths.length == programs.length);
        assert(fp.paths.length == igs.length);
        assert(fp.paths.length == symTbls.length);

        // Do not do codegen if any errors were encountered.
        for (size_t i = 0; i < igs.length; ++i) {
                if (!igs[i].ok || !symTbls[i].ok) {
                        exit(1);
                }
        }

        // Perform codegen.
        for (size_t i = 0; i < programs.length; ++i) {
                char[] asm_ = gen(&programs[i]);
                if (fp.flags & FlagType.ShowAsm) {
                        writeln("--- Generated assembly for file: ", fp.paths[i], " ---");
                        writeln(asm_);
                }
                writeX86_64AsmFile(asm_, fp.paths[i]);
        }

        assembleX86_64AsmFiles(fp);

        return 0;
}
