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
import gatherIdentifiers;
import depTracker;
import semanticSymbols;
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

        // Assemble each .asm file into its corresponding .o file
        for (size_t i = 0; i < gAsmFiles.length; ++i) {
                string[] nasmArgs = ["nasm", "-f", "elf64", gAsmFiles[i], "-o", gObjFiles[i], "-g", "-F dwarf"];
                auto nasmResult = execute(nasmArgs);
                if (nasmResult.status != 0) {
                        writeln("Assembly failed for ", gAsmFiles[i], ":");
                        writeln(nasmResult.output);
                        exit(1); // Exit with error code
                }
        }

        // Link all object files into the final executable
        string[] linkArgs = ["gcc", "-no-pie"];
        linkArgs ~= gObjFiles;
        linkArgs ~= ["-o", outputName, "-g"];

        auto linkResult = execute(linkArgs);
        if (linkResult.status != 0) {
                writeln("[Score: Linking failed]:");
                writeln(linkResult.output);
                exit(1);
        }

        // Cleanup temporary files if --no-cleanup is not specified
        if ((fp.flags & FlagType.NoCleanup) == 0) {
                for (size_t i = 0; i < gAsmFiles.length; ++i) {
                        if (exists(gAsmFiles[i])) {
                                remove(gAsmFiles[i]);
                        }
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
        DepTbl[] dts = [];
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
                dts      ~= determineDeps(&programs[i], fp.paths[i]);
        }

        for (size_t i = 0; i < programs.length; ++i) {
                // Pass current ig
                symTbls ~= semSymCheck(&programs[i], igs, dts[i], &igs[i]);
        }

        // Check for multiple main definitions
        size_t mainCount = 0;
        size_t mainFileIndex = 0;
        for (size_t i = 0; i < symTbls.length; ++i) {
                if ("main" in symTbls[i].procs) {
                        mainCount++;
                        mainFileIndex = i;
                        if (mainCount > 1) {
                                writeln("Error: Multiple definitions of 'main' found across files:");
                                for (size_t j = 0; j <= i; ++j) {
                                        if ("main" in symTbls[j].procs) {
                                                writeln("  - Defined in ", fp.paths[j]);
                                        }
                                }
                                exit(1);
                        }
                }
        }
        if (mainCount == 0) {
                writeln("Error: No 'main' function defined in any file.");
                exit(1);
        }

        // Do not do codegen if any errors were encountered
        for (size_t i = 0; i < igs.length; ++i) {
                if (!igs[i].ok || !symTbls[i].ok) {
                        exit(1);
                }
        }

        // Perform codegen
        for (size_t i = 0; i < programs.length; ++i) {
                char[] asm_ = gen(&programs[i], igs);
                if (fp.flags & FlagType.ShowAsm) {
                        writeln("--- Generated assembly for file: ", fp.paths[i], " ---");
                        writeln(asm_);
                }
                // writeX86_64AsmFile(asm_, fp.paths[i]);
        }

        // assembleX86_64AsmFiles(fp);
        return 0;
}

