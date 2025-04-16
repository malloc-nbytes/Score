module flag;

import std.stdio;
import core.stdc.stdlib : exit;

import utils;

public static const string FLAG_1HYPH_HELP = "-h";
public static const string FLAG_1HYPH_OUTPUT = "-o";
public static const string FLAG_1HYPH_SHOW_ASM = "-s";
public static const string FLAG_1HYPH_NO_CLEANUP = "-n";

public static const string FLAG_2HYPH_HELP = "--help";
public static const string FLAG_2HYPH_OUTPUT = "--output";
public static const string FLAG_2HYPH_SHOW_ASM = "--show-asm";
public static const string FLAG_2HYPH_NO_CLEANUP = "--no-cleanup";
public static const string FLAG_2HYPH_LC = "--lc";

enum FlagType {
        Output = 1 << 0,
        ShowAsm = 1 << 1,
        NoCleanup = 1 << 2,
        Help = 1 << 3,
        Lc = 1 << 4,
}

class FlagParser {
        string[] args;
        string[] paths;
        uint flags;
        string outputName;

        this(string[] args) {
                this.args = args;
                this.paths = [];
                this.flags = 0x0;
                this.outputName = "";
        }

        string eat() {
                assert(this.args.length > 0);
                string a = this.args[0];
                this.args = this.args[1..$];
                return a;
        }
}

private void handleOutputFlag(FlagParser fp) {
        fp.flags |= FlagType.Output;
        cast(void)fp.eat();
        string o = fp.eat();
        fp.outputName = o;
}

private void handleShowAsmFlag(FlagParser fp) {
        fp.flags |= FlagType.ShowAsm;
        cast(void)fp.eat();
}

private void handleNoCleanupFlag(FlagParser fp) {
        fp.flags |= FlagType.NoCleanup;
        cast(void)fp.eat();
}

private void handleHelpFlag(FlagParser fp) {
        fp.flags |= FlagType.Help;
        cast(void)fp.eat();
}

private void handleLcFlag(FlagParser fp) {
        fp.flags |= FlagType.Lc;
        cast(void)fp.eat();
}

FlagParser handleArgs(ref string[] args) {
        FlagParser fp = new FlagParser(args);
        while (fp.args.length > 0) {
                if (fp.args[0] == FLAG_2HYPH_OUTPUT || fp.args[0] == FLAG_1HYPH_OUTPUT) {
                        handleOutputFlag(fp);
                } else if (fp.args[0] == FLAG_2HYPH_SHOW_ASM || fp.args[0] == FLAG_1HYPH_SHOW_ASM) {
                        handleShowAsmFlag(fp);
                } else if (fp.args[0] == FLAG_2HYPH_NO_CLEANUP || fp.args[0] == FLAG_1HYPH_NO_CLEANUP) {
                        handleNoCleanupFlag(fp);
                } else if (fp.args[0] == FLAG_2HYPH_HELP || fp.args[0] == FLAG_1HYPH_HELP) {
                        handleHelpFlag(fp);
                } else if (fp.args[0] == FLAG_2HYPH_LC) {
                        handleLcFlag(fp);
                } else if (fp.args[0][0] == '-') {
                        err("Unknown flag: " ~ fp.args[0]);
                        exit(1);
                } else {
                        fp.paths ~= fp.eat();
                }
        }
        return fp;
}
