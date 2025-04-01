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
import flag;
import semantic;
import types;

void usage() {
        writeln("Usage: scr [paths...] [options...]");
        writeln("Options:");
        writeln("    ", FLAG_2HYPH_HELP, ",       ", FLAG_1HYPH_HELP, "   show this message");
        writeln("    ", FLAG_2HYPH_OUTPUT, ",     ", FLAG_1HYPH_OUTPUT, "   change the output file name");
        writeln("    ", FLAG_2HYPH_SHOW_ASM, ",   ", FLAG_1HYPH_SHOW_ASM, "   print all generated assembly instructions");
        writeln("    ", FLAG_2HYPH_NO_CLEANUP, ", ", FLAG_1HYPH_NO_CLEANUP, "   do not clean up files generated");
        exit(0);
}
        // for (size_t i = 0; i < p.stmts.length; ++i) {
        //         if (p.stmts[i].kind == StmtType.Proc) {
        //                 auto proc = cast(StmtProc)p.stmts[i];
        //                 for (size_t j = 0; j < proc.block.stmts.length; ++j) {
        //                         if (proc.block.stmts[j].kind == StmtType.Expr) {
        //                                 auto e = (cast(StmtExpr)proc.block.stmts[j]).expr;
        //                                 auto mem = cast(ExprMember)e;
        //                                 auto left = mem.left;
        //                                 auto right = mem.right;
        //                                 writeln((cast(ExprIdent)left).name, ' ', (cast(ExprIdent)right).name);
        //                         }
        //                 }
        //         }
        // }

int main(string[] args) {
        if (args.length < 2) {
                usage();
        }

        args = args[1..$];

        const string fp = "./input.scr";
        const string src = readText(fp);
        Lexer l = lexFile(src, fp);
        Program p = parseProgram(&l);

        semanticAnalyze(p);

        return 0;
}

