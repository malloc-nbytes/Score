module gatherIdentifiers;

import std.stdio;

import std.format;

import token;
import grammar;
import visitor;

class IdentGatherer {
        StmtMod mod;
        StmtProc[] procs;
        StmtStruct[] structs;
        StmtLet[] lets;
        string[] errs;
        bool ok;

        this() {
                this.mod = null;
                this.procs = [];
                this.structs = [];
                this.lets = [];
                this.errs = [];
                this.ok = true;
        }

        void reportErr(string msg) {
                this.errs ~= msg;
                this.ok = false;
        }
}

IdentGatherer getIdents(Program* p) {
        IdentGatherer ig = new IdentGatherer();

        for (size_t i = 0; i < p.stmts.length; ++i) {
                if (p.stmts[i].ty == StmtType.Mod) {
                        StmtMod m = cast(StmtMod)p.stmts[i];
                        if (ig.mod !is null) {
                                ig.reportErr(tokerrToStr(m.id) ~ format("Duplicate module `%s` found", m.id.lx));
                        } else {
                                ig.mod = m;
                        }
                } else if (p.stmts[i].ty == StmtType.Proc) {
                        ig.procs ~= cast(StmtProc)p.stmts[i];
                } else if (p.stmts[i].ty == StmtType.Let) {
                        ig.lets ~= cast(StmtLet)p.stmts[i];
                } else if (p.stmts[i].ty == StmtType.Struct) {
                        ig.structs ~= cast(StmtStruct)p.stmts[i];
                }
        }

        if (ig.errs.length != 0) {
                writeln("Errors found during identifier gatherer");
                foreach (ref string err; ig.errs) {
                        writeln(err);
                }
        }

        return ig;
}
