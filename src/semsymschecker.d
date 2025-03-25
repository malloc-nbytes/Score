module semanticSymbols;

import std.stdio;
import std.format;
import core.stdc.stdlib : exit;

import visitor;
import runtimeTypes;
import grammar;
import token;
import depTracker;
import gatherIdentifiers;

struct Sym {
        string name;
        RuntimeType* type;
        bool fun;
        bool strct;
}

struct SymScope {
        Sym[string] syms;
}

class SymTbl {
        SymScope[] scopes;
        DepTbl dt;
        IdentGatherer[] igs;
        IdentGatherer* currentIg;

        this(DepTbl dt, IdentGatherer[] igs, IdentGatherer* currentIg) {
                this.dt = dt;
                this.igs = igs;
                this.currentIg = currentIg;
        }

        void enterScope() {
                this.scopes ~= SymScope();
        }

        void exitScope() {
                assert(this.scopes.length > 0);
                this.scopes.length--;
        }

        bool addSym(string name, RuntimeType* type, bool fun, bool strct) {
                if (this.scopes.length == 0) {
                        this.enterScope();
                }
                auto currentScope = &this.scopes[$ - 1];

                // Check for redefinition in the current scope
                if (name in currentScope.syms) {
                        return false;
                }

                // For procedures, check dependencies but exclude the current file
                if (fun) {
                        foreach (ref depFile; this.dt.deps) {
                                foreach (ref ig; this.igs) {
                                        if (&ig != this.currentIg) { // Skip the current file's IdentGatherer
                                                foreach (ref proc; ig.procs) {
                                                        string procName = cast(string)proc.id.lx;
                                                        if (procName == name) {
                                                                return false; // Procedure defined in a dependency
                                                        }
                                                }
                                        }
                                }
                        }
                }

                // Add the symbol if no conflicts are found
                currentScope.syms[name] = Sym(name, type, fun, strct);
                return true;
        }

        const const(Sym)* symLookup(const ref string name) {
                // First, check local scopes
                foreach_reverse (const ref SymScope it; this.scopes) {
                        if (name in it.syms) {
                                return &(it.syms[name]);
                        }
                }

                // If not found locally, check dependencies
                foreach (ref depFile; this.dt.deps) {
                        foreach (ref ig; this.igs) {
                                if (&ig != this.currentIg) { // Skip current file for consistency
                                        foreach (ref proc; ig.procs) {
                                                string procName = cast(string)proc.id.lx;
                                                if (procName == name) {
                                                        static Sym foundSym;
                                                        foundSym = Sym(name, cast(RuntimeType*)proc.rtype, true, false);
                                                        return &foundSym;
                                                }
                                        }
                                }
                        }
                }

                return null;
        }
}

class SymTblChecker {
        SymTbl tbl;
        StmtProc[string] procs;
        StmtExtern[string] externs;
        string[] errs;
        bool ok;

        this(DepTbl dt, IdentGatherer[] igs, IdentGatherer* currentIg) {
                this.tbl = new SymTbl(dt, igs, currentIg);
                this.errs = [];
                this.ok = true;
        }

        void reportErr(string msg) {
                errs ~= msg;
                this.ok = false;
        }
}

/*
 * Check Expressions
 */

void symCheckVisitExprBin(Visitor* v, ExprBin s) {
        s.l.accept(s.l, v);
        s.r.accept(s.r, v);
}

void symCheckVisitExprUn(Visitor* v, ExprUn s) {
        assert(0);
}

void symCheckVisitExprStrLit(Visitor* v, ExprStrLit s) {
        return;
}

void symCheckVisitExprIntLit(Visitor* v, ExprIntLit s) {
        return;
}

void symCheckVisitExprIdent(Visitor* v, ExprIdent s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.id.lx;
        if (!checker.tbl.symLookup(name)) {
                checker.reportErr(tokerrToStr(s.id) ~ format("Identifier is not defined '%s'", name));
        }
}

void symCheckVisitExprMut(Visitor* v, ExprMut s) {
        s.l.accept(s.l, v);
        s.r.accept(s.r, v);
}

void symCheckVisitExprProcCall(Visitor* v, ExprProcCall s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        s.l.accept(s.l, v);

        for (size_t i = 0; i < s.exprs.length; ++i) {
                s.exprs[i].accept(s.exprs[i], v);
        }

        assert(s.l.ty == ExprType.Ident && "function calls must be identifiers for now");
        ExprIdent ident = cast(ExprIdent)s.l;
        string name = cast(const string)ident.id.lx;
        if (name in checker.procs) {
                const size_t N = checker.procs[name].pn.length;
                if (s.exprs.length != N && !checker.procs[name].variadic) {
                        checker.reportErr(tokerrToStr(ident.id)
                                          ~ format("Incorrect number of function arguments, expected %d but got %d",
                                                   N, s.exprs.length));
                }
        } else if (name in checker.externs && !checker.externs[name].proto.variadic) {
                const size_t N = checker.externs[name].proto.pn.length;
                if (s.exprs.length != N) {
                        checker.reportErr(tokerrToStr(ident.id)
                                          ~ format("Incorrect number of function arguments, expected %d but got %d",
                                                   N, s.exprs.length));
                }
        }
}

/*
 * Check Statements
 */

void symCheckVisitStmtLet(Visitor* v, StmtLet s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.id.lx;
        if (!checker.tbl.addSym(name, s.t, false, false)) {
                checker.reportErr(tokerrToStr(s.id) ~ format("Redefinition of identifier '%s'", name));
        }
        s.e.accept(s.e, v);
}

void symCheckVisitStmtExpr(Visitor* v, StmtExpr s) {
        s.e.accept(s.e, v);
}

void symCheckVisitStmtProc(Visitor* v, StmtProc s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.id.lx;

        if (!checker.tbl.addSym(name, s.rtype, true, false)) {
                checker.reportErr(tokerrToStr(s.id) ~ format("Redefinition of procedure '%s'", name));
        }

        checker.procs[name] = s;

        checker.tbl.enterScope();

        for (size_t i = 0; i < s.pn.length; ++i) {
                string pname = cast(string)s.pn[i].lx;
                if (!checker.tbl.addSym(pname, s.pt[i], false, false)) {
                        checker.reportErr(tokerrToStr(s.pn[i]) ~ format("Redefinition of parameter '%s' in procedure '%s'",
                                                                        pname, name));
                }
        }

        symCheckVisitStmtBlock(v, s.b);
        checker.tbl.exitScope();
}

void symCheckVisitStmtBlock(Visitor* v, StmtBlock s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        checker.tbl.enterScope();
        for (size_t i = 0; i < s.stmts.length; ++i) {
                s.stmts[i].accept(s.stmts[i], v);
        }
        checker.tbl.exitScope();
}

void symCheckVisitStmtReturn(Visitor* v, StmtReturn s) {
        s.e.accept(s.e, v);
}

void symCheckVisitStmtExtern(Visitor* v, StmtExtern s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.proto.id.lx;
        if (!checker.tbl.addSym(name, s.proto.rtype, true, false)) {
                checker.reportErr(tokerrToStr(s.proto.id) ~ format("Redefinition of procedure '%s'", name));
        }
        checker.externs[name] = s;
}

void symCheckVisitStmtIf(Visitor* v, StmtIf s) {
        s.e.accept(s.e, v);
        s.then.accept(s.then, v);
        if (s.else_) {
                s.else_.accept(s.else_, v);
        }
}

void symCheckVisitStmtWhile(Visitor* v, StmtWhile s) {
        s.e.accept(s.e, v);
        s.s.accept(s.s, v);
}

void symCheckVisitStmtStruct(Visitor* v, StmtStruct s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.id.lx;
        auto sym = checker.tbl.symLookup(name);
        if (sym !is null && sym.strct) {
                checker.reportErr(tokerrToStr(s.id) ~ format("Struct is already defined '%s'", name));
        }
        bool[string] memberIds;
        for (size_t i = 0; i < s.members.length; ++i) {
                const char[]* id = &s.members[i].lx;
                if (*id in memberIds) {
                        checker.reportErr(tokerrToStr(s.members[i])
                                          ~ format("Duplicate field '%s' in struct '%s'",
                                                   *id, name));
                }
                memberIds[*id] = true;
        }
}

Visitor createSymTblChecker(SymTblChecker* c) {
        Visitor v;
        v.context = cast(void*)c;

        v.visitExprBin      = &symCheckVisitExprBin;
        v.visitExprUn       = &symCheckVisitExprUn;
        v.visitExprStrLit   = &symCheckVisitExprStrLit;
        v.visitExprIntLit   = &symCheckVisitExprIntLit;
        v.visitExprIdent    = &symCheckVisitExprIdent;
        v.visitExprMut      = &symCheckVisitExprMut;
        v.visitExprProcCall = &symCheckVisitExprProcCall;

        v.visitStmtLet      = &symCheckVisitStmtLet;
        v.visitStmtExpr     = &symCheckVisitStmtExpr;
        v.visitStmtProc     = &symCheckVisitStmtProc;
        v.visitStmtBlock    = &symCheckVisitStmtBlock;
        v.visitStmtReturn   = &symCheckVisitStmtReturn;
        v.visitStmtExtern   = &symCheckVisitStmtExtern;
        v.visitStmtIf       = &symCheckVisitStmtIf;
        v.visitStmtWhile    = &symCheckVisitStmtWhile;
        v.visitStmtStruct   = &symCheckVisitStmtStruct;
        return v;
}

SymTblChecker semSymCheck(Program* p, IdentGatherer[] igs, DepTbl dt, IdentGatherer* currentIg) {
        SymTblChecker tbl = new SymTblChecker(dt, igs, currentIg);
        Visitor v = createSymTblChecker(&tbl);
        for (size_t i = 0; i < p.stmts.length; ++i) {
                p.stmts[i].accept(p.stmts[i], &v);
        }
        if (tbl.errs.length != 0) {
                writeln("Errors found during semantic symbols analysis");
                foreach (ref string err; tbl.errs) {
                        writeln(err);
                }
        }
        return tbl;
}

