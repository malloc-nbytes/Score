module semanticSymbols;

import std.format;

import visitor;
import runtimeTypes;
import grammar;

struct Sym {
        string name;
        RuntimeType* type;
        bool fun;
}

struct SymScope {
        Sym[string] syms;
}

class SymTbl {
        SymScope[] scopes;
        void enterScope() {
                this.scopes ~= SymScope();
        }
        void exitScope() {
                assert(this.scopes.length > 0);
                this.scopes.length--;
        }
        bool addSym(string name, RuntimeType* type, bool fun) {
                if (this.scopes.length == 0) {
                        this.enterScope();
                }
                auto currentScope = &this.scopes[$ - 1];
                if (name in currentScope.syms) {
                        // Symbol already exists
                        return false;
                }
                currentScope.syms[name] = Sym(name, type, fun);
                return true;
        }
        const const(Sym)* symLookup(const ref string name) {
                foreach (const ref SymScope it; this.scopes) {
                        if (name in it.syms) {
                                return &(it.syms[name]);
                        }
                }
                return null;
        }
}

class SymTblChecker {
        SymTbl tbl;
        string[] errs;

        void reportErr(string msg) {
                errs ~= msg;
        }
}

/*
 * Check Expressions
 */

void symCheckVisitExprBin(Visitor* v, ExprBin s) {
        assert(0);
}

void symCheckVisitExprUn(Visitor* v, ExprUn s) {
        assert(0);
}

void symCheckVisitExprStrLit(Visitor* v, ExprStrLit s) {
        assert(0);
}

void symCheckVisitExprIntLit(Visitor* v, ExprIntLit s) {
        assert(0);
}

void symCheckVisitExprIdent(Visitor* v, ExprIdent s) {
        assert(0);
}

void symCheckVisitExprMut(Visitor* v, ExprMut s) {
        assert(0);
}

void symCheckVisitExprProcCall(Visitor* v, ExprProcCall s) {
        assert(0);
}

/*
 * Check Statements
 */

void symCheckVisitStmtLet(Visitor* v, StmtLet s) {
        SymTblChecker* checker = cast(SymTblChecker*)v.context;
        string name = cast(string)s.id.lx;
        if (!checker.tbl.addSym(name, s.t, false)) {
                checker.reportErr(format("Redefinition of identifier '%s'", name));
        }
        if (s.e) {
                s.e.accept(s.e, v);
        }
}

void symCheckVisitStmtExpr(Visitor* v, StmtExpr s) {
        assert(0);
}

void symCheckVisitStmtProc(Visitor* v, StmtProc s) {
        assert(0);
}

void symCheckVisitStmtBlock(Visitor* v, StmtBlock s) {
        assert(0);
}

void symCheckVisitStmtReturn(Visitor* v, StmtReturn s) {
        assert(0);
}

void symCheckVisitStmtExtern(Visitor* v, StmtExtern s) {
        assert(0);
}

void symCheckVisitStmtIf(Visitor* v, StmtIf s) {
        assert(0);
}

void symCheckVisitStmtWhile(Visitor* v, StmtWhile s) {
        assert(0);
}

Visitor createSymTblChecker(SymTblChecker* c) {
        Visitor v;
        v.context = cast(void*)c;
        v.visitStmtLet = &symCheckVisitStmtLet;
        v.visitStmtExpr = &symCheckVisitStmtExpr;
        v.visitStmtProc = &symCheckVisitStmtProc;
        v.visitStmtBlock = &symCheckVisitStmtBlock;
        v.visitStmtReturn = &symCheckVisitStmtReturn;
        v.visitStmtExtern = &symCheckVisitStmtExtern;
        v.visitStmtIf = &symCheckVisitStmtIf;
        v.visitStmtWhile = &symCheckVisitStmtWhile;
        return v;
}

void semSymCheck(Program* p) {
        SymTblChecker tbl;
        Visitor v = createSymTblChecker(&tbl);
        for (size_t i = 0; i < p.stmts.length; ++i) {
                p.stmts[i].accept(p.stmts[i], &v);
        }
}
