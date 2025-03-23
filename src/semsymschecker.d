module semanticSymbols;

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

void symCheckVisitStmtLet(Visitor* v, StmtLet s) {
        assert(0);
}

void symCheckVisitStmtExpr(Visitor*v, StmtExpr s) {
        assert(0);
}

void symCheckVisitStmtProc(Visitor*v, StmtProc s) {
        assert(0);
}

void symCheckVisitStmtBlock(Visitor*v, StmtBlock s) {
        assert(0);
}

void symCheckVisitStmtReturn(Visitor*v, StmtReturn s) {
        assert(0);
}

void symCheckVisitStmtExtern(Visitor*v, StmtExtern s) {
        assert(0);
}

void symCheckVisitStmtIf(Visitor*v, StmtIf s) {
        assert(0);
}

void symCheckVisitStmtWhile(Visitor*v, StmtWhile s) {
        assert(0);
}

Visitor createSymTblChecker(SymTblChecker c) {
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
