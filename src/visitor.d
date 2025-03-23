module visitor;

import grammar;

struct Visitor {
        void* context;

        /*
         * Expression Visitors
         */
        void delegate(Visitor* v, ExprBin* e)      visitExprBin;
        void delegate(Visitor* v, ExprUn* e)       visitExprUn;
        void delegate(Visitor* v, ExprStrLit* e)   visitExprStrLit;
        void delegate(Visitor* v, ExprIntLit* e)   visitExprIntLit;
        void delegate(Visitor* v, ExprIdent* e)    visitExprIdent;
        void delegate(Visitor* v, ExprMut* e)      visitExprMut;
        void delegate(Visitor* v, ExprProcCall* e) visitExprProcCall;

        /*
         * Statement Visitors
         */
        void delegate(Visitor* v, StmtLet* s) visitStmtLet;
        void delegate(Visitor* v, StmtExpr* s) visitStmtExpr;
        void delegate(Visitor* v, StmtProc* s) visitStmtProc;
        void delegate(Visitor* v, StmtBlock* s) visitStmtBlock;
        void delegate(Visitor* v, StmtReturn* s) visitStmtReturn;
        void delegate(Visitor* v, StmtExtern* s) visitStmtExtern;
        void delegate(Visitor* v, StmtIf* s) visitStmtIf;
        void delegate(Visitor* v, StmtWhile* s) visitStmtWhile;
}

/*
 * Expression acceptors
 */

void acceptExprBin(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprUn(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprStrLit(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprIntLit(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprIdent(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprMut(Expr* e, Visitor* v) {
        assert(0);
}

void acceptExprProcCall(Expr* e, Visitor* v) {
        assert(0);
}

/*
 * Statement acceptors
 */

void acceptStmtLet(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtExpr(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtProc(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtBlock(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtReturn(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtExtern(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtIf(Stmt* s, Visitor* v) {
        assert(0);
}

void acceptStmtWhile(Stmt* s, Visitor* v) {
        assert(0);
}


