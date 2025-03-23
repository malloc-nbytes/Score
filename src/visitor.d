module visitor;

import grammar;

struct Visitor {
        void* context;

        /*
         * Expression Visitors
         */
        void function(Visitor* v, ExprBin e)      visitExprBin;
        void function(Visitor* v, ExprUn e)       visitExprUn;
        void function(Visitor* v, ExprStrLit e)   visitExprStrLit;
        void function(Visitor* v, ExprIntLit e)   visitExprIntLit;
        void function(Visitor* v, ExprIdent e)    visitExprIdent;
        void function(Visitor* v, ExprMut e)      visitExprMut;
        void function(Visitor* v, ExprProcCall e) visitExprProcCall;

        /*
         * Statement Visitors
         */
        void function(Visitor* v, StmtLet s)    visitStmtLet;
        void function(Visitor* v, StmtExpr s)   visitStmtExpr;
        void function(Visitor* v, StmtProc s)   visitStmtProc;
        void function(Visitor* v, StmtBlock s)  visitStmtBlock;
        void function(Visitor* v, StmtReturn s) visitStmtReturn;
        void function(Visitor* v, StmtExtern s) visitStmtExtern;
        void function(Visitor* v, StmtIf s)     visitStmtIf;
        void function(Visitor* v, StmtWhile s)  visitStmtWhile;
}

/*
 * Expression acceptors
 */

void acceptExprBin(Expr e, Visitor* v) {
        if (v.visitExprBin) {
                v.visitExprBin(v, cast(ExprBin)e);
        }
}

void acceptExprUn(Expr e, Visitor* v) {
        if (v.visitExprUn) {
                v.visitExprUn(v, cast(ExprUn)e);
        }
}

void acceptExprStrLit(Expr e, Visitor* v) {
        if (v.visitExprIntLit) {
                v.visitExprIntLit(v, cast(ExprIntLit)e);
        }
}

void acceptExprIntLit(Expr e, Visitor* v) {
        if (v.visitExprStrLit) {
                v.visitExprStrLit(v, cast(ExprStrLit)e);
        }
}

void acceptExprIdent(Expr e, Visitor* v) {
        if (v.visitExprIdent) {
                v.visitExprIdent(v, cast(ExprIdent)e);
        }
}

void acceptExprMut(Expr e, Visitor* v) {
        if (v.visitExprMut) {
                v.visitExprMut(v, cast(ExprMut)e);
        }
}

void acceptExprProcCall(Expr e, Visitor* v) {
        if (v.visitExprProcCall) {
                v.visitExprProcCall(v, cast(ExprProcCall)e);
        }
}

/*
 * Statement acceptors
 */

void acceptStmtLet(Stmt s, Visitor* v) {
        if (v.visitStmtLet) {
                v.visitStmtLet(v, cast(StmtLet)s);
        }
}

void acceptStmtExpr(Stmt s, Visitor* v) {
        if (v.visitStmtExpr) {
                v.visitStmtExpr(v, cast(StmtExpr)s);
        }
}

void acceptStmtProc(Stmt s, Visitor* v) {
        if (v.visitStmtProc) {
                v.visitStmtProc(v, cast(StmtProc)s);
        }
}

void acceptStmtBlock(Stmt s, Visitor* v) {
        if (v.visitStmtBlock) {
                v.visitStmtBlock(v, cast(StmtBlock)s);
        }
}

void acceptStmtReturn(Stmt s, Visitor* v) {
        if (v.visitStmtReturn) {
                v.visitStmtReturn(v, cast(StmtReturn)s);
        }
}

void acceptStmtExtern(Stmt s, Visitor* v) {
        if (v.visitStmtExtern) {
                v.visitStmtExtern(v, cast(StmtExtern)s);
        }
}

void acceptStmtIf(Stmt s, Visitor* v) {
        if (v.visitStmtIf) {
                v.visitStmtIf(v, cast(StmtIf)s);
        }
}

void acceptStmtWhile(Stmt s, Visitor* v) {
        if (v.visitStmtWhile) {
                v.visitStmtWhile(v, cast(StmtWhile)s);
        }
}

/*
 * Expression visitors
 */

void visitExprBin(Visitor* v, ExprBin e) {
        e.l.accept(e.l, v);
        e.r.accept(e.r, v);
}

void visitExprUn(Visitor* v, ExprUn e) {
        e.e.accept(e.e, v);
}

void visitExprStrLit(Visitor* v, ExprStrLit e) {
        return;
}

void visitExprIntLit(Visitor* v, ExprIntLit e) {
        return;
}

void visitExprIdent(Visitor* v, ExprIdent e) {
        return;
}

void visitExprMut(Visitor* v, ExprMut e) {
        e.l.accept(e.l, v);
        e.r.accept(e.r, v);
}

void visitExprProcCall(Visitor* v, ExprProcCall e) {
        e.l.accept(e.l, v);
        for (size_t i = 0; i < e.exprs.length; ++i) {
                e.exprs[i].accept(e.exprs[i], v);
        }
}

/*
 * Statement visitors
 */

void visitStmtLet(Visitor* v, StmtLet s) {
        assert(0);
}

void visitStmtExpr(Visitor* v, StmtExpr s) {
        assert(0);
}

void visitStmtProc(Visitor* v, StmtProc s) {
        assert(0);
}

void visitStmtBlock(Visitor* v, StmtBlock s) {
        assert(0);
}

void visitStmtReturn(Visitor* v, StmtReturn s) {
        assert(0);
}

void visitStmtExtern(Visitor* v, StmtExtern s) {
        assert(0);
}

void visitStmtIf(Visitor* v, StmtIf s) {
        s.e.accept(s.e, v);
        s.then.accept(s.then, v);
        if (s.else_) {
                s.else_.accept(s.else_, v);
        }
}

void visitStmtWhile(Visitor* v, StmtWhile s) {
        s.e.accept(s.e, v);
        s.s.accept(s.s, v);
}
