module visitor;

import grammar;

struct Visitor {
        void* context;

        /*
         * Expression Visitors
         */
        void function(Visitor* v, ExprBin e)        visitExprBin;
        void function(Visitor* v, ExprUn e)         visitExprUn;
        void function(Visitor* v, ExprStrLit e)     visitExprStrLit;
        void function(Visitor* v, ExprIntLit e)     visitExprIntLit;
        void function(Visitor* v, ExprIdent e)      visitExprIdent;
        void function(Visitor* v, ExprMut e)        visitExprMut;
        void function(Visitor* v, ExprProcCall e)   visitExprProcCall;
        void function(Visitor* v, ExprStructLit e)  visitExprStructLit;
        void function(Visitor* v, ExprMember e)     visitExprMember;

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
        void function(Visitor* v, StmtStruct s) visitStmtStruct;
        void function(Visitor* v, StmtMod s)    visitStmtMod;
        void function(Visitor* v, StmtImport s) visitStmtImport;
}

/*
 * Expression acceptors
 */

void acceptExprMember(Expr e, Visitor* v) {
        if (v.visitExprMember) {
                v.visitExprMember(v, cast(ExprMember)e);
        }
}

void acceptExprStructLit(Expr e, Visitor* v) {
        if (v.visitExprStructLit) {
                v.visitExprStructLit(v, cast(ExprStructLit)e);
        }
}

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
        if (v.visitExprStrLit) {
                v.visitExprStrLit(v, cast(ExprStrLit)e);
        }
}

void acceptExprIntLit(Expr e, Visitor* v) {
        if (v.visitExprIntLit) {
                v.visitExprIntLit(v, cast(ExprIntLit)e);
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

void acceptStmtStruct(Stmt s, Visitor* v) {
        if (v.visitStmtStruct) {
                v.visitStmtStruct(v, cast(StmtStruct)s);
        }
}

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

void acceptStmtMod(Stmt s, Visitor* v) {
        if (v.visitStmtMod) {
                v.visitStmtMod(v, cast(StmtMod)s);
        }
}

void acceptStmtImport(Stmt s, Visitor* v) {
        if (v.visitStmtImport) {
                v.visitStmtImport(v, cast(StmtImport)s);
        }
}

/*
 * Expression visitors
 */

void visitExprMember(Visitor* v, ExprMember e) {
        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
}

void visitExprStructLit(Visitor* v, ExprStructLit e) {
        for (size_t i = 0; i < e.fields.length; ++i) {
                e.fields[i].expr.accept(e.fields[i].expr, v);
        }
}

void visitExprBin(Visitor* v, ExprBin e) {
        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
}

void visitExprUn(Visitor* v, ExprUn e) {
        e.expr.accept(e.expr, v);
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
        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
}

void visitExprProcCall(Visitor* v, ExprProcCall e) {
        e.call.accept(e.call, v);
        for (size_t i = 0; i < e.args.length; ++i) {
                e.args[i].accept(e.args[i], v);
        }
}

/*
 * Statement visitors
 */

void visitStmtLet(Visitor* v, StmtLet s) {
        s.expr.accept(s.expr, v);
}

void visitStmtExpr(Visitor* v, StmtExpr s) {
        s.expr.accept(s.expr, v);
}

void visitStmtProc(Visitor* v, StmtProc s) {
        visitStmtBlock(v, s.block);
}

void visitStmtBlock(Visitor* v, StmtBlock s) {
        for (size_t i = 0; i < s.stmts.length; ++i) {
                s.stmts[i].accept(s.stmts[i], v);
        }
}

void visitStmtReturn(Visitor* v, StmtReturn s) {
        s.expr.accept(s.expr, v);
}

void visitStmtExtern(Visitor* v, StmtExtern s) {
        return;
}

void visitStmtIf(Visitor* v, StmtIf s) {
        s.expr.accept(s.expr, v);
        s.then.accept(s.then, v);
        if (s.else_) {
                s.else_.accept(s.else_, v);
        }
}

void visitStmtWhile(Visitor* v, StmtWhile s) {
        s.expr.accept(s.expr, v);
        s.stmt.accept(s.stmt, v);
}

void visitStmtMod(Visitor* v, StmtMod s) {
        return;
}

void visitStmtImport(Stmt s, Visitor* v) {
        return;
}
