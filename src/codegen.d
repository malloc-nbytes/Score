module codegen;

import std.stdio;
import std.stdint;
import std.format;

import grammar;
import semantic;
import visitor;
import types;
import registers;

class Context {
        File file;
        size_t[] stack;

        Register* genRegs;
        Register* paramRegs;
        Register* resRegs;
        Register*[] pushedRegs;

        Register* lru; // Last register used
        string[] globls;
        string outputName;

        this(string outputName, Register* genStart, Register* paramStart, Register* resStart) {
                this.file = File(outputName~".asm", "w");
                this.stack = [0];
                genRegs = genStart;
                paramRegs = paramStart;
                resRegs = resStart;
                lru = null;
                pushedRegs = [];
                outputName = outputName;
        }
        ~this() {
                if (file.isOpen) { file.close(); }
        }
        void addGlobl(string name) {
                wrtln(format("global %s", name));
        }
        void wrtln(string s) {
                file.writeln(s);
                file.flush();
        }
        void wrt(string s) {
                file.write(s);
                file.flush();
        }
        size_t getStack() {
                return stack[$-1];
        }
        void incrStack(size_t bytes) {
                wrtln(format("sub rsp, %d", bytes));
                stack[$-1] += bytes;
        }
        void pushStack() {
                stack ~= 0;
        }
        void popStack() {
                assert(this.stack.length > 0);
                stack.length--;
        }
        void pushHot64Registers() {
                Register* it = genRegs;
                while (it) {
                        if (it.regInUse()) {
                                wrtln(format("push %s", it.name));
                                pushedRegs ~= it;
                        }
                        it = it.next;
                }
        }
        void popHot64Registers() {
                Register* it = genRegs;
                for (size_t i = 0; i < pushedRegs.length; ++i) {
                        wrtln(format("pop %s", pushedRegs[i].name));
                }
                pushedRegs = [];
        }
        Register* getRetReg(size_t sz) {
                assert(sz == 4 || sz == 8, "Invalid return register size");
                if (sz == 8) { return resRegs; }
                if (sz == 4) { return resRegs.down; }
                assert(0);
        }
        Register* allocGenReg(size_t sz) {
                assert(sz == 4 || sz == 8);
                Register* it = null;
                if (sz == 8) { it = genRegs; }
                else if (sz == 4) { it = genRegs.down; }
                while (it) {
                        if (!it.regInUse()) {
                                it.inUse = true;
                                lru = it;
                                return it;
                        }
                        it = it.next;
                }
                assert(0 && "out of gen registers");
        }
        Register* allocParamReg(size_t sz) {
                assert(sz == 4 || sz == 8);
                Register* it = null;
                if (sz == 8) { it = paramRegs; }
                else if (sz == 4) { it = paramRegs.down; }
                while (it) {
                        if (!it.regInUse()) {
                                it.inUse = true;
                                return it;
                        }
                        it = it.next;
                }
                assert(0 && "out of param registers");
        }
        void freeGenReg(Register* r) {
                r.inUse = false;
        }
        void freeParamReg(Register* r) {
                r.inUse = false;
        }
        void prologue() {
                wrtln("push rbp");
                wrtln("mov rbp, rsp");
        }
        void epilogue() {
                wrtln("mov rsp, rbp");
                wrtln("pop rbp");
        }
}

private void visitStmtExit(Visitor* v, StmtExit s) {
        Context c = cast(Context)v.context;
        if (s.expr) {
                s.expr.accept(s.expr, v);
        }
        c.wrtln(format("mov rax, 60"));
        if (s.expr) {
                Register* reg = c.allocParamReg(s.expr.type.size);
                c.wrtln(format("mov %s, %s", reg.name, c.lru.name));
                c.freeParamReg(reg);
        } else {
                c.wrtln("mov edi, 0");
        }
        c.wrtln("syscall");
}

private void visitStmtStruct(Visitor* v, StmtStruct s) {
        return;
}

private void visitStmtLet(Visitor* v, StmtLet s) {
        Context c = cast(Context)v.context;
        c.incrStack(s.type.size);
        s.expr.accept(s.expr, v);
        c.wrtln(format("mov [rbp-%d], %s", s.offset, c.lru.name));
        c.freeGenReg(c.lru);
}

private void visitStmtProc(Visitor* v, StmtProc s) {
        Context c = cast(Context)v.context;
        if (s.isExport) {
                c.addGlobl(s.name);
        }
        c.wrtln(s.name ~ ":");
        c.prologue();

        // TODO: support for more parameters
        assert(s.params.length <= 6);

        Register*[] pregs = [];

        for (size_t i = 0; i < s.params.length; ++i) {
                pregs ~= c.allocParamReg(s.params[i].type.size);
                c.wrtln(format("mov [rbp-%d], %s; store param", s.params[i].type.size, pregs[i].name));
        }

        for (size_t i = 0; i < pregs.length; ++i) {
                c.freeParamReg(pregs[i]);
        }

        s.block.accept(s.block, v);
}

private void visitStmtExtern(Visitor* v, StmtExtern s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitStmtBlock(Visitor* v, StmtBlock s) {
        Context c = cast(Context)v.context;
        c.pushStack();
        for (size_t i = 0; i < s.stmts.length; ++i) {
                s.stmts[i].accept(s.stmts[i], v);
        }
        c.popStack();
}

private void visitStmtReturn(Visitor* v, StmtReturn s) {
        Context c = cast(Context)v.context;
        s.expr.accept(s.expr, v);
        Register* retReg = c.getRetReg(s.expr.type.size);
        c.wrtln(format("mov %s, %s", retReg.name, c.lru.name));
        c.freeGenReg(c.lru);
        c.epilogue();
        c.wrtln("ret");
}

private void visitStmtIf(Visitor* v, StmtIf s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitStmtWhile(Visitor* v, StmtWhile s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitStmtExpr(Visitor* v, StmtExpr s) {
        Context c = cast(Context)v.context;
        s.expr.accept(s.expr, v);
}

private void visitStmtMod(Visitor* v, StmtMod s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitStmtImport(Visitor* v, StmtImport s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprMember(Visitor* v, ExprMember e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprStructLit(Visitor* v, ExprStructLit e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprBin(Visitor* v, ExprBin e) {
        Context c = cast(Context)v.context;

        e.left.accept(e.left, v);
        Register* lreg = c.lru;
        e.right.accept(e.right, v);
        Register* rreg = c.lru;

        switch (e.op) {
        case "+": {
                c.wrtln(format("add %s, %s", lreg.name, rreg.name));
        } break;
        default: assert(0);
        }

        //c.freeReg(lreg);
        c.freeGenReg(rreg);

        c.lru = lreg;
}

private void visitExprUn(Visitor* v, ExprUn e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprStrLit(Visitor* v, ExprStrLit e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprIntLit(Visitor* v, ExprIntLit e) {
        Context c = cast(Context)v.context;
        Register* reg = c.allocGenReg(e.type.size);
        // TODO: support for 16bit and 8bit types
        string instr = e.type.size == 8 ? "QWORD" : "DWORD";
        c.wrtln(format("mov %s %s, %d", instr, reg.name, e.num));
}

private void visitExprIdent(Visitor* v, ExprIdent e) {
        Context c = cast(Context)v.context;
        if (e.type.kind == TypeKind.Proc) {
                Register* reg = c.allocGenReg(8);
                c.wrtln(format("mov %s, %s", reg.name, e.name));
        } else {
                Register* reg = c.allocGenReg(e.type.size);
                c.wrtln(format("mov %s, [rbp-%d]", reg.name, e.address));
        }
}

private void visitExprMut(Visitor* v, ExprMut e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprProcCall(Visitor* v, ExprProcCall e) {
        Context c = cast(Context)v.context;

        c.pushHot64Registers();
        e.call.accept(e.call, v);
        Register* callReg = c.lru;
        // TODO: allow for more than 6 args
        assert(e.args.length <= 6);
        Register*[] paramRegs = [];
        for (size_t i = 0; i < e.args.length; ++i) {
                e.args[i].accept(e.args[i], v);
                Register* lr = c.lru;
                paramRegs ~= c.allocParamReg(e.args[i].type.size);
                c.wrtln(format("mov %s, %s; parameter", paramRegs[i].name, lr.name));
                c.freeGenReg(c.lru);
        }
        c.wrtln(format("call %s", callReg.name));
        c.popHot64Registers();
        c.freeGenReg(callReg);
        for (size_t i = 0; i < paramRegs.length; ++i) {
                c.freeParamReg(paramRegs[i]);
        }
        Register* reg = c.allocGenReg(e.type.size);
        c.wrtln(format("mov %s, %s", reg.name, c.getRetReg(e.type.size).name));
}

private Visitor createVisitor(Context c) {
        Visitor v;
        v.context = cast(void*)c;

        v.visitStmtStruct = &visitStmtStruct;
        v.visitStmtLet = &visitStmtLet;
        v.visitStmtProc = &visitStmtProc;
        v.visitStmtExtern = &visitStmtExtern;
        v.visitStmtBlock = &visitStmtBlock;
        v.visitStmtReturn = &visitStmtReturn;
        v.visitStmtIf = &visitStmtIf;
        v.visitStmtWhile = &visitStmtWhile;
        v.visitStmtExpr = &visitStmtExpr;
        v.visitStmtMod = &visitStmtMod;
        v.visitStmtImport = &visitStmtImport;
        v.visitStmtExit = &visitStmtExit;

        v.visitExprMember = &visitExprMember;
        v.visitExprStructLit = &visitExprStructLit;
        v.visitExprBin = &visitExprBin;
        v.visitExprUn = &visitExprUn;
        v.visitExprStrLit = &visitExprStrLit;
        v.visitExprIntLit = &visitExprIntLit;
        v.visitExprIdent = &visitExprIdent;
        v.visitExprMut = &visitExprMut;
        v.visitExprProcCall = &visitExprProcCall;

        return v;
}

void gen(Program p, string outputName) {
        Register* r10 = null, rdi = null, rax = null;
        buildRegisters(&r10, &rdi, &rax);
        Context c = new Context(outputName, r10, rdi, rax);
        c.wrtln("section .text");
        Visitor v = createVisitor(c);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
        }
}
