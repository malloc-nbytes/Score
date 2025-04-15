module codegen;

import std.stdio;
import std.stdint;
import std.format;

import grammar;
import semantic;
import visitor;
import types;

//=====================================================================================REGISTERS
// calling order: rdi, rsi, rdx, rcx, r8, r9.

// https://math.hws.edu/eck/cs220/f22/registers.html

// 64 bit
// rax, rbx, rcx, rdx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15

// 32 bit
// eax, ebx, ecx, edx, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d

// 16 bit
// ax, bx, cx, dx, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w

// 8 bit
// (ah,al), (bh,bl), (ch,cl), (dh,dl), r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
//=====================================================================================END REGISTERS

struct Register {
        Register* next;
        Register* down, up;
        bool inUse;
        string name;

        this(string name, Register* next = null, Register* up = null, Register* down = null) {
                this.next = next;
                this.up = up;
                this.down = down;
                this.inUse = false;
                this.name = name;
        }

        bool regInUse() {
                if (inUse) { return true; }
                Register* it = this.up;
                while (it) {
                        if (it.inUse) { return true; }
                        it = it.up;
                }
                it = this.down;
                while (it) {
                        if (it.inUse) { return true; }
                        it = it.down;
                }
                return false;
        }

        void append(Register** r) {
                Register* it = this.next;
                Register* p = &this;
                while (it) {
                        p = it;
                        it = it.next;
                }
                it = *r;
                p.next = it;
        }

        void appendDown(Register** r) {
                Register* it = this.down;
                Register* p = &this;
                while (it) {
                        p = it;
                        it = it.down;
                }
                it = *r;
                p.down = it;
                if (it) {
                        it.up = p;
                }
        }

        void dump() {
                writeln("DUMPING: ", name);
                write("NEXT: ");
                Register* it = this.next;
                while (it) {
                        write(it.name);
                        it = it.next;
                        if (it) { write(' '); }
                }
                write("\nDOWN: ");
                it = this.down;
                while (it) {
                        write(it.name);
                        it = it.down;
                        if (it) { write(' '); }
                }
                write("\nUP: ");
                it = this.up;
                while (it) {
                        write(it.name);
                        it = it.up;
                        if (it) { write(' '); }
                }
                writeln();
        }
}

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
                        }
                        pushedRegs ~= it;
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
        Register* r10 = new Register("r10");
        Register* r11 = new Register("r11");
        Register* rbx = new Register("rbx");
        Register* r12 = new Register("r12");
        Register* r13 = new Register("r13");
        Register* r14 = new Register("r14");
        Register* r15 = new Register("r15");

        Register* r10d = new Register("r10d");
        Register* r11d = new Register("r11d");
        Register* ebx = new Register("ebx");
        Register* r12d = new Register("r12d");
        Register* r13d = new Register("r13d");
        Register* r14d = new Register("r14d");
        Register* r15d = new Register("r15d");

        Register* r10w = new Register("r10w");
        Register* r11w = new Register("r11w");
        Register* bx = new Register("bx");
        Register* r12w = new Register("r12w");
        Register* r13w = new Register("r13w");
        Register* r14w = new Register("r14w");
        Register* r15w = new Register("r15w");

        Register* r10b = new Register("r10b");
        Register* r11b = new Register("r11b");
        Register* bl = new Register("bl");
        Register* r12b = new Register("r12b");
        Register* r13b = new Register("r13b");
        Register* r14b = new Register("r14b");
        Register* r15b = new Register("r15b");

        r10.append(&r11);
        r10.append(&rbx);
        r10.append(&r12);
        r10.append(&r13);
        r10.append(&r14);
        r10.append(&r15);

        r10d.append(&r11d);
        r10d.append(&ebx);
        r10d.append(&r12d);
        r10d.append(&r13d);
        r10d.append(&r14d);
        r10d.append(&r15d);

        r10w.append(&r11w);
        r10w.append(&bx);
        r10w.append(&r12w);
        r10w.append(&r13w);
        r10w.append(&r14w);
        r10w.append(&r15w);

        r10b.append(&r11b);
        r10b.append(&bl);
        r10b.append(&r12b);
        r10b.append(&r13b);
        r10b.append(&r14b);
        r10b.append(&r15b);

        r10.appendDown(&r10d);
        r10.appendDown(&r10w);
        r10.appendDown(&r10b);

        r11.appendDown(&r11d);
        r11.appendDown(&r11w);
        r11.appendDown(&r11b);

        rbx.appendDown(&ebx);
        rbx.appendDown(&bx);
        rbx.appendDown(&bl);

        r12.appendDown(&r12d);
        r12.appendDown(&r12w);
        r12.appendDown(&r12b);

        r13.appendDown(&r13d);
        r13.appendDown(&r13w);
        r13.appendDown(&r13b);

        r14.appendDown(&r14d);
        r14.appendDown(&r14w);
        r14.appendDown(&r14b);

        r15.appendDown(&r15d);
        r15.appendDown(&r15w);
        r15.appendDown(&r15b);

        Register* rdi = new Register("rdi");
        Register* rsi = new Register("rsi");
        Register* rdx = new Register("rdx");
        Register* rcx = new Register("rcx");
        Register* r8 = new Register("r8");
        Register* r9 = new Register("r9");

        Register* edi = new Register("edi");
        Register* esi = new Register("esi");
        Register* edx = new Register("edx");
        Register* ecx = new Register("ecx");
        Register* r8d = new Register("r8d");
        Register* r9d = new Register("r9d");

        Register* di = new Register("di");
        Register* si = new Register("si");
        Register* dx = new Register("dx");
        Register* cx = new Register("cx");
        Register* r8w = new Register("r8w");
        Register* r9w = new Register("r9w");

        Register* dil = new Register("dil");
        Register* sil = new Register("sil");
        Register* dl = new Register("dl");
        Register* cl = new Register("cl");
        Register* r8b = new Register("r8b");
        Register* r9b = new Register("r9b");

        rdi.append(&rsi);
        rdi.append(&rdx);
        rdi.append(&rcx);
        rdi.append(&r8);
        rdi.append(&r9);

        edi.append(&esi);
        edi.append(&edx);
        edi.append(&ecx);
        edi.append(&r8d);
        edi.append(&r9d);

        di.append(&si);
        di.append(&dx);
        di.append(&cx);
        di.append(&r8w);
        di.append(&r9w);

        dil.append(&sil);
        dil.append(&dl);
        dil.append(&cl);
        dil.append(&r8b);
        dil.append(&r9b);

        rdi.appendDown(&edi);
        rdi.appendDown(&di);
        rdi.appendDown(&dil);

        rsi.appendDown(&esi);
        rsi.appendDown(&si);
        rsi.appendDown(&sil);

        rdx.appendDown(&edx);
        rdx.appendDown(&dx);
        rdx.appendDown(&dl);

        rcx.appendDown(&ecx);
        rcx.appendDown(&cx);
        rcx.appendDown(&cl);

        r8.appendDown(&r8d);
        r8.appendDown(&r8w);
        r8.appendDown(&r8b);

        r9.appendDown(&r9d);
        r9.appendDown(&r9w);
        r9.appendDown(&r9b);

        Register* rax = new Register("rax");
        Register* eax = new Register("eax");
        Register* ax = new Register("ax");
        Register* al = new Register("al");

        rax.appendDown(&eax);
        rax.appendDown(&ax);
        rax.appendDown(&al);

        Context c = new Context(outputName, r10, rdi, rax);
        c.wrtln("section .text");
        Visitor v = createVisitor(c);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
        }
}
