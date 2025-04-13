module codegen;

import std.stdio;
import std.stdint;
import std.format;

import grammar;
import semantic;
import visitor;

//=====================================================================================REGISTERS
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

enum Reg {
        // 64 bit
        RAX = "rax", RBX = "rbx", RCX = "rcx",
        RDX = "rdx", RSI = "rsi", RDI = "rdi",
        R8 = "r8",   R9 = "r9",   R10 = "r10",
        R11 = "r11", R12 = "r12", R13 = "r13",
        R14 = "r14", R15 = "r15",

        // 32 bit
        EAX = "eax",   EBX = "ebx",   ECX = "ecx",
        EDX = "edx",   ESI = "esi",   EDI = "edi",
        R8D = "r8d",   R9D = "r9d",   R10D = "r10d",
        R11D = "r11d", R12D = "r12d", R13D = "r13d",
        R14D = "r14d", R15D = "r15d",

        // 16 bit
        AX = "ax",     BX = "bx",     CX = "cx",
        DX = "dx",     SI = "si",     DI = "di",
        R8W = "r8w",   R9W = "r9w",   R10W = "r10w",
        R11W = "r11w", R12W = "r12w", R13W = "r13w",
        R14W = "r14w", R15W = "r15w",

        // 8 bit
        AH = "ah",     AL = "al",     BH = "bh",
        BL = "bl",     CH = "ch",     CL = "cl",
        DH = "dh",     DL = "dl",     R8B = "r8b",
        R9B = "r9b",   R10B = "r10b", R11B = "r11b",
        R12B = "r12b", R13B = "r13b", R14B = "r14b",
        R15B = "r15b",
}

static const(Reg[]) gGenRegs64 = [Reg.RBX, Reg.R10, Reg.R11, Reg.R12, Reg.R13, Reg.R14, Reg.R15];
static const(Reg[]) gGenRegs32 = [Reg.EBX, Reg.R10D, Reg.R11D, Reg.R12D, Reg.R13D, Reg.R14D, Reg.R15D];

static const(Reg[]) gParamRegs64 = [Reg.RDI, Reg.RSI, Reg.RDX, Reg.RCX, Reg.R8, Reg.R9];
static const(Reg[]) gParamRegs32 = [Reg.EDI, Reg.ESI, Reg.EDX, Reg.ECX, Reg.R8D, Reg.R9D];
static const(Reg[]) gGenRegs = gGenRegs32 ~ gGenRegs64;
static const(Reg[]) gParamRegs = gParamRegs32 ~ gParamRegs64;

class Context {
        File file;
        size_t[] stack;
        int genRegs;
        int paramRegs;
        int lastReg;
        string[] globls;
        this() {
                this.file = File("output.asm", "w");
                this.stack = [0];
                genRegs = 0x000000;
                paramRegs = 0x000000;
                lastReg = 0;
                globls = [];
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
                wrtln(format("add rsp, %d", bytes));
                stack[$-1] += bytes;
        }
        void pushStack() {
                stack ~= 0;
        }
        void popStack() {
                assert(this.stack.length > 0);
                stack.length--;
        }
        string getRetReg(size_t sz) {
                // TODO: support 16bit and 8bit registers
                assert(sz == 8 || sz == 4);
                return sz == 8 ? "rax" : "eax";
        }
        int allocReg(size_t sz) {
                // TODO: support 16bit and 8bit registers
                assert(sz == 4 || sz == 8);
                const (Reg[]) regs = (sz == 4) ? gGenRegs32 : gGenRegs64;
                for (int i = 0; i < cast(int)regs.length; ++i) {
                        if (!(genRegs & (1 << i))) {
                                genRegs |= (1 << i);
                                if (sz == 8) {
                                        i += gGenRegs32.length;
                                }
                                lastReg = i;
                                return i;
                        }
                }
                assert(0 && "out of registers");
        }
        int allocParamReg(size_t sz) {
                // TODO: support 16bit and 8bit registers
                assert(sz == 4 || sz == 8);
                const (Reg[]) regs = (sz == 4) ? gParamRegs32 : gParamRegs64;
                for (int i = 0; i < cast(int)regs.length; ++i) {
                        if (!(paramRegs & (1 << i))) {
                                paramRegs |= (1 << i);
                                if (sz == 8) {
                                        i += gParamRegs32.length;
                                }
                                // lastReg = i;
                                return i;
                        }
                }
                assert(0 && "out of registers");
        }
        void freeReg(int r) {
                genRegs &= ~(1 << r);
        }
        string regToStr(int r) {
                return gGenRegs[r];
        }
        string paramRegToStr(int r) {
                return gParamRegs[r];
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
                int reg = c.allocParamReg(s.expr.type.size);
                c.wrtln(format("mov %s, %s", c.paramRegToStr(reg), c.regToStr(c.lastReg)));
                c.freeReg(reg);
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
        c.wrtln(format("mov [rbp-%d], %s", s.offset, c.regToStr(c.lastReg)));
        c.freeReg(c.lastReg);
}

private void visitStmtProc(Visitor* v, StmtProc s) {
        Context c = cast(Context)v.context;
        if (s.isExport) {
                c.addGlobl(s.name);
        }
        c.wrtln(s.name ~ ":");
        c.prologue();
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
        string retReg = c.getRetReg(s.expr.type.size);
        c.wrtln(format("mov %s, %s", retReg, c.regToStr(c.lastReg)));
        c.freeReg(c.lastReg);
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
        assert(0);
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
        assert(0);
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
        int reg = c.allocReg(e.type.size);
        // TODO: support for 16bit and 8bit types
        string instr = e.type.size == 8 ? "QWORD" : "DWORD";
        c.wrtln(format("mov %s %s, %d", instr, c.regToStr(reg), e.num));
}

private void visitExprIdent(Visitor* v, ExprIdent e) {
        Context c = cast(Context)v.context;
        int reg = c.allocReg(e.type.size);
        c.wrtln(format("mov %s, [rbp-%d]", c.regToStr(reg), e.address));
}

private void visitExprMut(Visitor* v, ExprMut e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprProcCall(Visitor* v, ExprProcCall e) {
        Context c = cast(Context)v.context;
        assert(0);
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

void gen(Program p, SemanticAnalyzer ana) {
        Context c = new Context;
        c.wrtln("section .text");
        Visitor v = createVisitor(c);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
        }
}
