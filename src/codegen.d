module codegen;

import std.stdio;
import std.stdint;
import std.format;
import std.conv;

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
        string[] externs;
        string[] strs;
        string outputName;
        int labelCounter;

        this(string outputName, Register* genStart, Register* paramStart, Register* resStart) {
                this.file = File(outputName~".asm", "w");
                this.stack = [0];
                genRegs = genStart;
                paramRegs = paramStart;
                resRegs = resStart;
                lru = null;
                globls = [];
                externs = [];
                strs = [];
                pushedRegs = [];
                outputName = outputName;
                labelCounter = 0;
        }
        ~this() {
                wrtln("section .data");
                for (size_t i = 0; i < strs.length; ++i) {
                        wrtln(strs[i]);
                }
                wrtln("section .note.GNU-stack");
                if (file.isOpen) { file.close(); }
        }
        string genLabel() {
                return ".L"~(labelCounter++).to!string;
        }
        string genStrLabel() {
                static int l = 0;
                return ".s"~(l++).to!string;
        }
        void addGlobl(string name) {
                wrtln(format("global %s", name));
        }
        void addExtern(string name) {
                wrtln(format("extern %s", name));
        }
        void addStr(string s) {
                strs ~= s;
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
                // TODO: Make the registers in use temporarily
                //       not in use.
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
                // TODO: Make the registers in be back in use.
                Register* it = genRegs;
                for (int i = cast(int)pushedRegs.length-1; i >= 0; --i) {
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
                wrtln("sub rsp, 8");
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
        c.wrtln(format("mov [rbp-%d], %s; storing variable: %s", s.offset, c.lru.name, s.name));
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

        int rspAmnt = 0;
        for (size_t i = 0; i < s.params.length; ++i) {
                rspAmnt += s.params[i].address;
        }

        if (rspAmnt > 0) {
                c.wrtln(format("sub rsp, %d", rspAmnt));
        }

        Register*[] pregs = [];
        for (size_t i = 0; i < s.params.length; ++i) {
                pregs ~= c.allocParamReg(s.params[i].type.size);
                c.wrtln(format("mov [rbp-%d], %s; store param", s.params[i].address, pregs[i].name));
        }

        for (size_t i = 0; i < pregs.length; ++i) {
                c.freeParamReg(pregs[i]);
        }

        s.block.accept(s.block, v);
}

private void visitStmtExtern(Visitor* v, StmtExtern s) {
        Context c = cast(Context)v.context;
        c.addExtern(s.name);
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
        string elseLabel = c.genLabel();
        string endLabel = c.genLabel();
        s.expr.accept(s.expr, v);

        c.wrtln(format("cmp %s, 0", c.lru.name));
        c.wrtln(format("je %s", s.else_ !is null ? elseLabel : endLabel));

        c.freeGenReg(c.lru);

        s.then.accept(s.then, v);

        if (s.else_ !is null) {
                c.wrtln(format("jmp %s", endLabel));
                c.wrtln(format("%s:", elseLabel));
                s.else_.accept(s.else_, v);
        }

        c.wrtln(format("%s:", endLabel));
}

private void visitStmtWhile(Visitor* v, StmtWhile s) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitStmtExpr(Visitor* v, StmtExpr s) {
        Context c = cast(Context)v.context;
        s.expr.accept(s.expr, v);

        // We are not using the result since
        // since this is a statement expression.
        c.freeGenReg(c.lru);
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
        case "-": {
                c.wrtln(format("sub %s, %s", lreg.name, rreg.name));
        } break;
        case "==": {
                c.wrtln(format("cmp %s, %s", lreg.name, rreg.name));
                // c.wrtln("sete al");     // Set al to 1 if equal, 0 otherwise
                // c.wrtln("movzx rax, al"); // Zero-extend to 64-bit
                Register* reg8bit = lreg.getSmallestReg();
                c.wrtln(format("sete %s", reg8bit.name));
                c.wrtln(format("movzx %s, %s", lreg.name, reg8bit.name));
        } break;
        default: assert(0);
        }

        //c.freeReg(lreg);
        c.freeGenReg(rreg);

        // TODO: Maybe we need to free c.lru since
        //       we are reassigning it?
        c.lru = lreg;
}

private void visitExprUn(Visitor* v, ExprUn e) {
        Context c = cast(Context)v.context;
        assert(0);
}

private void visitExprStrLit(Visitor* v, ExprStrLit e) {
        Context c = cast(Context)v.context;
        static size_t strCount = 0;
        string label = ".s" ~ strCount++.to!string;

        string buf;

        for (size_t i = 0; i < e.str.length; ++i) {
                if (e.str[i] == '\n') {
                        buf ~= "\", 10, \"";
                } else {
                        buf ~= e.str[i];
                }
        }

        // Remove surrounding quotes from e.s.lx if present and combine with processed content
        string rawStr = e.str.idup;
        if (rawStr.length >= 2 && rawStr[0] == '"' && rawStr[$-1] == '"') {
                rawStr = rawStr[1..$-1]; // Strip quotes
        }

        // Only wrap in quotes if there's content, and append null terminator
        if (buf.length > 0) {
                c.addStr(label ~ ": db \"" ~ buf ~ "\", 0");
        } else {
                c.addStr(label ~ ": db 0"); // Empty string case
        }

        Register* reg = c.allocGenReg(8);
        c.wrtln(format("lea %s, [%s]", reg.name, label));
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

        // Save hot registers to preserve their state
        c.pushHot64Registers();

        // Step 1: Evaluate all arguments first and store results in temporary registers
        Register*[] argRegs;
        for (size_t i = 0; i < e.args.length; ++i) {
                e.args[i].accept(e.args[i], v); // Evaluate argument
                Register* lr = c.lru;           // Get the register holding the argument result
                argRegs ~= lr;                  // Store the register for later use
                // Note: Don't free lr yet, as we need its value
        }

        // Step 2: Evaluate the function expression (e.g., function name or pointer)
        e.call.accept(e.call, v);
        Register* callReg = c.lru; // Register holding the function address

        // Step 3: Move arguments to parameter registers
        Register*[] paramRegs;
        assert(e.args.length <= 6, "More than 6 arguments not supported");
        for (size_t i = 0; i < e.args.length; ++i) {
                paramRegs ~= c.allocParamReg(e.args[i].type.size);
                c.wrtln(format("mov %s, %s; parameter", paramRegs[i].name, argRegs[i].name));
                c.freeGenReg(argRegs[i]); // Free the temporary register after moving
        }

        // Step 4: Prepare and perform the function call
        c.wrtln("xor rax, rax"); // Clear rax (no floating-point args)
        c.wrtln(format("call %s", callReg.name));

        // Step 5: Restore hot registers and clean up
        c.popHot64Registers();
        c.freeGenReg(callReg); // Free the function address register
        for (size_t i = 0; i < paramRegs.length; ++i) {
                c.freeParamReg(paramRegs[i]);
        }

        // Step 6: Store the return value
        Register* reg = c.allocGenReg(e.type.size);
        c.wrtln(format("mov %s, %s", reg.name, c.getRetReg(e.type.size).name));
        c.lru = reg; // Update lru to the return value register
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
