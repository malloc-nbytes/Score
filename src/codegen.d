module codegen;

import std.stdio;
import std.conv;
import std.algorithm;

import grammar;
import runtimeTypes;
import visitor;

class Context {
        string[] rotdata         = [];
        string[] bss             = [];
        string[] data            = [];
        string[] text            = [];
        string[] externs         = [];
        string[] exports         = [];
        const string noexecstack = "section .note.GNU-stack noalloc noexec nowrite progbits";
        const string s           = "    ";
        RuntimeType* current_return_type;

        struct Symbol {
                string name;
                size_t offset; // Offset from rbp (e.g., [rbp - 8])
                RuntimeType* type;
                bool fun;
                bool variadic;
                bool param;
        }

        Symbol[] symbols; // Stack-based, no scope yet
        size_t stackOffset = 0; // Total stack space used

        this() {
                this.rotdata ~= "section .rotdata";
                this.bss     ~= "section .bss";
                this.data    ~= "section .data";
                this.text    ~= "section .text";
        }

        void addComment(string msg) {
                this.text ~= this.s ~ "; " ~ msg;
        }

        void addSymbol(string name,
                       RuntimeType* type,
                       bool isFunction = false,
                       bool variadic = false,
                       bool param = false) {
                size_t size = isFunction ? 0 : getTypeSize(type);  // No stack space for functions
                stackOffset += size;
                symbols ~= Symbol(name, stackOffset, type, isFunction, variadic, param);
        }

        // Find a symbol by name (returns null if not found)
        Symbol* findSymbol(string name) {
                foreach (ref sym; symbols) {
                        if (sym.name == name) {
                                return &sym;
                        }
                }
                return null;
        }

        void extern_(string name) {
                this.externs ~= "extern " ~ name;
        }

        void export_(string name) {
                this.exports ~= "global " ~ name;
        }

        void prologue(string label) {
                this.text ~= label ~ ":";
                this.text ~= this.s ~ "push rbp";
                this.text ~= this.s ~ "mov rbp, rsp";
        }

        void epilogue() {
                this.text ~= this.s ~ "add rsp, " ~ this.stackOffset.to!string;
                this.text ~= this.s ~ "leave";
                this.text ~= this.s ~ "ret";
        }

        char[] write() {
                char[] res = [];
                foreach (const ref string s; this.externs) res ~= s ~ '\n';
                foreach (const ref string s; this.rotdata) res ~= s ~ '\n';
                foreach (const ref string s; this.bss)     res ~= s ~ '\n';
                foreach (const ref string s; this.data)    res ~= s ~ '\n';
                foreach (const ref string s; this.exports) res ~= s ~ '\n';
                foreach (const ref string s; this.text)    res ~= s ~ '\n';
                res ~= '\n' ~ this.noexecstack ~ '\n';
                return res;
        }
}

void compileExprBin(Visitor* v, ExprBin e) {
        assert(0);
}

void compileExprUn(Visitor* v, ExprUn e) {
        assert(0);
}

void compileExprStrLit(Visitor* v, ExprStrLit e) {
        Context* c = cast(Context*)v.context;

        static size_t str_count = 0;
        string label = "str_" ~ str_count++.to!string;

        c.rotdata ~= label ~ ": db \"" ~ e.s.lx.idup ~ "\", 0";
        c.text ~= c.s ~ "lea rax, [" ~ label ~ "]";
}

void compileExprIntLit(Visitor* v, ExprIntLit e) {
        Context* c = cast(Context*)v.context;

        // Assuming 64-bit integers for literals unless specified
        c.text ~= c.s ~ "mov rax, " ~ e.i.lx.idup;
}

void compileExprIdent(Visitor* v, ExprIdent e) {
        Context* c = cast(Context*)v.context;

        string name = e.id.lx.idup;
        Context.Symbol* sym = c.findSymbol(name);

        if (sym is null) {
                c.text ~= c.s ~ "; ERROR: Undefined symbol " ~ name;
                return;
        }

        size_t size = getTypeSize(sym.type);
        string size_spec = size == 8 ? "qword" :
                size == 4 ? "dword" :
                size == 2 ? "word" : "byte";
        string reg = size == 8 ? "rax" :
                size == 4 ? "eax" :
                size == 2 ? "ax" : "al";

        // Load the value from memory into the appropriate register size
        if (sym.param) {
                c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ (2 * sym.offset).to!string ~ "]";
        } else {
                c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ sym.offset.to!string ~ "]";
        }

        // Extend to 64-bit rax if needed
        if (size < 8) {
                if (sym.type.b == RuntimeTypeBase.U8 ||
                    sym.type.b == RuntimeTypeBase.U16 ||
                    sym.type.b == RuntimeTypeBase.U32) {
                        c.text ~= c.s ~ "movzx rax, " ~ reg;  // Zero-extend for unsigned
                } else {
                        c.text ~= c.s ~ "movsx rax, " ~ reg;  // Sign-extend for signed
                }
        }
}

void compileExprMut(Visitor* v, ExprMut e) {
        assert(0);
}

void compileExprProcCall(Visitor* v, ExprProcCall e) {
        Context* c = cast(Context*)v.context;
        c.addComment("Calling procedure");
        if (auto ident = cast(ExprIdent)e.l) {
                string proc_name = ident.id.lx.idup;
                string[] regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                Context.Symbol* sym = c.findSymbol(proc_name);
                bool isVariadic = sym !is null && sym.variadic;

                // Evaluate arguments
                size_t arg_count = min(e.exprs.length, 6);
                for (size_t i = 0; i < arg_count; i++) {
                        e.exprs[i].accept(e.exprs[i], v);
                        c.text ~= c.s ~ "mov " ~ regs[i] ~ ", rax";
                }

                // Align stack to 16 bytes (assuming rsp was aligned at function entry)
                //c.text ~= c.s ~ "sub rsp, 8"; // Adjust for alignment
                if (isVariadic) {
                        c.text ~= c.s ~ "xor al, al"; // No FP args
                }
                c.text ~= c.s ~ "call " ~ proc_name;
                //c.text ~= c.s ~ "add rsp, 8"; // Restore stack
        } else {
                c.text ~= c.s ~ "; ERROR: Procedure call must use identifier";
        }
        c.addComment("End calling procedure");
}

void compileStmtLet(Visitor* v, StmtLet s) {
        Context* c = cast(Context*)v.context;

        size_t varSize = getTypeSize(s.t);
        if (varSize == 0) assert(0, "Cannot allocate variable with void type");

        string varName = s.id.lx.idup;
        c.addSymbol(varName, s.t);

        c.text ~= c.s ~ "sub rsp, " ~ varSize.to!string;

        // c.text ~= c.s ~ "; " ~ varName ~ " at [rbp - " ~ c.stackOffset.to!string ~ "]";
        c.addComment(varName ~ " at [rbp - " ~ c.stackOffset.to!string ~ "]");

        if (s.e !is null) {
                s.e.accept(s.e, v); // Result in rax

                string size_spec = varSize == 8 ? "qword" :
                        varSize == 4 ? "dword" :
                        varSize == 2 ? "word" : "byte";
                string reg = varSize == 8 ? "rax" :
                        varSize == 4 ? "eax" :
                        varSize == 2 ? "ax" : "al";

                if (varSize < 8 && (s.t.b == RuntimeTypeBase.U8 || s.t.b == RuntimeTypeBase.U16 || s.t.b == RuntimeTypeBase.U32)) {
                        c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg;
                }
                c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ (c.stackOffset).to!string ~ "], " ~ reg;
        }
}

void compileStmtExpr(Visitor* v, StmtExpr s) {
        Context* c = cast(Context*)v.context;
        s.e.accept(s.e, v);
        // c.text ~= c.s ~ "; Expression result in rax (discarded)";
        c.addComment("Expression result in rax (discarded)");
}

void compileStmtProc(Visitor* v, StmtProc s) {
        Context* c = cast(Context*)v.context;

        // Save the old stack offset to restore after function generation
        size_t oldStackOffset = c.stackOffset;

        string proc_name = s.id.lx.idup;
        c.current_return_type = s.rtype; // Set the return type
        if (s.isExport) {
                c.export_(proc_name); // Export the function
        }

        // Generate prologue
        c.prologue(proc_name);

        // Parameter handling
        string[] regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        size_t stack_offset = 0;

        foreach (i, param_name; s.pn) {
                RuntimeType* param_type = s.pt[i];
                size_t param_size = getTypeSize(param_type);
                string param_str = param_name.lx.idup;

                // Align each parameter to at least 8 bytes (ABI requirement)
                if (param_size < 8) param_size = 8; // Minimum size for stack slots
                stack_offset += param_size;

                // Add parameter to symbol table with aligned offset
                c.addSymbol(param_str, param_type, false, false, true);

                if (i < 6) { // Parameters passed in registers
                        // Select register based on parameter size
                        string reg = param_size == 8 ? regs[i] :
                                param_size == 4 ? regs[i][0 .. 2] ~ "i" : // edi, esi, etc.
                                param_size == 2 ? regs[i][2 .. $] :       // di, si, etc.
                                regs[i][3 .. $];                         // dil, sil, etc.
                        string size_spec = param_size == 8 ? "qword" :
                                param_size == 4 ? "dword" :
                                param_size == 2 ? "word" : "byte";

                        // Store register value to stack
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ stack_offset.to!string ~ "], " ~ reg;
                        c.addComment(param_str ~ " at [rbp - " ~ stack_offset.to!string ~ "]");
                } else {
                        // Stack parameters are pushed by the caller; just note their location
                        c.text ~= c.s ~ "; " ~ param_str ~ " at [rbp - " ~ stack_offset.to!string ~ "] (stack param)";
                }
        }

        // Align total stack space to 16 bytes if necessary
        if (stack_offset > 0 && stack_offset % 16 != 0) {
                size_t padding = 16 - (stack_offset % 16);
                stack_offset += padding;
                c.text ~= c.s ~ "; Added " ~ padding.to!string ~ " bytes padding for 16-byte alignment";
        }

        // If stack space was allocated, adjust rsp
        if (stack_offset > 0) {
                c.text ~= c.s ~ "sub rsp, " ~ stack_offset.to!string;
        }

        // Compile the function body
        s.b.accept(s.b, v);

        // Generate epilogue
        if (stack_offset > 0) {
                c.text ~= c.s ~ "add rsp, " ~ stack_offset.to!string; // Restore stack
        }
        c.text ~= c.s ~ "leave";
        c.text ~= c.s ~ "ret";

        // Restore outer scope’s stack offset
        c.stackOffset = oldStackOffset;
}

void compileStmtBlock(Visitor* v, StmtBlock s) {
        Context* c = cast(Context*)v.context;

        for (size_t i = 0; i < s.stmts.length; ++i) {
                s.stmts[i].accept(s.stmts[i], v);
        }
}

void compileStmtReturn(Visitor* v, StmtReturn s) {
        Context* c = cast(Context*)v.context;

        s.e.accept(s.e, v); // Result in rax
}

void compileStmtExtern(Visitor* v, StmtExtern s) {
        Context* c = cast(Context*)v.context;

        string proc_name = s.proto.id.lx.idup;

        // Check for redefinition
        if (c.findSymbol(proc_name) !is null) {
                c.text ~= c.s ~ "; ERROR: Redefinition of external symbol " ~ proc_name;
                return;
        }

        string sig = "extern " ~ proc_name ~ "(";
        foreach (i, param; s.proto.pn) {
                sig ~= s.proto.pt[i].b.to!string ~ " " ~ param.lx.idup;
                if (i < s.proto.pn.length - 1) sig ~= ", ";
        }
        sig ~= ") -> " ~ s.proto.rtype.b.to!string;
        // c.text ~= c.s ~ "; " ~ sig;
        c.externs ~= "; " ~ sig;

        // Add extern directive
        c.extern_(proc_name);

        // Add to symbol table as a function
        c.addSymbol(proc_name, s.proto.rtype, true, s.proto.variadic);
}

void compileStmtIf(Visitor* v, StmtIf s) {
        assert(0);
}

void compileStmtWhile(Visitor* v, StmtWhile s) {
        assert(0);
}

Visitor createCodegenContext(Context* c) {
        Visitor v;
        v.context           = cast(void*)c;

        v.visitExprBin      = &compileExprBin;
        v.visitExprUn       = &compileExprUn;
        v.visitExprStrLit   = &compileExprStrLit;
        v.visitExprIntLit   = &compileExprIntLit;
        v.visitExprIdent    = &compileExprIdent;
        v.visitExprMut      = &compileExprMut;
        v.visitExprProcCall = &compileExprProcCall;

        v.visitStmtLet      = &compileStmtLet;
        v.visitStmtExpr     = &compileStmtExpr;
        v.visitStmtProc     = &compileStmtProc;
        v.visitStmtBlock    = &compileStmtBlock;
        v.visitStmtReturn   = &compileStmtReturn;
        v.visitStmtExtern   = &compileStmtExtern;
        v.visitStmtIf       = &compileStmtIf;
        v.visitStmtWhile    = &compileStmtWhile;
        return v;
}

void gen(Program* p) {
        Context c = new Context();
        Visitor v = createCodegenContext(&c);

        for (size_t i = 0; i < p.stmts.length; ++i) {
                p.stmts[i].accept(p.stmts[i], &v);
        }

        writeln(c.write());
}
