module codegen;

import std.stdio;
import std.conv;
import std.algorithm;
import std.array;
import std.file : write, exists, remove;
import std.process : execute;

import grammar;
import runtimeTypes;
import visitor;
import token;

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
        size_t labelCounter = 0;
        size_t oldStackOffset;

        struct Symbol {
                string name;
                size_t offset; // Offset from rbp (e.g., [rbp - 8])
                RuntimeType* type;
                bool fun;
                bool variadic;
                bool param;
        }

        Symbol[][] symbols; // Stack of scopes
        size_t stackOffset = 0; // Total stack space used in current scope

        this() {
                this.rotdata ~= "section .rotdata";
                this.bss     ~= "section .bss";
                this.data    ~= "section .data";
                this.text    ~= "section .text";
                if (this.symbols.length == 0) {
                        this.symbols = [[]];
                }
                this.oldStackOffset = 0;
        }

        string genLabel(string prefix) {
                return prefix ~ "_" ~ this.labelCounter++.to!string;
        }

        void addComment(string msg) {
                this.text ~= this.s ~ "; " ~ msg;
        }

        void pushScope() {
                this.symbols ~= [[]]; // Add a new scope
                //this.stackOffset = 0; // Reset offset for new scope
                this.oldStackOffset = this.stackOffset;
        }

        void popScope() {
                if (this.symbols.length > 1) { // Preserve global scope
                        this.symbols = this.symbols[0 .. $ - 1];
                        this.stackOffset = this.symbols.length > 0 ? this.symbols[$ - 1].map!(s => s.offset + getTypeSize(s.type)).maxElement(0) : 0;
                        this.stackOffset = this.oldStackOffset;
                }
        }

        void addSymbol(string name, RuntimeType* type, bool isFunction = false, bool variadic = false, bool param = false) {
                size_t size = isFunction ? 0 : getTypeSize(type);
                stackOffset += size;
                this.symbols[$ - 1] ~= Symbol(name, stackOffset, type, isFunction, variadic, param); // Add to current scope
        }

        Symbol* findSymbol(string name) {
                // Search from innermost scope outward
                for (ptrdiff_t i = this.symbols.length - 1; i >= 0; i--) {
                        foreach (ref sym; this.symbols[i]) {
                                if (sym.name == name) {
                                        return &sym;
                                }
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
        Context* c = cast(Context*)v.context;

        // Evaluate left operand (result in rax)
        e.l.accept(e.l, v);
        // Save l operand to the stack to free rax
        c.text ~= c.s ~ "push rax";

        // Evaluate r operand (result in rax)
        e.r.accept(e.r, v);
        // Move r operand to rbx
        c.text ~= c.s ~ "mov rbx, rax";

        // Pop l operand back into rax
        c.text ~= c.s ~ "pop rax";

        // Perform the operation based on the operator
        switch (e.op.ty) {
                // Arithmetic Operations
        case TokenType.Plus:
                c.text ~= c.s ~ "add rax, rbx"; // rax = rax + rbx
                break;
        case TokenType.Minus:
                c.text ~= c.s ~ "sub rax, rbx"; // rax = rax - rbx
                break;
        case TokenType.Asterisk:
                c.text ~= c.s ~ "imul rax, rbx"; // rax = rax * rbx (signed multiply)
                break;
        case TokenType.ForwardSlash:
                c.text ~= c.s ~ "cqo";         // Sign-extend rax into rdx:rax
                c.text ~= c.s ~ "idiv rbx";    // rax = rax / rbx (signed division)
                break;
        case TokenType.Percent:
                c.text ~= c.s ~ "cqo";         // Sign-extend rax into rdx:rax
                c.text ~= c.s ~ "idiv rbx";    // rdx = rax % rbx (remainder)
                c.text ~= c.s ~ "mov rax, rdx"; // Move remainder to rax
                break;

                // Comparison Operations (result is 0 or 1)
        case TokenType.DoubleEquals:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "sete al";     // Set al to 1 if equal, 0 otherwise
                c.text ~= c.s ~ "movzx rax, al"; // Zero-extend to 64-bit
                break;
        case TokenType.BangEquals:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "setne al";    // Set al to 1 if not equal
                c.text ~= c.s ~ "movzx rax, al";
                break;
        case TokenType.Lessthan:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "setl al";     // Set al to 1 if less than
                c.text ~= c.s ~ "movzx rax, al";
                break;
        case TokenType.Greaterthan:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "setg al";     // Set al to 1 if greater than
                c.text ~= c.s ~ "movzx rax, al";
                break;
        case TokenType.LessthanEquals:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "setle al";    // Set al to 1 if less than or equal
                c.text ~= c.s ~ "movzx rax, al";
                break;
        case TokenType.GreaterthanEquals:
                c.text ~= c.s ~ "cmp rax, rbx";
                c.text ~= c.s ~ "setge al";    // Set al to 1 if greater than or equal
                c.text ~= c.s ~ "movzx rax, al";
                break;

                // Logical Operations (short-circuit not implemented here)
        case TokenType.DoubleAmpersand:
                c.text ~= c.s ~ "and rax, rbx"; // Logical AND (non-zero = true)
                c.text ~= c.s ~ "setne al";     // Convert to 0 or 1
                c.text ~= c.s ~ "movzx rax, al";
                break;
        case TokenType.DoublePipe:
                c.text ~= c.s ~ "or rax, rbx";  // Logical OR
                c.text ~= c.s ~ "setne al";     // Convert to 0 or 1
                c.text ~= c.s ~ "movzx rax, al";
                break;

                // Bitwise Operations
        case TokenType.Ampersand:
                c.text ~= c.s ~ "and rax, rbx"; // Bitwise AND
                break;
        case TokenType.Pipe:
                c.text ~= c.s ~ "or rax, rbx";  // Bitwise OR
                break;
        case TokenType.Uptick:
                c.text ~= c.s ~ "xor rax, rbx"; // Bitwise XOR
                break;

        default:
                c.text ~= c.s ~ "; ERROR: Unsupported binary operator " ~ e.op.ty.to!string;
                break;
        }
}

void compileExprUn(Visitor* v, ExprUn e) {
        assert(0);
}

void compileExprStrLit(Visitor* v, ExprStrLit e) {
        Context* c = cast(Context*)v.context;

        static size_t str_count = 0;
        string label = "str_" ~ str_count++.to!string;

        string buf;

        for (size_t i = 0; i < e.s.lx.length; ++i) {
                if (e.s.lx[i] == '\n') {
                        buf ~= "\", 10, \"";
                } else {
                        buf ~= e.s.lx[i];
                }
        }

        // Remove surrounding quotes from e.s.lx if present and combine with processed content
        string rawStr = e.s.lx.idup;
        if (rawStr.length >= 2 && rawStr[0] == '"' && rawStr[$-1] == '"') {
                rawStr = rawStr[1..$-1]; // Strip quotes
        }

        // Only wrap in quotes if there's content, and append null terminator
        if (buf.length > 0) {
                c.rotdata ~= label ~ ": db \"" ~ buf ~ "\", 0";
        } else {
                c.rotdata ~= label ~ ": db 0"; // Empty string case
        }

        c.text ~= c.s ~ "lea rax, [" ~ label ~ "]";
}

void compileExprIntLit(Visitor* v, ExprIntLit e) {
        Context* c = cast(Context*)v.context;

        // TODO: use appropriate register
        c.text ~= c.s ~ "mov eax, " ~ e.i.lx.idup;
}

void compileExprIdent(Visitor* v, ExprIdent e) {
        Context* c = cast(Context*)v.context;

        string name = e.id.lx.idup;
        Context.Symbol* sym = c.findSymbol(name);

        if (sym is null) {
                c.text ~= c.s ~ "; ERROR: Undefined symbol " ~ name;
                return;
        }

        c.addComment("Retrieving identifier: " ~ name);

        size_t size = getTypeSize(sym.type);
        string size_spec = size == 8 ? "qword" :
                size == 4 ? "dword" :
                size == 2 ? "word" : "byte";
        string reg = size == 8 ? "rax" :
                size == 4 ? "eax" :
                size == 2 ? "ax" : "al";

        // Load the value from memory into the appropriate register size
        if (sym.param) {
                c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ (2*sym.offset).to!string ~ "]";
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
        Context* c = cast(Context*)v.context;

        if (auto ident = cast(ExprIdent)e.l) {
                string varName = ident.id.lx.idup;
                Context.Symbol* sym = c.findSymbol(varName);

                if (sym is null) {
                        c.text ~= c.s ~ "; ERROR: Undefined variable " ~ varName;
                        return;
                }

                size_t size = getTypeSize(sym.type);
                string size_spec = size == 8 ? "qword" :
                        size == 4 ? "dword" :
                        size == 2 ? "word" : "byte";
                string reg = size == 8 ? "rax" :
                        size == 4 ? "eax" :
                        size == 2 ? "ax" : "al";
                string offset = sym.param ? (2 * sym.offset).to!string : sym.offset.to!string;

                // Evaluate right-hand side
                e.r.accept(e.r, v);

                switch (e.eqty.ty) {
                case TokenType.Equals:
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;

                case TokenType.PlusEquals:
                        c.text ~= c.s ~ "add " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.MinusEquals:
                        c.text ~= c.s ~ "mov rbx, " ~ reg; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "sub " ~ reg ~ ", rbx";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.AsteriskEquals:
                        c.text ~= c.s ~ "imul " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.ForwardSlashEquals:
                        c.text ~= c.s ~ "mov rbx, rax"; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "cqo";
                        c.text ~= c.s ~ "idiv rbx";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.PercentEquals:
                        c.text ~= c.s ~ "mov rbx, rax"; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "cqo";
                        c.text ~= c.s ~ "idiv rbx";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], rdx";
                        break;
                case TokenType.AmpersandEquals:
                        c.text ~= c.s ~ "and " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.PipeEquals:
                        c.text ~= c.s ~ "or " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.CaretEquals:
                        c.text ~= c.s ~ "xor " ~ reg ~ ", " ~ size_spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;

                default:
                        c.text ~= c.s ~ "; ERROR: Unsupported mutation operator " ~ e.eqty.ty.to!string;
                        break;
                }
        } else {
                c.text ~= c.s ~ "; ERROR: Left side of mutation must be an identifier";
        }
}

// TODO: handle more than 6 function args and clean
//       up the stack after pushing them.
void compileExprProcCall(Visitor* v, ExprProcCall e) {
        Context* c = cast(Context*)v.context;
        c.addComment("Calling procedure");
        if (auto ident = cast(ExprIdent)e.l) {
                string proc_name = ident.id.lx.idup;
                string[] regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                Context.Symbol* sym = c.findSymbol(proc_name);
                bool isVariadic = sym !is null && sym.variadic;

                // Evaluate arguments
                size_t argCount = min(e.exprs.length, 6);
                string[] usedRegs = regs[0..argCount];
                foreach (const ref string r; usedRegs) {
                        c.text ~= c.s ~ "push " ~ r;
                }
                for (size_t i = 0; i < argCount; i++) {
                        e.exprs[i].accept(e.exprs[i], v);
                        c.text ~= c.s ~ "mov " ~ regs[i] ~ ", rax";
                }

                // Align stack to 16 bytes (assuming rsp was aligned at function entry)
                //c.text ~= c.s ~ "sub rsp, 8"; // Adjust for alignment
                if (isVariadic) {
                        c.text ~= c.s ~ "xor al, al"; // No FP args
                }
                c.text ~= c.s ~ "call " ~ proc_name;
                foreach (const ref string r; usedRegs) {
                        c.text ~= c.s ~ "pop " ~ r;
                }
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
        c.addSymbol(varName, s.t); // Adds to current scope (symbols[$ - 1])

        c.text ~= c.s ~ "sub rsp, " ~ varSize.to!string;

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

        string proc_name = s.id.lx.idup;
        c.current_return_type = s.rtype;
        if (s.isExport) {
                c.export_(proc_name);
        }

        c.prologue(proc_name);
        c.pushScope();

        string[] regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        size_t paramOffset = 8;

        foreach (i, param_name; s.pn) {
                RuntimeType* param_type = s.pt[i];
                size_t param_size = getTypeSize(param_type);
                if (param_size < 8) param_size = 8;

                c.addSymbol(param_name.lx.idup, param_type, false, false, true);

                if (i < 6) {
                        string reg = param_size == 8 ? regs[i] :
                                param_size == 4 ? regs[i][0 .. 2] ~ "i" :
                                param_size == 2 ? regs[i][2 .. $] :
                                regs[i][3 .. $];
                        string size_spec = param_size == 8 ? "qword" :
                                param_size == 4 ? "dword" :
                                param_size == 2 ? "word" : "byte";

                        c.text ~= c.s ~ "mov " ~ size_spec ~ " [rbp - " ~ paramOffset.to!string ~ "], " ~ reg;
                        c.addComment(param_name.lx.idup ~ " at [rbp - " ~ paramOffset.to!string ~ "]");
                        paramOffset += 8;
                } else {
                        c.text ~= c.s ~ "; " ~ param_name.lx.idup ~ " at [rbp + " ~ (16 + (i - 6) * 8).to!string ~ "] (stack param)";
                        paramOffset += 8;
                }
        }

        size_t paramSpace = paramOffset - 8;
        size_t totalStackSpace = paramSpace;
        if (totalStackSpace > 0) {
                if ((totalStackSpace + 8) % 16 != 0) {
                        size_t padding = 16 - ((totalStackSpace + 8) % 16);
                        totalStackSpace += padding;
                        c.text ~= c.s ~ "; Added " ~ padding.to!string ~ " bytes padding for 16-byte alignment";
                }
                c.text ~= c.s ~ "sub rsp, " ~ totalStackSpace.to!string;
        }

        // c.stackOffset = 0; // Reset stackOffset for local variables
        c.stackOffset = paramSpace;
        s.b.accept(s.b, v);

        if (totalStackSpace > 0 || c.stackOffset > 0) {
                c.text ~= c.s ~ "add rsp, " ~ (totalStackSpace + c.stackOffset).to!string;
        }
        c.text ~= c.s ~ "leave";
        c.text ~= c.s ~ "ret";

        c.popScope();
}

void compileStmtBlock(Visitor* v, StmtBlock s) {
        Context* c = cast(Context*)v.context;

        c.pushScope();

        for (size_t i = 0; i < s.stmts.length; ++i) {
                s.stmts[i].accept(s.stmts[i], v);
        }

        c.popScope();
}

void compileStmtReturn(Visitor* v, StmtReturn s) {
        Context* c = cast(Context*)v.context;

        s.e.accept(s.e, v); // Result in rax
        c.epilogue();
}

void compileStmtExtern(Visitor* v, StmtExtern s) {
        Context* c = cast(Context*)v.context;

        string proc_name = s.proto.id.lx.idup;

        // Check for redefinition across all scopes
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
        c.externs ~= "; " ~ sig;

        // Add extern directive
        c.extern_(proc_name);

        // Add to global scope (symbols[0]) as a function
        if (c.symbols[0].length == 0 && c.symbols.length == 1) {
                c.symbols[0] ~= Context.Symbol(proc_name, 0, s.proto.rtype, true, s.proto.variadic); // Add directly to global scope
        } else {
                c.symbols[0] ~= Context.Symbol(proc_name, 0, s.proto.rtype, true, s.proto.variadic); // Append to global scope
        }
}

void compileStmtIf(Visitor* v, StmtIf s) {
        Context* c = cast(Context*)v.context;

        // Generate unique labels
        string elseLabel = c.genLabel("else");
        string endLabel = c.genLabel("endif");

        // Evaluate the condition expression (result in rax)
        s.e.accept(s.e, v);

        // Compare rax with 0 (false if zero, true if non-zero)
        c.text ~= c.s ~ "cmp rax, 0";
        c.text ~= c.s ~ "je " ~ (s.else_ !is null ? elseLabel : endLabel); // Jump if equal (false)

        s.then.accept(s.then, v);

        // If there's an else block, jump to end after then
        if (s.else_ !is null) {
                c.text ~= c.s ~ "jmp " ~ endLabel;
                c.text ~= elseLabel ~ ":";
                s.else_.accept(s.else_, v);
        }

        // End of if statement
        c.text ~= endLabel ~ ":";
}

void compileStmtWhile(Visitor* v, StmtWhile s) {
        Context* c = cast(Context*)v.context;
        string loopBeginLabel = c.genLabel("while");
        string loopEndLabel = c.genLabel("endwhile");

        c.text ~= loopBeginLabel ~ ":";
        s.e.accept(s.e, v);

        // Compare rax with 0 (false if zero, true if non-zero)
        c.text ~= c.s ~ "cmp rax, 0";
        c.text ~= c.s ~ "je " ~ loopEndLabel; // Jump if equal (false)

        s.s.accept(s.s, v);
        c.text ~= c.s ~ "jmp " ~ loopBeginLabel;

        // End of if statement
        c.text ~= loopEndLabel ~ ":";
}

void compileStmtStruct(Visitor* v, StmtStruct s) {
        Context* c = cast(Context*)v.context;

        // Extract struct name
        string structName = s.id.lx.idup;

        // Check for redefinition
        if (c.findSymbol(structName) !is null) {
                c.text ~= c.s ~ "; ERROR: Redefinition of symbol " ~ structName;
                return;
        }

        // Calculate member offsets and total size
        size_t totalSize = 0;
        size_t[] memberOffsets;
        foreach (memberType; s.memberTypes) {
                size_t memberSize = getTypeSize(memberType);
                // Align to next 8-byte boundary if needed (for x86-64 compatibility)
                if (totalSize % 8 != 0) {
                        size_t padding = 8 - (totalSize % 8);
                        totalSize += padding;
                }
                memberOffsets ~= totalSize;
                totalSize += memberSize;
        }

        // Create a RuntimeType for the struct
        RuntimeType* structType = new RuntimeType();
        structType.b = RuntimeTypeBase.Struct;
        structType.size = totalSize;
        structType.memberNames = s.members.map!(m => m.lx.idup).array; // Convert Token*[] to string[]
        structType.memberTypes = s.memberTypes.dup;                    // Copy member types
        structType.memberOffsets = memberOffsets.dup;                  // Copy offsets
        structType.nptr = null;                                        // Not a pointer yet

        // Add the struct to the global scope
        c.addSymbol(structName, structType, false, false, false); // Not a function, not variadic, not a param

        // Add a comment for debugging
        c.addComment("Defined struct " ~ structName ~ " with size " ~ totalSize.to!string ~ " bytes");
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
        v.visitStmtStruct   = &compileStmtStruct;
        return v;
}

void writeX86_64AsmFile(Context c) {
        string outputName = "output";
        string asmFile = outputName~".asm";
        string objFile = outputName~".o";
        string[] nasmArgs = ["nasm", "-f", "elf64", asmFile, "-o", objFile, "-g", "-F dwarf"];
        string[] linkArgs = ["gcc", "-no-pie", objFile, "-o", outputName, "-g"];
        writeln("Generated assembly:");
        writeln(c.write());
        write(asmFile, c.write());
        auto nasmResult = execute(nasmArgs);
        if (nasmResult.status != 0) {
                writeln("Assembly failed:");
                writeln(nasmResult.output);
        } else {
                auto linkResult = execute(linkArgs);
                if (linkResult.status != 0) {
                        writeln("Linking failed:");
                        writeln(linkResult.output);
                } else {
                        writeln("Successfully compiled and linked to ", outputName);
                }
        }
        // if (exists(asmFile)) remove(asmFile);
        if (exists(objFile)) remove(objFile);
}

void gen(Program* p) {
        Context c = new Context();
        Visitor v = createCodegenContext(&c);

        for (size_t i = 0; i < p.stmts.length; ++i) {
                p.stmts[i].accept(p.stmts[i], &v);
        }

        writeX86_64AsmFile(c);
}
