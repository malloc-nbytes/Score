module codegen;
import std.stdio;
import std.conv;
import std.algorithm;
import std.array;
import std.file : write, exists, remove;
import std.process : execute;
import std.format;

import grammar;
import runtimeTypes;
import visitor;
import token;
import gatherIdentifiers;

class Context {
        IdentGatherer[] igs;
        string[] rotdata         = [];
        string[] bss             = [];
        string[] data            = [];
        string[] text            = [];
        string[] externs         = [];
        string[] exports         = [];
        const string noexecstack = "section .note.GNU-stack noalloc noexec nowrite progbits";
        const string s           = "    ";
        size_t labelCounter = 0;
        size_t oldStackOffset;

        struct Symbol {
                string name;
                size_t offset; // Offset from rbp (e.g., [rbp - 8])
                RuntimeType* type;
                bool fun;
                bool variadic;
                bool param;
                bool struct_;
        }

        Symbol[][] symbols; // Stack of scopes
        size_t stackOffset = 0; // Total stack space used in current scope

        this(IdentGatherer[] igs) {
                this.igs = igs;
                this.rotdata ~= "section .rotdata";
                this.bss     ~= "section .bss";
                this.data    ~= "section .data";
                this.text    ~= "section .text";
                if (this.symbols.length == 0) {
                        this.symbols = [[]];
                }
                this.oldStackOffset = 0;

                for (size_t i = 0; i < this.igs.length; ++i) {
                        for (size_t j = 0; j < this.igs[i].procs.length; ++j) {
                                StmtProc* s = &this.igs[i].procs[j];
                                assert(s);
                                if (s.isExport) {
                                        this.addSymbol(s.id.lx.idup,
                                                       s.rtype, true,
                                                       s.isExport, false);
                                }
                        }
                }
        }

        void makeProcsExterns(ref const char[] modName) {
                for (size_t i = 0; i < this.igs.length; ++i) {
                        if (igs[i].mod.id.lx == modName) {
                                for (size_t j = 0; j < igs[i].procs.length; ++j) {
                                        if (igs[i].procs[j].isExport) {
                                                this.extern_(igs[i].procs[j].id.lx.idup);
                                        }
                                }
                        }
                }
        }

        string genLabel(string prefix) {
                return prefix ~ "_" ~ this.labelCounter++.to!string;
        }

        void addComment(string msg) {
                this.text ~= this.s ~ "; " ~ msg;
        }

        void pushScope() {
                this.symbols ~= [[]];
                this.oldStackOffset = this.stackOffset;  // Save previous offset
        }

        void popScope() {
                if (this.symbols.length > 1) {
                        this.symbols = this.symbols[0 .. $ - 1];
                        this.stackOffset = this.oldStackOffset;  // Restore previous offset
                }
        }

        void addSymbol(string name, RuntimeType* type, bool isFunction = false, bool variadic = false, bool param = false, bool struct_ = false) {
                size_t size = isFunction ? 0 : getTypeSize(type);
                size = struct_ ? 0 : size;
                if (size < 8 && !isFunction && !struct_) size = 8;
                stackOffset += size;
                this.symbols[$ - 1] ~= Symbol(name, stackOffset, type, isFunction, variadic, param, struct_);
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

        void prolog(string label) {
                this.text ~= label ~ ":";
                this.text ~= this.s ~ "push rbp";
                this.text ~= this.s ~ "mov rbp, rsp";
        }

        void epilog() {
                this.text ~= this.s ~ "add rsp, " ~ this.stackOffset.to!string;
                this.text ~= this.s ~ "leave";
                this.text ~= this.s ~ "ret";
        }

        void leave(){
                this.text~=this.s~"leave";
        }

        void ret(){
                this.text~=this.s~"ret";
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


RuntimeType* getExprType(Expr e, Context* c) {
        final switch (e.ty) {
        case ExprType.Ident:
                ExprIdent ident = cast(ExprIdent)e;
                Context.Symbol* sym = c.findSymbol(ident.id.lx.idup);
                if (sym is null) assert(0, "Undefined identifier: " ~ ident.id.lx.idup);
                return sym.type;

        case ExprType.IntLit:
                // Assume i64 for simplicity (could infer from value or context)
                return new RuntimeType(RuntimeTypeBase.I64, null);

        case ExprType.StrLit:
                // String literals could be treated as pointers (char*)
                RuntimeType* charType = new RuntimeType(RuntimeTypeBase.U8, null);
                RuntimeType* ptrType = new RuntimeType(RuntimeTypeBase.Ptr, charType);
                return ptrType;

        case ExprType.ProcCall:
                ExprProcCall procCall = cast(ExprProcCall)e;
                ExprIdent procIdent = cast(ExprIdent)procCall.l;
                Context.Symbol* procSym = c.findSymbol(procIdent.id.lx.idup);
                if (procSym is null) assert(0, "Undefined procedure: " ~ procIdent.id.lx.idup);
                // Assume StmtProc stored return type in symbol (needs adjustment)
                return procSym.type;  // Placeholder; needs rtype from StmtProc

        case ExprType.Bin: return getExprType((cast(ExprBin)e).l, c);
        case ExprType.Un:
                assert(0, "Type inference for binary/unary expressions not implemented");

        case ExprType.Mut:
                assert(0, "Type inference for mutation expressions not implemented");

        case ExprType.StructInst: assert(0);

        case ExprType.Get: assert(0);
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
        c.text ~= c.s ~ "mov rax, " ~ e.i.lx.idup;
}

void compileExprIdent(Visitor* v, ExprIdent e) {
        Context* c = cast(Context*)v.context;
        string name = e.id.lx.idup;
        Context.Symbol* sym = c.findSymbol(name);
        assert(sym, "Symbol not found: " ~ name);

        c.addComment("Retrieving identifier: " ~ name);
        size_t varSize = getTypeSize(sym.type);

        // If it’s a struct, return its address; otherwise, load its value
        if (sym.type.b == RuntimeTypeBase.Struct) {
                c.text ~= c.s ~ "lea rax, [rbp - " ~ sym.offset.to!string ~ "]";
        } else {
                if (varSize == 0) assert(0, "Zero size for " ~ name);
                string reg = ""; string spec = "";
                getGenPReg(varSize, &reg, &spec);
                c.text ~= c.s ~ "mov " ~ spec ~ " " ~ reg ~ ", [rbp - " ~ sym.offset.to!string ~ "]";
                if (varSize < 8 && (sym.type.b == RuntimeTypeBase.U8 || sym.type.b == RuntimeTypeBase.U16 || sym.type.b == RuntimeTypeBase.U32)) {
                        c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg;
                }
        }
}

// 1
// void compileExprIdent(Visitor* v, ExprIdent e) {
//         Context* c = cast(Context*)v.context;

//         string name = e.id.lx.idup;
//         Context.Symbol* sym = c.findSymbol(name);

//         assert(sym);

//         c.addComment("Retrieving identifier: " ~ name);

//         size_t varSize = getTypeSize(sym.type);

//         // NOTE: change here
//         if (varSize > 8) {
//                 return;
//         }

//         string reg = "";
//         string spec = "";
//         getGenPReg(varSize, &reg, &spec);

//         // Load the value from the stack into the register
//         c.text ~= c.s ~ "mov " ~ spec ~ " " ~ reg ~ ", [" ~ "rbp - " ~ sym.offset.to!string ~ "]";

//         // Zero-extend unsigned types if necessary
//         if (varSize < 8
//             && (sym.type.b == RuntimeTypeBase.U8
//                 || sym.type.b == RuntimeTypeBase.U16
//                 || sym.type.b == RuntimeTypeBase.U32)) {
//                 c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg;
//         }
// }

void compileExprGet(Visitor* v, ExprGet e) {
        Context* c = cast(Context*)v.context;
        e.l.accept(e.l, v); // rax = address of p
        c.text ~= c.s ~ "mov rbx, rax";
        c.text ~= c.s ~ "mov rbx, [rbx]"; // Dereference p

        ExprIdent baseIdent = cast(ExprIdent)e.l;
        string baseName = baseIdent.id.lx.idup;
        Context.Symbol* baseSym = c.findSymbol(baseName);
        assert(baseSym);

        RuntimeType* baseType = baseSym.type;
        if (baseType.b != RuntimeTypeBase.Struct) {
                c.text ~= c.s ~ "; ERROR: " ~ baseName ~ " is not a struct";
                return;
        }

        ExprIdent memberIdent = cast(ExprIdent)e.r;
        string memberName = memberIdent.id.lx.idup;
        ptrdiff_t memberIndex = -1;
        for (size_t i = 0; i < baseType.memberNames.length; i++) {
                if (baseType.memberNames[i] == memberName) {
                        memberIndex = i;
                        break;
                }
        }
        if (memberIndex == -1) {
                c.text ~= c.s ~ "; ERROR: Member " ~ memberName ~ " not found in struct " ~ baseType.structName;
                return;
        }

        size_t memberOffset = baseType.memberOffsets[memberIndex];
        RuntimeType* memberType = baseType.memberTypes[memberIndex];
        size_t memberSize = getTypeSize(memberType);

        string reg = ""; string spec = "";
        getGenPReg(memberSize, &reg, &spec);
        c.text ~= c.s ~ "mov " ~ spec ~ " " ~ reg ~ ", [rbx + " ~ memberOffset.to!string ~ "]";
        c.addComment("Loaded " ~ baseName ~ "." ~ memberName ~ " into " ~ reg);
}

// void compileExprGet(Visitor* v, ExprGet e) {
//         Context* c = cast(Context*)v.context;

//         // Step 1: Evaluate the base expression (e.g., 'p')
//         e.l.accept(e.l, v); // Result is in rax (address of the struct or value)
//         c.text ~= c.s ~ "mov rbx, rax"; // Save the base address/value in rbx

//         // Step 2: Check that the left-hand side is an identifier (for now)
//         if (e.l.ty != ExprType.Ident) {
//                 c.text ~= c.s ~ "; ERROR: Left side of '.' must be an identifier (complex expressions not yet supported)";
//                 return;
//         }
//         ExprIdent baseIdent = cast(ExprIdent)e.l;
//         string baseName = baseIdent.id.lx.idup;
//         Context.Symbol* baseSym = c.findSymbol(baseName);
//         if (baseSym is null) {
//                 c.text ~= c.s ~ "; ERROR: Undefined variable " ~ baseName;
//                 return;
//         }

//         // Step 3: Determine the type of the base
//         RuntimeType* baseType = baseSym.type;
//         if (baseType.b == RuntimeTypeBase.Ptr) {
//                 // If base is a pointer, dereference it to get the struct type
//                 baseType = baseType.nptr;
//                 c.text ~= c.s ~ "mov rbx, [rbx]"; // Dereference the pointer
//         }
//         if (baseType.b != RuntimeTypeBase.Struct) {
//                 c.text ~= c.s ~ "; ERROR: " ~ baseName ~ " is not a struct or pointer to struct";
//                 return;
//         }

//         // Step 4: Check that the right-hand side is an identifier
//         if (e.r.ty != ExprType.Ident) {
//                 c.text ~= c.s ~ "; ERROR: Right side of '.' must be an identifier (complex expressions not yet supported)";
//                 return;
//         }
//         ExprIdent memberIdent = cast(ExprIdent)e.r;
//         string memberName = memberIdent.id.lx.idup;

//         // Step 5: Resolve the member name in the struct
//         ptrdiff_t memberIndex = -1;
//         for (size_t i = 0; i < baseType.memberNames.length; i++) {
//                 if (baseType.memberNames[i] == memberName) {
//                         memberIndex = i;
//                         break;
//                 }
//         }
//         if (memberIndex == -1) {
//                 c.text ~= c.s ~ "; ERROR: Member " ~ memberName ~ " not found in struct " ~ baseType.structName;
//                 return;
//         }

//         // Step 6: Get member details
//         size_t memberOffset = baseType.memberOffsets[memberIndex];
//         RuntimeType* memberType = baseType.memberTypes[memberIndex];
//         size_t memberSize = getTypeSize(memberType);

//         // Step 7: Load the member value into rax
//         string reg = "";
//         string spec = "";
//         getGenPReg(memberSize, &reg, &spec);

//         // Load from [rbx + memberOffset], where rbx is the struct’s base address
//         c.text ~= c.s ~ "mov " ~ spec ~ " " ~ reg ~ ", [rbx + " ~ memberOffset.to!string ~ "]";
//         c.addComment("Loaded " ~ baseName ~ "." ~ memberName ~ " into " ~ reg);

//         // Step 8: Handle unsigned types if necessary
//         if (memberSize < 8 && (memberType.b == RuntimeTypeBase.U8 || 
//                                memberType.b == RuntimeTypeBase.U16 || 
//                                memberType.b == RuntimeTypeBase.U32)) {
//                 c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg; // Zero-extend to 64-bit
//         }
// }

// TODO: -= operator fails with i32 because
//       of size operand mismatch.
void compileExprMut(Visitor* v, ExprMut e) {
        Context* c = cast(Context*)v.context;

        if (auto ident = cast(ExprIdent)e.l) {
                string varName = ident.id.lx.idup;
                Context.Symbol* sym = c.findSymbol(varName);

                if (sym is null) {
                        c.text ~= c.s ~ "; ERROR: Undefined variable " ~ varName;
                        return;
                }

                size_t varSize = getTypeSize(sym.type);
                string reg = "";
                string spec = "";
                getGenPReg(varSize, &reg, &spec);
                string offset = sym.offset.to!string;

                // Evaluate right-hand side
                e.r.accept(e.r, v);

                switch (e.eqty.ty) {
                case TokenType.Equals:
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;

                case TokenType.PlusEquals:
                        c.text ~= c.s ~ "add " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.MinusEquals:
                        c.text ~= c.s ~ "mov rbx, " ~ reg; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "sub " ~ reg ~ ", rbx";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.AsteriskEquals:
                        c.text ~= c.s ~ "imul " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.ForwardSlashEquals:
                        c.text ~= c.s ~ "mov rbx, rax"; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "cqo";
                        c.text ~= c.s ~ "idiv rbx";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.PercentEquals:
                        c.text ~= c.s ~ "mov rbx, rax"; // Save right operand
                        c.text ~= c.s ~ "mov " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "cqo";
                        c.text ~= c.s ~ "idiv rbx";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], rdx";
                        break;
                case TokenType.AmpersandEquals:
                        c.text ~= c.s ~ "and " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.PipeEquals:
                        c.text ~= c.s ~ "or " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;
                case TokenType.CaretEquals:
                        c.text ~= c.s ~ "xor " ~ reg ~ ", " ~ spec ~ " [rbp - " ~ offset ~ "]";
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ offset ~ "], " ~ reg;
                        break;

                default:
                        c.text ~= c.s ~ "; ERROR: Unsupported mutation operator " ~ e.eqty.ty.to!string;
                        break;
                }
        } else {
                c.text ~= c.s ~ "; ERROR: Left side of mutation must be an identifier";
        }
}

void compileExprProcCall(Visitor* v, ExprProcCall e) {
        Context* c = cast(Context*)v.context;

        // Get procedure symbol
        if (e.l.ty != ExprType.Ident) {
                assert(0, "Procedure call target must be an identifier");
        }
        ExprIdent procIdent = cast(ExprIdent)e.l;
        string procName = procIdent.id.lx.idup;
        Context.Symbol* procSym = c.findSymbol(procName);
        if (procSym is null) {
                c.extern_(procName);
        } else {
                assert(procSym.fun, "Identifier " ~ procName ~ " is not a function");
        }

        // Calculate stack space for arguments beyond 6 and temporaries
        size_t argCount = e.exprs.length;
        size_t stackArgs = (argCount > 6) ? (argCount - 6) : 0;
        size_t tempSpace = argCount * 8;  // Space for all args temporarily
        size_t totalStackSpace = tempSpace + (stackArgs * 8);
        if (totalStackSpace > 0) {
                size_t totalWithCall = totalStackSpace + 8;
                if (totalWithCall % 16 != 0) {
                        totalStackSpace += 16 - (totalWithCall % 16);
                }
                c.text ~= c.s ~ "sub rsp, " ~ totalStackSpace.to!string;
        }

        // Evaluate arguments and store temporarily on stack
        foreach (i, argExpr; e.exprs) {
                argExpr.accept(argExpr, v);  // Result in rax
                RuntimeType* argType = getExprType(argExpr, c);
                size_t argSize = getTypeSize(argType);
                if (argSize < 8) argSize = 8;
                string spec = (argSize == 8) ? "qword" : "unknown";  // Simplified for now
                size_t tempOffset = i * 8;
                c.text ~= c.s ~ "mov " ~ spec ~ " [rsp + " ~ tempOffset.to!string ~ "], rax";
        }

        // Move arguments to registers or stack
        foreach (i; 0 .. argCount) {
                RuntimeType* argType = getExprType(e.exprs[i], c);
                size_t argSize = getTypeSize(argType);
                if (argSize < 8) argSize = 8;
                string reg = "";
                string spec = "";
                size_t tempOffset = i * 8;
                if (i < 6) {
                        getReg(argSize, i, &reg, &spec);
                        c.text ~= c.s ~ "mov " ~ spec ~ " " ~ reg ~ ", [rsp + " ~ tempOffset.to!string ~ "]";
                } else {
                        size_t stackOffset = (argCount - i - 1) * 8 + tempSpace;
                        c.text ~= c.s ~ "mov " ~ spec ~ " [rsp + " ~ stackOffset.to!string ~ "], [rsp + " ~ tempOffset.to!string ~ "]";
                }
        }

        // Handle variadic functions
        if (procSym && procSym.variadic) {
                c.text ~= c.s ~ "mov rax, 0";
        }

        // Emit the call
        c.text ~= c.s ~ "call " ~ procName;

        // Clean up stack
        if (totalStackSpace > 0) {
                c.text ~= c.s ~ "add rsp, " ~ totalStackSpace.to!string;
        }
}

void compileStmtLet(Visitor* v, StmtLet s) {
        Context* c = cast(Context*)v.context;
        size_t varSize = getTypeSize(s.t);
        RuntimeType* varType = s.t;

        if (s.t.b == RuntimeTypeBase.Struct) {
                Context.Symbol* strct = c.findSymbol(s.t.structName.idup);
                assert(strct, "Struct " ~ s.t.structName.idup ~ " not found");
                varSize = 8; // Pointer size
                varType = strct.type;
        }

        if (varSize == 0) {
                assert(0, "Cannot create variable of type void");
        }

        // Allocate space for the variable (pointer for structs)
        c.text ~= c.s ~ "sub rsp, " ~ varSize.to!string;
        string varName = s.id.lx.idup;
        c.addSymbol(varName, varType);

        // Compile the expression (struct address in rax)
        s.e.accept(s.e, v);

        string reg = ""; string spec = "";
        getGenPReg(varSize, &reg, &spec);
        c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ c.stackOffset.to!string ~ "], " ~ reg;
}

// void compileStmtLet(Visitor* v, StmtLet s) {
//         Context* c = cast(Context*)v.context;

//         size_t varSize = getTypeSize(s.t);
//         RuntimeType* varType = s.t;

//         if (s.t.b == RuntimeTypeBase.Struct) {
//                 Context.Symbol* strct = c.findSymbol(s.t.structName.idup);
//                 assert(strct, "Struct " ~ s.t.structName.idup ~ " not found");
//                 writeln("Let ", s.id.lx.idup, " struct symbol: ", strct.name, " ", *strct.type);
//                 varSize = strct.type.size;
//                 varType = strct.type; // Use the full struct type from the symbol table
//         }

//         if (varSize == 0) {
//                 assert(0, "Cannot create variable of type void");
//         }

//         // Align varSize to 8 bytes for consistency with parameters
//         if (varSize < 8) varSize = 8;

//         c.text ~= c.s ~ "sub rsp, " ~ varSize.to!string;

//         // Add symbol after allocation, store at current stackOffset + varSize
//         string varName = s.id.lx.idup;
//         c.addSymbol(varName, varType); // Use the full type, not s.t

//         s.e.accept(s.e, v);

//         string reg = "";
//         string spec = "";
//         getGenPReg(varSize, &reg, &spec);

//         // Zero-extend for unsigned types if needed
//         if (varSize < 8 && (varType.b == RuntimeTypeBase.U8 || varType.b == RuntimeTypeBase.U16 || varType.b == RuntimeTypeBase.U32)) {
//                 c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg;
//         }
//         // Store result at [rbp - stackOffset]
//         c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ c.stackOffset.to!string ~ "], " ~ reg;
// }

// 1
// void compileStmtLet(Visitor* v, StmtLet s) {
//         Context* c = cast(Context*)v.context;

//         size_t varSize = getTypeSize(s.t);

//         if (s.t.b == RuntimeTypeBase.Struct) {
//                 Context.Symbol* strct = c.findSymbol(s.t.structName.idup);
//                 assert(strct);
//                 varSize = strct.type.size;
//         }

//         if (varSize == 0) {
//                 assert(0, "Cannot create variable of type void");
//         }

//         // Align varSize to 8 bytes for consistency with parameters
//         if (varSize < 8) varSize = 8;

//         c.text ~= c.s ~ "sub rsp, " ~ varSize.to!string;

//         // Add symbol after allocation, store at current stackOffset + varSize
//         string varName = s.id.lx.idup;
//         c.addSymbol(varName, s.t);

//         s.e.accept(s.e, v);

//         string reg = "";
//         string spec = "";
//         getGenPReg(varSize, &reg, &spec);

//         // Zero-extend for unsigned types if needed
//         if (varSize < 8 && (s.t.b == RuntimeTypeBase.U8 || s.t.b == RuntimeTypeBase.U16 || s.t.b == RuntimeTypeBase.U32)) {
//                 c.text ~= c.s ~ "movzx " ~ reg ~ ", " ~ reg;  // e.g., movzx eax, al
//         }
//         // Store result at [rbp - stackOffset]
//         c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ c.stackOffset.to!string ~ "], " ~ reg;
// }

void compileStmtExpr(Visitor* v, StmtExpr s) {
        Context* c = cast(Context*)v.context;
        s.e.accept(s.e, v);
}

void getGenPReg(size_t sz, string* reg, string* spec) {
        string sizeSpec, reg_;
        switch (sz) {
        case 8: sizeSpec = "qword"; reg_ = "rax"; break;
        case 4: sizeSpec = "dword"; reg_ = "eax"; break;
        case 2: sizeSpec = "word";  reg_ = "ax";  break;
        case 1: sizeSpec = "byte";  reg_ = "al";  break;
        default: assert(0, "Invalid variable size: " ~ sz.to!string);
        }
        *reg = reg_;
        *spec = sizeSpec;
}

void getReg(size_t sz, size_t i, string* reg, string* spec) {
        string[] regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        if (i >= regs.length) {
                assert(0, "Too many parameters for register passing");
        }
        string reg_ = (sz == 8) ? regs[i] :
                (sz == 4) ? regs[i][0..2] ~ "i" :
                (sz == 2) ? regs[i][2..$] :
                regs[i][3..$];
        string spec_ = (sz == 8) ? "qword" :
                (sz == 4) ? "dword" :
                (sz == 2) ? "word" : "byte";
        *reg = reg_;
        *spec = spec_;
}

void compileStmtProc(Visitor* v, StmtProc s) {
        Context* c = cast(Context*)v.context;
        c.prolog(s.id.lx.idup);
        c.addSymbol(s.id.lx.idup, s.rtype, true, s.variadic, false);

        c.pushScope();

        if (s.isExport) {
                c.export_(s.id.lx.idup);
        }

        // Reset stackOffset for this scope
        c.stackOffset = 0;

        // First pass: Calculate total parameter space
        size_t paramSpace = 0;
        foreach (i, paramName; s.pn) {
                RuntimeType* paramType = s.pt[i];
                size_t paramSize = getTypeSize(paramType);
                if (paramSize < 8) paramSize = 8; // Minimum 8-byte alignment
                paramSpace += paramSize;
        }

        // Align stack to 16 bytes (including push rbp)
        size_t totalStackSpace = paramSpace;
        if (totalStackSpace > 0) {
                size_t totalWithRbp = totalStackSpace + 8;  // +8 for saved RBP
                if (totalWithRbp % 16 != 0) {
                        size_t padding = 16 - (totalWithRbp % 16);
                        totalStackSpace += padding;
                }
                // Allocate stack space upfront
                c.text ~= c.s ~ "sub rsp, " ~ totalStackSpace.to!string;
        }

        // Second pass: Store parameters and add symbols
        size_t currentOffset = 0;
        foreach (i, paramName; s.pn) {
                RuntimeType* paramType = s.pt[i];
                size_t paramSize = getTypeSize(paramType);
                if (paramSize < 8) paramSize = 8;

                // Increment currentOffset before storing (offset is end of param)
                currentOffset += paramSize;
                c.addSymbol(paramName.lx.idup, paramType, false, false, true);

                string reg = "";
                string spec = "";
                getReg(paramSize, i, &reg, &spec);

                // Store parameter at [rbp - currentOffset]
                c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ currentOffset.to!string ~ "], " ~ reg;
        }

        // Compile the body (may increase stackOffset for locals)
        s.b.accept(s.b, v);

        // Clean up stack, including any local variables
        size_t localSpace = (c.stackOffset > paramSpace) ? (c.stackOffset - paramSpace) : 0;
        size_t totalCleanup = totalStackSpace + localSpace;
        if (totalCleanup > 0) {
                size_t totalWithRbp = totalCleanup + 8;
                if (totalWithRbp % 16 != 0) {
                        size_t padding = 16 - (totalWithRbp % 16);
                        totalCleanup += padding;
                }
                c.text ~= c.s ~ "add rsp, " ~ totalCleanup.to!string;
        }

        c.leave();
        c.ret();
        c.popScope();
}

void compileStmtBlock(Visitor* v, StmtBlock s) {
        Context* c=cast(Context*)v.context;
        c.pushScope();
        for(size_t i=0;i<s.stmts.length;++i){
                s.stmts[i].accept(s.stmts[i],v);
        }
        c.popScope();
}

void compileStmtReturn(Visitor* v, StmtReturn s) {
        Context* c = cast(Context*)v.context;
        s.e.accept(s.e, v);
        c.leave();
        c.ret();
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

        if (c.symbols[0].length == 0 && c.symbols.length == 1) {
                c.symbols[0] ~= Context.Symbol(proc_name, 0, s.proto.rtype, true, s.proto.variadic);
        } else {
                c.symbols[0] ~= Context.Symbol(proc_name, 0, s.proto.rtype, true, s.proto.variadic);
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

        // Create a RuntimeType for the struct
        RuntimeType* structType = new RuntimeType();
        structType.b = RuntimeTypeBase.Struct;
        structType.structName = s.id.lx.idup;
        structType.memberNames = s.members.map!(m => m.lx.idup).array;
        structType.memberTypes = s.memberTypes.dup;
        structType.memberOffsets = s.memberOffsets.dup;
        structType.size = s.size;

        // Add the struct type to the symbol table (offset 0 since it’s a type, not a variable)
        c.addSymbol(s.id.lx.idup, structType, false, false, false, true);

        c.addComment("Defined struct " ~ s.id.lx.idup ~ " with size " ~ s.size.to!string);
}

void compileStmtMod(Visitor* v, StmtMod s) {
        return;
}

void compileExprStructInst(Visitor* v, ExprStructInst e) {
        Context* c = cast(Context*)v.context;
        string structName = e.structId.lx.idup;
        Context.Symbol* structSym = c.findSymbol(structName);
        if (structSym is null || structSym.type.b != RuntimeTypeBase.Struct) {
                c.text ~= c.s ~ "; ERROR: Struct " ~ structName ~ " not defined";
                return;
        }
        RuntimeType* structType = structSym.type;
        size_t structSize = structType.size;

        size_t alignedSize = (structSize + 7) & ~7;
        c.text ~= c.s ~ "sub rsp, " ~ alignedSize.to!string;
        c.addComment("Allocated " ~ alignedSize.to!string ~ " bytes for struct " ~ structName);

        size_t baseOffset = c.stackOffset + alignedSize;
        foreach (i, memberId; e.structMemIds) {
                string memberName = memberId.lx.idup;
                Expr memberExpr = e.structMemExprs[i];
                ptrdiff_t memberIndex = -1;
                for (size_t j = 0; j < structType.memberNames.length; j++) {
                        if (structType.memberNames[j] == memberName) {
                                memberIndex = j;
                                break;
                        }
                }
                if (memberIndex == -1) {
                        c.text ~= c.s ~ "; ERROR: Member " ~ memberName ~ " not found in struct " ~ structName;
                        return;
                }

                size_t memberOffset = structType.memberOffsets[memberIndex];
                RuntimeType* memberType = structType.memberTypes[memberIndex];
                size_t memberSize = getTypeSize(memberType);

                memberExpr.accept(memberExpr, v);
                string reg = ""; string spec = "";
                getGenPReg(memberSize, &reg, &spec);

                size_t stackPosition = baseOffset - memberOffset - memberSize;
                c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ stackPosition.to!string ~ "], " ~ reg;
                c.addComment("Stored " ~ memberName ~ " at offset " ~ memberOffset.to!string);
        }

        c.text ~= c.s ~ "lea rax, [rbp - " ~ baseOffset.to!string ~ "]";
        c.addComment("Struct " ~ structName ~ " address in rax");
}

// void compileExprStructInst(Visitor* v, ExprStructInst e) {
//         Context* c = cast(Context*)v.context;
//         string structName = e.structId.lx.idup;
//         Context.Symbol* structSym = c.findSymbol(structName);
//         if (structSym is null || structSym.type.b != RuntimeTypeBase.Struct) {
//                 c.text ~= c.s ~ "; ERROR: Struct " ~ structName ~ " not defined";
//                 return;
//         }
//         RuntimeType* structType = structSym.type;
//         size_t structSize = structType.size;

//         size_t alignedSize = (structSize + 7) & ~7;
//         c.text ~= c.s ~ "sub rsp, " ~ alignedSize.to!string;
//         c.addComment("Allocated " ~ alignedSize.to!string ~ " bytes for struct " ~ structName);

//         foreach (i, memberId; e.structMemIds) {
//                 string memberName = memberId.lx.idup;
//                 Expr memberExpr = e.structMemExprs[i];
//                 ptrdiff_t memberIndex = -1;
//                 for (size_t j = 0; j < structType.memberNames.length; j++) {
//                         if (structType.memberNames[j] == memberName) {
//                                 memberIndex = j;
//                                 break;
//                         }
//                 }
//                 if (memberIndex == -1) {
//                         c.text ~= c.s ~ "; ERROR: Member " ~ memberName ~ " not found in struct " ~ structName;
//                         return;
//                 }

//                 size_t memberOffset = structType.memberOffsets[memberIndex];
//                 RuntimeType* memberType = structType.memberTypes[memberIndex];
//                 size_t memberSize = getTypeSize(memberType);

//                 memberExpr.accept(memberExpr, v);
//                 string reg = ""; string spec = "";
//                 getGenPReg(memberSize, &reg, &spec);

//                 // Store at [rbp - (stackOffset + alignedSize - memberOffset)]
//                 size_t stackPosition = c.stackOffset + alignedSize - memberOffset - memberSize;
//                 c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ stackPosition.to!string ~ "], " ~ reg;
//                 c.addComment("Stored " ~ memberName ~ " at offset " ~ memberOffset.to!string);
//         }

//         c.text ~= c.s ~ "lea rax, [rbp - " ~ (c.stackOffset + alignedSize).to!string ~ "]";
//         c.addComment("Struct " ~ structName ~ " address in rax");
// }

// 1
// void compileExprStructInst(Visitor* v, ExprStructInst e) {
//         Context* c = cast(Context*)v.context;

//         // Step 1: Find the struct type in the symbol table
//         string structName = e.structId.lx.idup;
//         Context.Symbol* structSym = c.findSymbol(structName);
//         if (structSym is null || structSym.type.b != RuntimeTypeBase.Struct) {
//                 c.text ~= c.s ~ "; ERROR: Struct " ~ structName ~ " not defined";
//                 return;
//         }
//         RuntimeType* structType = structSym.type;
//         size_t structSize = structType.size;

//         // Step 2: Allocate space on the stack for the struct
//         // Align to 8 bytes for consistency
//         size_t alignedSize = (structSize + 7) & ~7; // Round up to next multiple of 8
//         c.text ~= c.s ~ "sub rsp, " ~ alignedSize.to!string;
//         c.addComment("Allocated " ~ alignedSize.to!string ~ " bytes for struct " ~ structName);

//         // Step 3: Evaluate each member expression and store it at the correct offset
//         foreach (i, memberId; e.structMemIds) {
//                 string memberName = memberId.lx.idup;
//                 Expr memberExpr = e.structMemExprs[i];

//                 // Find the member’s offset and type
//                 ptrdiff_t memberIndex = -1;
//                 for (size_t j = 0; j < structType.memberNames.length; j++) {
//                         if (structType.memberNames[j] == memberName) {
//                                 memberIndex = j;
//                                 break;
//                         }
//                 }
//                 if (memberIndex == -1) {
//                         c.text ~= c.s ~ "; ERROR: Member " ~ memberName ~ " not found in struct " ~ structName;
//                         return;
//                 }

//                 size_t memberOffset = structType.memberOffsets[memberIndex];
//                 RuntimeType* memberType = structType.memberTypes[memberIndex];
//                 size_t memberSize = getTypeSize(memberType);

//                 // Compile the member expression (result in rax)
//                 memberExpr.accept(memberExpr, v);

//                 // Determine the appropriate register and size specifier
//                 string reg = "";
//                 string spec = "";
//                 getGenPReg(memberSize, &reg, &spec);

//                 // Store the value at [rbp - (current stackOffset + memberOffset)]
//                 // Since we just subtracted alignedSize, the base is at rsp
//                 size_t stackPosition = c.stackOffset + alignedSize - memberOffset - memberSize;
//                 c.text ~= c.s ~ "mov " ~ spec ~ " [rbp - " ~ stackPosition.to!string ~ "], " ~ reg;
//                 c.addComment("Stored " ~ memberName ~ " at offset " ~ memberOffset.to!string);
//         }

//         // Step 4: Leave the address of the struct in rax
//         // The struct starts at [rbp - (stackOffset + alignedSize)]
//         c.text ~= c.s ~ "lea rax, [rbp - " ~ (c.stackOffset + alignedSize).to!string ~ "]";
//         c.addComment("Struct " ~ structName ~ " address in rax");

//         // Note: The stackOffset isn’t updated here because this is an expression,
//         // not a variable declaration. The caller (e.g., StmtLet) will handle symbol table updates.
// }

void compileStmtImport(Visitor* v, StmtImport s) {
        Context* c = cast(Context*)v.context;
        c.makeProcsExterns(s.id.lx);
}

Visitor createCodegenContext(Context* c) {
        Visitor v;
        v.context             = cast(void*)c;

        v.visitExprBin        = &compileExprBin;
        v.visitExprUn         = &compileExprUn;
        v.visitExprStrLit     = &compileExprStrLit;
        v.visitExprIntLit     = &compileExprIntLit;
        v.visitExprIdent      = &compileExprIdent;
        v.visitExprMut        = &compileExprMut;
        v.visitExprProcCall   = &compileExprProcCall;
        v.visitExprStructInst = &compileExprStructInst;
        v.visitExprGet        = &compileExprGet;

        v.visitStmtLet        = &compileStmtLet;
        v.visitStmtExpr       = &compileStmtExpr;
        v.visitStmtProc       = &compileStmtProc;
        v.visitStmtBlock      = &compileStmtBlock;
        v.visitStmtReturn     = &compileStmtReturn;
        v.visitStmtExtern     = &compileStmtExtern;
        v.visitStmtIf         = &compileStmtIf;
        v.visitStmtWhile      = &compileStmtWhile;
        v.visitStmtStruct     = &compileStmtStruct;
        v.visitStmtMod        = &compileStmtMod;
        v.visitStmtImport     = &compileStmtImport;

        return v;
}

char[] gen(Program* p, IdentGatherer[] igs) {
        // foreach (ig; igs) {
        //         foreach (s; ig.structs) {
        //                 foreach (t; s.memberTypes)
        //                         writeln(*t);
        //         }
        // }

        Context c = new Context(igs);
        Visitor v = createCodegenContext(&c);

        for (size_t i = 0; i < p.stmts.length; ++i) {
                p.stmts[i].accept(p.stmts[i], &v);
        }

        return c.write();
}
