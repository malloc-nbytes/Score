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

enum SymType {
        Var,
        Proc,
        Struct,
}

class Sym {
        char[] id;
        SymType type;
        this(char[] id, SymType type) {
                this.id = id;
                this.type = type;
        }
}

class Var : Sym {
        RuntimeType* t;
        this(char[] id, RuntimeType* t) {
                super(id, SymType.Var);
                this.t = t;
        }
}

class Proc : Sym {
        StmtProc stmt;
        this(StmtProc stmt) {
                super(stmt.id.lx, SymType.Proc);
                this.stmt = stmt;
        }
}

class Struct : Sym {
        StmtStruct stmt;
        this(StmtStruct stmt) {
                super(stmt.id.lx, SymType.Struct);
                this.stmt = stmt;
        }
}

class Scope {
        Sym[][] syms;

        this() {
                this.syms = [[]];
        }

        void add(Sym s) {
                this.syms[$-1] ~= s;
        }

        Sym get(char[] id) {
                for (int i = cast(int)this.syms.length-1; i >= 0; --i) {
                        for (size_t j = 0; j < this.syms[i].length; ++j) {
                                if (this.syms[i][j].id == id) {
                                        return this.syms[i][j];
                                }
                        }
                }
                return null;
        }

        void push() {
                this.syms ~= [[]];
        }

        void pop() {
                assert(this.syms.length > 0);
                this.syms.length--;
        }
}

class Context {
        IdentGatherer[] igs;
        string[] data;
        string[] globals;
        string [] strcts;
        Scope scpe;

        int lc;
        int gc;

        this(IdentGatherer[] igs) {
                this.igs = igs;
                this.data = [];
                this.globals = [];
                this.strcts = [];
                this.scpe = new Scope;
                this.lc = 0;
                this.gc = 0;

                // for (size_t i = 0; i < this.igs.length; ++i) {
                //         for (size_t j = 0; j < this.igs[i].procs.length; ++j) {
                //                 StmtProc* s = &this.igs[i].procs[j];
                //                 assert(s);
                //                 if (s.isExport) {
                //                         this.addSymbol(s.id.lx.idup,
                //                                        s.rtype, true,
                //                                        s.isExport, false);
                //                 }
                //         }
                // }
        }

        string genTmpVar() {
                return "%" ~ "__tmp_" ~ (this.lc++).to!string;
        }

        string genGlobalVar() {
                return "$__global" ~ (this.gc++).to!string;
        }

        void add(string s, bool add_space = true) {
                string buf = "";
                if (add_space) {
                        buf ~= "    ";
                }
                this.data ~= buf ~ s;
        }

        void addGlobal(string s) {
                this.globals ~= s;
        }

        void addStruct(string s) {
                this.strcts ~= s;
        }

        char[] write() {
                char[] res = [];
                for (size_t i = 0; i < this.globals.length; ++i) {
                        res ~= this.globals[i] ~ "\n";
                }
                for (size_t i = 0; i < this.strcts.length; ++i) {
                        res ~= this.strcts[i] ~ "\n";
                }
                for (size_t i = 0; i < this.data.length; ++i) {
                        res ~= this.data[i] ~ "\n";
                }
                return res;
        }
}

string compileExprBin(ExprBin e, Context c) {
        string l = compileExpr(e.l, c);
        string r = compileExpr(e.r, c);
        string result = c.genTmpVar();

        switch (e.op.ty) {
                // Arithmetic operators
        case TokenType.Plus:
                c.add(result ~ " =w add " ~ l ~ ", " ~ r);
                break;
        case TokenType.Minus:
                c.add(result ~ " =w sub " ~ l ~ ", " ~ r);
                break;
        case TokenType.Asterisk:
                c.add(result ~ " =w mul " ~ l ~ ", " ~ r);
                break;
        case TokenType.ForwardSlash:
                c.add(result ~ " =w div " ~ l ~ ", " ~ r);
                break;
        case TokenType.Percent:
                c.add(result ~ " =w rem " ~ l ~ ", " ~ r);  // Remainder
                break;

                // Comparison operators (return 0 or 1)
        case TokenType.DoubleEquals:
                c.add(result ~ " =w ceqw " ~ l ~ ", " ~ r);  // Compare equal word
                break;
        case TokenType.BangEquals:
                c.add(result ~ " =w cnew " ~ l ~ ", " ~ r);  // Compare not equal word
                break;
        case TokenType.Lessthan:
                c.add(result ~ " =w csltw " ~ l ~ ", " ~ r); // Compare signed less than
                break;
        case TokenType.Greaterthan:
                c.add(result ~ " =w csgtw " ~ l ~ ", " ~ r); // Compare signed greater than
                break;
        case TokenType.LessthanEquals:
                c.add(result ~ " =w cslew " ~ l ~ ", " ~ r); // Compare signed less or equal
                break;
        case TokenType.GreaterthanEquals:
                c.add(result ~ " =w csgew " ~ l ~ ", " ~ r); // Compare signed greater or equal
                break;

                // Bitwise operators
        case TokenType.Ampersand:
                c.add(result ~ " =w and " ~ l ~ ", " ~ r);
                break;
        case TokenType.Pipe:
                c.add(result ~ " =w or " ~ l ~ ", " ~ r);
                break;
        case TokenType.CaretEquals:  // Using as XOR (no separate XOR token)
                c.add(result ~ " =w xor " ~ l ~ ", " ~ r);
                break;

                // Logical operators (assuming short-circuit evaluation isn't required)
        case TokenType.DoubleAmpersand:
                c.add(result ~ " =w and " ~ l ~ ", " ~ r);  // Logical AND as bitwise for now
                break;
        case TokenType.DoublePipe:
                c.add(result ~ " =w or " ~ l ~ ", " ~ r);   // Logical OR as bitwise for now
                break;

        default:
                assert(0, "Unsupported binary operator: " ~ e.op.ty.to!string);
        }

        return result;
}

string compileExprIntlit(ExprIntLit e, Context c) {
        string lbl = c.genTmpVar();
        string num = e.i.lx.idup;
        c.add(lbl ~ " =l " ~ "copy " ~ num);
        return lbl;
}

string compileExprIdent(ExprIdent e, Context c) {
        Sym sym = c.scpe.get(e.id.lx);
        assert(sym);
        assert(sym.type == SymType.Var);

        string size = scrTypeToQbeType((cast(Var)sym).t);
        string buf = "";
        string lbl = c.genTmpVar();
        buf ~= lbl ~ " =" ~ size ~ " load" ~ size ~ " %" ~ e.id.lx;
        c.add(buf);
        return lbl;
}

string compileExprProcCall(ExprProcCall e, Context c) {
        // Get the procedure name from the left expression (should be an identifier)
        string procName;
        if (e.l.ty == ExprType.Ident) {
                procName = (cast(ExprIdent)e.l).id.lx.idup;
        } else {
                assert(0, "Procedure call must reference an identifier");
        }

        // Compile all arguments
        string[] args;
        foreach (arg; e.exprs) {
                string compiledArg = compileExpr(arg, c);
                args ~= compiledArg;
        }

        Sym sym = c.scpe.get(procName.dup);
        assert(sym && sym.type == SymType.Proc, "Called symbol must be a procedure");
        Proc proc = cast(Proc)sym;
        string returnType = scrTypeToQbeType(proc.stmt.rtype);
        bool isVariadic = proc.stmt.variadic;

        // Generate the call
        string result = c.genTmpVar();
        string callLine = "";
        if (proc.stmt.rtype.b != RuntimeTypeBase.Void) {
                callLine = format("%s =%s call $%s(", result,
                                  returnType, procName);
        } else {
                callLine = format("call $%s(", procName);
        }

        // Add arguments to the call
        for (size_t i = 0; i < args.length; ++i) {
                if (i > 0) callLine ~= ", ";

                // For variadic functions, only use parameter types for explicitly declared params
                if (isVariadic && i >= proc.stmt.pt.length) {
                        // TODO: add type inference here based on the expression
                        callLine ~= format("w %s", args[i]);
                } else {
                        string argType = scrTypeToQbeType(proc.stmt.pt[i]);
                        callLine ~= format("%s %s", argType, args[i]);
                }
        }

        // Add variadic marker if the function is variadic
        if (isVariadic && args.length > 0) {
                callLine ~= ", ...";
        }

        callLine ~= ")";
        c.add(callLine);
        return result;
}

string compileExprStrLit(ExprStrLit e, Context c) {
        string var = c.genGlobalVar();
        string msg = "data " ~ var ~ " = { b \"" ~ e.s.lx.idup ~ "\", b 0 }";
        c.addGlobal(msg);
        return var;
}

string compileExprMut(ExprMut e, Context c) {
        // Ensure left-hand side is an identifier
        string varName;
        if (e.l.ty == ExprType.Ident) {
                varName = (cast(ExprIdent)e.l).id.lx.idup;
        } else {
                assert(0, "Left-hand side of assignment must be an identifier");
        }

        // Look up the variable to get its type
        Sym sym = c.scpe.get(varName.dup);
        assert(sym && sym.type == SymType.Var, "Assigned symbol must be a variable");
        Var var = cast(Var)sym;
        string typeSize = scrTypeToQbeType(var.t);

        // Compile the right-hand side
        string rhs = compileExpr(e.r, c);

        // Handle different assignment operators
        switch (e.eqty.ty) {
        case TokenType.Equals:  // Simple assignment
                c.add(format("store%s %s, %%%s", typeSize, rhs, varName));
                break;

        case TokenType.PlusEquals:  // +=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s add %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.MinusEquals:  // -=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s sub %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.AsteriskEquals:  // *=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s mul %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.ForwardSlashEquals:  // /=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s div %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.PercentEquals:  // %=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s rem %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.AmpersandEquals:  // &=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s and %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.PipeEquals:  // |=
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s or %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        case TokenType.CaretEquals:  // ^= (XOR)
                string tmp = c.genTmpVar();
                c.add(format("%s =%s load%s %%%s", tmp, typeSize, typeSize, varName));
                c.add(format("%s =%s xor %s, %s", tmp, typeSize, tmp, rhs));
                c.add(format("store%s %s, %%%s", typeSize, tmp, varName));
                break;

        default:
                assert(0, "Unsupported assignment operator: " ~ e.eqty.ty.to!string);
        }

        // Return the variable name as the expression result
        return "%" ~ varName;
}

string compileExprStructInst(ExprStructInst e, Context c) {
        // Look up the struct definition
        Sym sym = c.scpe.get(e.structId.lx);
        assert(sym && sym.type == SymType.Struct, "Must reference a defined struct");
        Struct st = cast(Struct)sym;

        // Allocate space for the struct
        string structVar = c.genTmpVar();
        size_t size = st.stmt.size;  // Total size from StmtStruct
        c.add(format("%s =l alloc8 %d", structVar, size));

        // Compile and store each member
        for (size_t i = 0; i < e.structMemExprs.length; i++) {
                string value = compileExpr(e.structMemExprs[i], c);
                string typeSize = scrTypeToQbeType(st.stmt.memberTypes[i]);
                size_t offset = st.stmt.memberOffsets[i];

                // Store at offset
                string ptrTmp = c.genTmpVar();
                c.add(format("%s =l add %s, %d", ptrTmp, structVar, offset));
                c.add(format("store%s %s, %s", typeSize, value, ptrTmp));
        }

        return structVar;
}

string compileExprGet(ExprGet e, Context c) {
        string base = compileExpr(e.l, c);

        // Right-hand side must be an identifier or a procedure call
        if (e.r.ty == ExprType.Ident) {
                // Field access (e.g., p.x)
                string fieldName = (cast(ExprIdent)e.r).id.lx.idup;

                // Determine the type of the left-hand side
                Sym sym = null;
                if (e.l.ty == ExprType.Ident) {
                        sym = c.scpe.get((cast(ExprIdent)e.l).id.lx);
                } else {
                        // For chained expressions, we need type info from elsewhere
                        assert(0, "Chained field access requires type inference not yet implemented");
                }
                assert(sym && sym.type == SymType.Var, "Left-hand side must be a variable");
                Var var = cast(Var)sym;

                // Check if it's a struct
                if (var.t.b == RuntimeTypeBase.Struct) {
                        Sym structSym = c.scpe.get(var.t.structName.dup);
                        assert(structSym && structSym.type == SymType.Struct, "Must reference a defined struct");
                        Struct st = cast(Struct)structSym;

                        // Find the field offset and type
                        size_t offset = 0;
                        string typeSize = "";
                        for (size_t i = 0; i < st.stmt.members.length; i++) {
                                if (st.stmt.members[i].lx == fieldName) {
                                        offset = st.stmt.memberOffsets[i];
                                        typeSize = scrTypeToQbeType(st.stmt.memberTypes[i]);
                                        break;
                                }
                        }
                        assert(typeSize != "", "Field not found in struct: " ~ fieldName);

                        // Generate code to access the field
                        string ptrTmp = c.genTmpVar();
                        string result = c.genTmpVar();
                        c.add(format("%s =l add %s, %d", ptrTmp, base, offset));
                        c.add(format("%s =%s load%s %s", result, typeSize, typeSize, ptrTmp));
                        return result;
                } else {
                        assert(0, "Member access on non-struct type");
                }
        } else if (e.r.ty == ExprType.ProcCall) {
                // Method call (e.g., p.f())
                ExprProcCall call = cast(ExprProcCall)e.r;
                assert(call.l.ty == ExprType.Ident, "Method name must be an identifier");
                string procName = (cast(ExprIdent)call.l).id.lx.idup;

                // Compile arguments
                string[] args;
                foreach (arg; call.exprs) {
                        args ~= compileExpr(arg, c);
                }

                // Look up the procedure
                Sym procSym = c.scpe.get(procName.dup);
                assert(procSym && procSym.type == SymType.Proc, "Called symbol must be a procedure");
                Proc proc = cast(Proc)procSym;
                string returnType = scrTypeToQbeType(proc.stmt.rtype);
                bool isVariadic = proc.stmt.variadic;

                // Generate the call, passing the base as the first argument (like 'self')
                string result = c.genTmpVar();
                string callLine = "";
                if (proc.stmt.rtype.b != RuntimeTypeBase.Void) {
                        callLine = format("%s =%s call $%s(", result, returnType, procName);
                } else {
                        callLine = format("call $%s(", procName);
                }

                // Add the base as the first argument (assuming struct pointer)
                Sym baseSym = c.scpe.get((cast(ExprIdent)e.l).id.lx);
                assert(baseSym && baseSym.type == SymType.Var);
                Var baseVar = cast(Var)baseSym;
                string baseType = baseVar.t.b == RuntimeTypeBase.Struct ?
                        ":" ~ baseVar.t.structName.idup : scrTypeToQbeType(baseVar.t);
                callLine ~= format("%s %s", baseType, base);

                // Add remaining arguments
                for (size_t i = 0; i < args.length; ++i) {
                        callLine ~= ", ";
                        if (isVariadic && i >= proc.stmt.pt.length - 1) { // -1 because base is first param
                                callLine ~= format("w %s", args[i]);
                        } else {
                                string argType = scrTypeToQbeType(proc.stmt.pt[i + 1]); // +1 for base
                                callLine ~= format("%s %s", argType, args[i]);
                        }
                }

                if (isVariadic && args.length > 0) {
                        callLine ~= ", ...";
                }
                callLine ~= ")";
                c.add(callLine);
                return result;
        } else {
                assert(0, "Right-hand side of get expression must be an identifier or procedure call");
        }
}

string compileExpr(Expr e, Context c) {
        switch (e.ty) {
        case ExprType.Bin: return compileExprBin(cast(ExprBin)e, c);
        case ExprType.Un: assert(0); break;
        case ExprType.StrLit: return compileExprStrLit(cast(ExprStrLit)e, c); break;
        case ExprType.IntLit: return compileExprIntlit(cast(ExprIntLit)e, c);
        case ExprType.Ident: return compileExprIdent(cast(ExprIdent)e, c);
        case ExprType.Mut: return compileExprMut(cast(ExprMut)e, c); break;
        case ExprType.ProcCall: return compileExprProcCall(cast(ExprProcCall)e, c);
        case ExprType.StructInst: return compileExprStructInst(cast(ExprStructInst)e, c); break;
        case ExprType.Get: return compileExprGet(cast(ExprGet)e, c); break;
        default: assert(0);
        }
        assert(0);
}

void compileStmtBlock(StmtBlock s, Context c) {
        c.scpe.push();
        for (size_t i = 0; i < s.stmts.length; ++i) {
                compileStmt(s.stmts[i], c);
        }
        c.scpe.pop();
}

void compileStmtLet(StmtLet s, Context c) {
        string allocsz = getTypeSize(s.t).to!string;
        if (s.t.b == RuntimeTypeBase.Struct) {
                Sym strct = c.scpe.get(s.t.structName.dup);
                assert(strct && strct.type == SymType.Struct);
                allocsz = (cast(Struct)strct).stmt.size.to!string;
        }
        string buf = "%" ~ s.id.lx.idup ~ " =l" ~ " alloc8 " ~ allocsz;
        c.add(buf);
        buf = "";
        string res = compileExpr(s.e, c);
        buf ~= "store" ~ scrTypeToQbeType(s.t) ~ " " ~ res ~ ", %" ~ s.id.lx.idup;
        c.add(buf);
        Var v = new Var(s.id.lx, s.t);
        c.scpe.add(v);
}

void compileStmtProc(StmtProc s, Context c) {
        c.scpe.add(new Proc(s));
        string procDef = "";
        if (s.isExport) {
                procDef ~= "export ";
        }
        procDef ~= "function ";
        procDef ~= scrTypeToQbeType(s.rtype);
        procDef ~= " $" ~ s.id.lx ~ "(";
        c.scpe.push();
        for (size_t i = 0; i < s.pn.length; ++i) {
                if (i != 0) {
                        procDef ~= ", ";
                }
                string scrTy = scrTypeToQbeType(s.pt[i]);
                if (s.pt[i].b == RuntimeTypeBase.Struct) {
                        scrTy = ":" ~ s.pt[i].structName.idup;
                }
                procDef ~= scrTy  ~ " %__" ~ s.pn[i].lx;
                Var v = new Var(s.pn[i].lx, s.pt[i]);
                c.scpe.add(v);
        }
        procDef ~= ") {";
        c.add(procDef, 0);
        c.add("@start", false);

        // Stack alloc parameters
        for (size_t i = 0; i < s.pn.length; ++i) {
                string sz = getTypeSize(s.pt[i]).to!string;
                if (s.pt[i].b == RuntimeTypeBase.Struct) {
                        Sym sym = c.scpe.get(s.pt[i].structName.dup);
                        assert(sym && sym.type == SymType.Struct);
                        sz = (cast(Struct)sym).stmt.size.to!string;
                }
                c.add("%" ~ s.pn[i].lx.idup ~ " =l" ~ " alloc8 " ~ sz, true);
        }

        for (size_t i = 0; i < s.pn.length; ++i) {
                c.add("store" ~ scrTypeToQbeType(s.pt[i]) ~ " %__" ~ s.pn[i].lx.idup ~ ", %" ~ s.pn[i].lx.idup);
        }

        compileStmtBlock(s.b, c);

        c.scpe.pop();

        // Return checks
        if (s.rtype.b == RuntimeTypeBase.Void) {
                c.add("@epilog", false);
                c.add("ret");
        } else if (s.id.lx == "main") {
                c.add("@epilog", false);
                c.add("ret 0");
        }

        c.add("}", 0);
}

void compileStmtReturn(StmtReturn s, Context c) {
        string e = compileExpr(s.e, c);
        c.add("ret " ~ e);
}

void compileStmtExtern(StmtExtern s, Context c) {
        Proc p = new Proc(s.proto);
        c.scpe.add(p);
}

void compileStmtExpr(StmtExpr s, Context c) {
        cast(void)compileExpr(s.e, c);
}

void compileStmtIf(StmtIf s, Context c) {
        // Generate unique labels
        string thenLabel = "@then_" ~ c.lc.to!string;
        string elseLabel = s.else_ ? "@else_" ~ c.lc.to!string : "@end_" ~ c.lc.to!string;
        string endLabel = "@end_" ~ c.lc.to!string;
        c.lc++; // Increment label counter

        // Compile the condition
        string cond = compileExpr(s.e, c);

        // Generate branch instruction
        // Assuming the condition evaluates to 0 (false) or non-zero (true)
        c.add(format("jnz %s, %s, %s", cond, thenLabel, elseLabel), true);

        // Then block
        c.add(thenLabel, false);
        c.scpe.push();
        if (s.then.ty == StmtType.Block) {
                compileStmtBlock(cast(StmtBlock)s.then, c);
        } else {
                compileStmt(s.then, c);
        }
        c.add(format("jmp %s", endLabel), true);
        c.scpe.pop();

        // Else block (if present)
        if (s.else_) {
                c.add(elseLabel, false);
                c.scpe.push();
                if (s.else_.ty == StmtType.Block) {
                        compileStmtBlock(cast(StmtBlock)s.else_, c);
                } else {
                        compileStmt(s.else_, c);
                }
                c.scpe.pop();
        }

        // End label
        c.add(endLabel, false);
}

void compileStmtWhile(StmtWhile s, Context c) {
        string startLabel = "@while_start_" ~ c.lc.to!string;
        string bodyLabel = "@while_body_" ~ c.lc.to!string;
        string endLabel = "@while_end_" ~ c.lc.to!string;
        c.lc++;

        // Start of the loop
        c.add(startLabel, false);

        // Compile the condition
        string cond = compileExpr(s.e, c);

        // Branch: if condition is false (0), jump to end
        c.add(format("jnz %s, %s, %s", cond, bodyLabel, endLabel), true);

        // Loop body
        c.add(bodyLabel, false);
        c.scpe.push();
        if (s.s.ty == StmtType.Block) {
                compileStmtBlock(cast(StmtBlock)s.s, c);
        } else {
                compileStmt(s.s, c);
        }
        c.add(format("jmp %s", startLabel), true);
        c.scpe.pop();

        c.add(endLabel, false);
}

void compileStmtStruct(StmtStruct s, Context c) {
        Struct strct = new Struct(s);
        c.scpe.add(strct);
        string buf = format("type :%s = { ", s.id.lx);
        for (size_t i = 0; i < s.memberTypes.length; ++i) {
                if (i != 0) {
                        buf ~= ", ";
                }
                buf ~= scrTypeToQbeType(s.memberTypes[i]);
        }
        buf ~= " }";
        c.addStruct(buf);
}

void compileStmt(Stmt s, Context c) {
        switch (s.ty) {
        case StmtType.Let: compileStmtLet(cast(StmtLet)s, c); break;
        case StmtType.Expr: compileStmtExpr(cast(StmtExpr)s, c); break;
        case StmtType.Proc: compileStmtProc(cast(StmtProc)s, c); break;
        case StmtType.Block: assert(0); break;
        case StmtType.Return: compileStmtReturn(cast(StmtReturn)s, c); break;
        case StmtType.Extern: compileStmtExtern(cast(StmtExtern)s, c); break;
        case StmtType.If: compileStmtIf(cast(StmtIf)s, c); break;
        case StmtType.While: compileStmtWhile(cast(StmtWhile)s, c); break;
        case StmtType.Struct: compileStmtStruct(cast(StmtStruct)s, c); break;
        case StmtType.Mod: assert(0); break;
        case StmtType.Import: assert(0); break;
        default: assert(0);
        }
}

char[] gen(Program* p, IdentGatherer[] igs) {
        Context c = new Context(igs);

        for (size_t i = 0; i < p.stmts.length; ++i) {
                compileStmt(p.stmts[i], c);
        }

        return c.write();
}
