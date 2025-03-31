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
        // Compile the right-hand side (value to assign or operate with)
        string rhs = compileExpr(e.r, c);

        // Handle different types of left-hand side expressions
        switch (e.l.ty) {
        case ExprType.Ident: {
                // Simple variable assignment or compound assignment: x = 1, x += 2
                ExprIdent ident = cast(ExprIdent)e.l;
                Sym sym = c.scpe.get(ident.id.lx);
                assert(sym && sym.type == SymType.Var, "Assignment target must be a variable");
                Var var = cast(Var)sym;
                string typeSize = scrTypeToQbeType(var.t);
                string varName = "%" ~ ident.id.lx.idup;

                // Handle different assignment operators
                switch (e.eqty.ty) {
                case TokenType.Equals: {
                        // Simple assignment
                        c.add(format("store%s %s, %s", typeSize, rhs, varName));
                        return rhs;
                }
                case TokenType.PlusEquals:
                case TokenType.MinusEquals:
                case TokenType.AsteriskEquals:
                case TokenType.ForwardSlashEquals:
                case TokenType.PercentEquals:
                case TokenType.AmpersandEquals:
                case TokenType.PipeEquals:
                case TokenType.CaretEquals: {
                        // Compound assignment
                        string currentVal = c.genTmpVar();
                        c.add(format("%s =%s load%s %s", currentVal, typeSize, typeSize, varName));
                    
                        string result = c.genTmpVar();
                        string op;
                        switch (e.eqty.ty) {
                        case TokenType.PlusEquals:      op = "add"; break;
                        case TokenType.MinusEquals:     op = "sub"; break;
                        case TokenType.AsteriskEquals:  op = "mul"; break;
                        case TokenType.ForwardSlashEquals: op = "div"; break;
                        case TokenType.PercentEquals:   op = "rem"; break;
                        case TokenType.AmpersandEquals: op = "and"; break;
                        case TokenType.PipeEquals:      op = "or";  break;
                        case TokenType.CaretEquals:     op = "xor"; break;
                        default: assert(0); // Unreachable
                        }
                        c.add(format("%s =%s %s %s, %s", result, typeSize, op, currentVal, rhs));
                        c.add(format("store%s %s, %s", typeSize, result, varName));
                        return result;
                }
                default:
                        assert(0, "Unsupported assignment operator: " ~ e.eqty.ty.to!string);
                }
        }

        case ExprType.Un: {
                // Pointer dereference assignment: *p = 3, *p += 2
                ExprUn un = cast(ExprUn)e.l;
                assert(un.op.ty == TokenType.Asterisk, "Mutation unary operator must be dereference (*)");
                string ptr = compileExpr(un.e, c);
                string typeSize = "w";  // Default, ideally get from type info

                switch (e.eqty.ty) {
                case TokenType.Equals: {
                        c.add(format("store%s %s, %s", typeSize, rhs, ptr));
                        return rhs;
                }
                case TokenType.PlusEquals:
                case TokenType.MinusEquals:
                case TokenType.AsteriskEquals:
                case TokenType.ForwardSlashEquals:
                case TokenType.PercentEquals:
                case TokenType.AmpersandEquals:
                case TokenType.PipeEquals:
                case TokenType.CaretEquals: {
                        string currentVal = c.genTmpVar();
                        c.add(format("%s =%s load%s %s", currentVal, typeSize, typeSize, ptr));
                    
                        string result = c.genTmpVar();
                        string op;
                        switch (e.eqty.ty) {
                        case TokenType.PlusEquals:      op = "add"; break;
                        case TokenType.MinusEquals:     op = "sub"; break;
                        case TokenType.AsteriskEquals:  op = "mul"; break;
                        case TokenType.ForwardSlashEquals: op = "div"; break;
                        case TokenType.PercentEquals:   op = "rem"; break;
                        case TokenType.AmpersandEquals: op = "and"; break;
                        case TokenType.PipeEquals:      op = "or";  break;
                        case TokenType.CaretEquals:     op = "xor"; break;
                        default: assert(0); // Unreachable
                        }
                        c.add(format("%s =%s %s %s, %s", result, typeSize, op, currentVal, rhs));
                        c.add(format("store%s %s, %s", typeSize, result, ptr));
                        return result;
                }
                default:
                        assert(0, "Unsupported assignment operator for pointer: " ~ e.eqty.ty.to!string);
                }
        }

        case ExprType.Get: {
                // Struct member assignment: p.x = 4, p.x += 2
                ExprGet get = cast(ExprGet)e.l;
                string base = compileExpr(get.l, c);
                assert(get.r.ty == ExprType.Ident, "Struct member must be an identifier");
                ExprIdent member = cast(ExprIdent)get.r;

                // Look up struct type
                Sym baseSym;
                if (get.l.ty == ExprType.Ident) {
                        baseSym = c.scpe.get((cast(ExprIdent)get.l).id.lx);
                }
                assert(baseSym && baseSym.type == SymType.Var);
                Var var = cast(Var)baseSym;
                assert(var.t.b == RuntimeTypeBase.Struct || 
                       (var.t.b == RuntimeTypeBase.Ptr && var.t.nptr.b == RuntimeTypeBase.Struct));
            
                RuntimeType* structType = (var.t.b == RuntimeTypeBase.Ptr) ? var.t.nptr : var.t;
                StmtStruct structDef;
                Sym structSym = c.scpe.get(structType.structName.dup);
                assert(structSym && structSym.type == SymType.Struct);
                structDef = (cast(Struct)structSym).stmt;

                // Find member info
                size_t memberIndex = -1;
                for (size_t i = 0; i < structDef.members.length; i++) {
                        if (structDef.members[i].lx == member.id.lx) {
                                memberIndex = i;
                                break;
                        }
                }
                assert(memberIndex != -1, "Member not found in struct");
            
                size_t offset = structDef.memberOffsets[memberIndex];
                string typeSize = scrTypeToQbeType(structDef.memberTypes[memberIndex]);
                string ptrTmp = c.genTmpVar();
                c.add(format("%s =l add %s, %d", ptrTmp, base, offset));

                switch (e.eqty.ty) {
                case TokenType.Equals: {
                        c.add(format("store%s %s, %s", typeSize, rhs, ptrTmp));
                        return rhs;
                }
                case TokenType.PlusEquals:
                case TokenType.MinusEquals:
                case TokenType.AsteriskEquals:
                case TokenType.ForwardSlashEquals:
                case TokenType.PercentEquals:
                case TokenType.AmpersandEquals:
                case TokenType.PipeEquals:
                case TokenType.CaretEquals: {
                        string currentVal = c.genTmpVar();
                        c.add(format("%s =%s load%s %s", currentVal, typeSize, typeSize, ptrTmp));
                    
                        string result = c.genTmpVar();
                        string op;
                        switch (e.eqty.ty) {
                        case TokenType.PlusEquals:      op = "add"; break;
                        case TokenType.MinusEquals:     op = "sub"; break;
                        case TokenType.AsteriskEquals:  op = "mul"; break;
                        case TokenType.ForwardSlashEquals: op = "div"; break;
                        case TokenType.PercentEquals:   op = "rem"; break;
                        case TokenType.AmpersandEquals: op = "and"; break;
                        case TokenType.PipeEquals:      op = "or";  break;
                        case TokenType.CaretEquals:     op = "xor"; break;
                        default: assert(0); // Unreachable
                        }
                        c.add(format("%s =%s %s %s, %s", result, typeSize, op, currentVal, rhs));
                        c.add(format("store%s %s, %s", typeSize, result, ptrTmp));
                        return result;
                }
                default:
                        assert(0, "Unsupported assignment operator for struct member: " ~ e.eqty.ty.to!string);
                }
        }

        default:
                assert(0, "Unsupported mutation target: " ~ e.l.ty.to!string);
        }
}

// string compileExprMut(ExprMut e, Context c) {
//         // Compile the right-hand side (value to assign)
//         string rhs = compileExpr(e.r, c);

//         // Handle different types of left-hand side expressions
//         switch (e.l.ty) {
//         case ExprType.Ident: {
//                 // Simple variable assignment: x = 1
//                 ExprIdent ident = cast(ExprIdent)e.l;
//                 Sym sym = c.scpe.get(ident.id.lx);
//                 assert(sym && sym.type == SymType.Var, "Assignment target must be a variable");
//                 Var var = cast(Var)sym;

//                 string typeSize = scrTypeToQbeType(var.t);
//                 c.add(format("store%s %s, %%%s", typeSize, rhs, ident.id.lx.idup));
//                 return rhs;  // Return the value just assigned
//         }

//         case ExprType.Un: {
//                 // Pointer dereference assignment: *p = 3
//                 ExprUn un = cast(ExprUn)e.l;
//                 assert(un.op.ty == TokenType.Asterisk, "Mutation unary operator must be dereference (*)");

//                 // Compile the pointer expression
//                 string ptr = compileExpr(un.e, c);
//                 string typeSize = "w";  // Default to word size, adjust based on type info if available

//                 // In a full implementation, you'd need type info from the pointer to determine the correct size
//                 // For now, assuming word-sized values
//                 c.add(format("store%s %s, %s", typeSize, rhs, ptr));
//                 return rhs;
//         }

//         case ExprType.Get: {
//                 // Struct member assignment: p.x = 4
//                 ExprGet get = cast(ExprGet)e.l;

//                 // Left part should be the struct (or pointer to struct)
//                 string base = compileExpr(get.l, c);

//                 // Right part should be the member identifier
//                 assert(get.r.ty == ExprType.Ident, "Struct member must be an identifier");
//                 ExprIdent member = cast(ExprIdent)get.r;

//                 // Look up the struct type
//                 Sym baseSym;
//                 if (get.l.ty == ExprType.Ident) {
//                         baseSym = c.scpe.get((cast(ExprIdent)get.l).id.lx);
//                 }
//                 assert(baseSym && baseSym.type == SymType.Var);

//                 Var var = cast(Var)baseSym;
//                 assert(var.t.b == RuntimeTypeBase.Struct || 
//                        (var.t.b == RuntimeTypeBase.Ptr && var.t.nptr.b == RuntimeTypeBase.Struct),
//                        "Left side of . must be struct or struct pointer");

//                 RuntimeType* structType = (var.t.b == RuntimeTypeBase.Ptr) ? var.t.nptr : var.t;
//                 StmtStruct structDef;
//                 Sym structSym = c.scpe.get(structType.structName.dup);
//                 assert(structSym && structSym.type == SymType.Struct);
//                 structDef = (cast(Struct)structSym).stmt;

//                 // Find the member offset and type
//                 size_t memberIndex = -1;
//                 for (size_t i = 0; i < structDef.members.length; i++) {
//                         if (structDef.members[i].lx == member.id.lx) {
//                                 memberIndex = i;
//                                 break;
//                         }
//                 }
//                 assert(memberIndex != -1, "Member not found in struct");

//                 size_t offset = structDef.memberOffsets[memberIndex];
//                 string typeSize = scrTypeToQbeType(structDef.memberTypes[memberIndex]);

//                 // Generate the store
//                 string ptrTmp = c.genTmpVar();
//                 c.add(format("%s =l add %s, %d", ptrTmp, base, offset));
//                 c.add(format("store%s %s, %s", typeSize, rhs, ptrTmp));

//                 return rhs;
//         }

//         default:
//                 assert(0, "Unsupported mutation target: " ~ e.l.ty.to!string);
//         }
// }

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
        // Compile the left-hand side (the struct or pointer-to-struct)
        string base = compileExpr(e.l, c);

        // Right-hand side should be an identifier (the member name)
        assert(e.r.ty == ExprType.Ident, "Right side of . must be an identifier");
        ExprIdent member = cast(ExprIdent)e.r;

        // Determine the type of the left-hand side
        Sym baseSym;
        if (e.l.ty == ExprType.Ident) {
                baseSym = c.scpe.get((cast(ExprIdent)e.l).id.lx);
                assert(baseSym && baseSym.type == SymType.Var, "Left side of . must be a variable");
        } else {
                // For now, assume it's a variable; could be extended for nested expressions
                assert(0, "Complex left-hand side of . not yet supported");
        }

        Var var = cast(Var)baseSym;
        assert(var.t.b == RuntimeTypeBase.Struct || 
               (var.t.b == RuntimeTypeBase.Ptr && var.t.nptr.b == RuntimeTypeBase.Struct),
               "Left side of . must be struct or struct pointer");

        // Get the struct type (dereference if it's a pointer)
        RuntimeType* structType = (var.t.b == RuntimeTypeBase.Ptr) ? var.t.nptr : var.t;

        // Look up the struct definition
        Sym structSym = c.scpe.get(structType.structName.dup);
        assert(structSym && structSym.type == SymType.Struct, "Struct type must be defined");
        StmtStruct structDef = (cast(Struct)structSym).stmt;

        // Find the member in the struct
        size_t memberIndex = -1;
        for (size_t i = 0; i < structDef.members.length; i++) {
                if (structDef.members[i].lx == member.id.lx) {
                        memberIndex = i;
                        break;
                }
        }
        assert(memberIndex != -1, "Member " ~ member.id.lx.idup ~ " not found in struct " ~ structType.structName);

        // Calculate the address of the member
        size_t offset = structDef.memberOffsets[memberIndex];
        string typeSize = scrTypeToQbeType(structDef.memberTypes[memberIndex]);
        string ptrTmp = c.genTmpVar();
        c.add(format("%s =l add %s, %d", ptrTmp, base, offset));

        // Load the member's value
        string result = c.genTmpVar();
        c.add(format("%s =%s load%s %s", result, typeSize, typeSize, ptrTmp));

        return result;
}

string compileExprUn(ExprUn e, Context c) {
        // Compile the operand first
        string operand = compileExpr(e.e, c);
        string result = c.genTmpVar();

        switch (e.op.ty) {
        case TokenType.Plus:
                c.add(format("%s =w copy %s", result, operand));
                break;

        case TokenType.Minus:
                c.add(format("%s =w neg %s", result, operand));
                break;

        case TokenType.Bang:
                c.add(format("%s =w ceqw %s, 0", result, operand));
                break;

        case TokenType.Asterisk:
                // Determine the type being dereferenced (assuming pointer to word for now)
                // This is simplified - in a full implementation, we'd need type info
                string typeSize = "w";
                c.add(format("%s =w load%s %s", result, typeSize, operand));
                break;

        case TokenType.Ampersand:
                if (e.e.ty == ExprType.Ident) {
                        string varName = (cast(ExprIdent)e.e).id.lx.idup;
                        return "%" ~ varName;
                } else {
                        assert(0, "Address-of operator can only be applied to identifiers");
                }
                break;

        default:
                assert(0, "Unsupported unary operator: " ~ e.op.ty.to!string);
        }

        return result;
}

string compileExpr(Expr e, Context c) {
        switch (e.ty) {
        case ExprType.Bin: return compileExprBin(cast(ExprBin)e, c);
        case ExprType.Un: return compileExprUn(cast(ExprUn)e, c);
        case ExprType.StrLit: return compileExprStrLit(cast(ExprStrLit)e, c);
        case ExprType.IntLit: return compileExprIntlit(cast(ExprIntLit)e, c);
        case ExprType.Ident: return compileExprIdent(cast(ExprIdent)e, c);
        case ExprType.Mut: return compileExprMut(cast(ExprMut)e, c); break;
        case ExprType.ProcCall: return compileExprProcCall(cast(ExprProcCall)e, c);
        case ExprType.StructInst: return compileExprStructInst(cast(ExprStructInst)e, c);
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
                        procDef ~= scrTy ~ " %" ~ s.pn[i].lx;
                } else {
                        procDef ~= scrTy ~ " %__" ~ s.pn[i].lx;
                }
                Var v = new Var(s.pn[i].lx, s.pt[i]);
                c.scpe.add(v);
        }
        procDef ~= ")";
        if (s.variadic) {
                procDef ~= ", ...";
        }
        procDef ~= " {";
        c.add(procDef, 0);
        c.add("@start", false);

        // Handle parameters based on type
        for (size_t i = 0; i < s.pn.length; ++i) {
                if (s.pt[i].b == RuntimeTypeBase.Struct) {
                        // Structs: Use the pointer directly, no alloc/store needed
                        continue;
                } else {
                        // Primitives: Allocate stack space and store the parameter value
                        string sz = getTypeSize(s.pt[i]).to!string;
                        string paramName = s.pn[i].lx.idup;
                        c.add("%" ~ paramName ~ " =l alloc8 " ~ sz, true);
                        string typeSize = scrTypeToQbeType(s.pt[i]);
                        c.add(format("store%s %%__%s, %%%s", typeSize, paramName, paramName));
                }
        }

        compileStmtBlock(s.b, c);
        c.scpe.pop();

        if (s.rtype.b == RuntimeTypeBase.Void) {
                c.add("@epilog", false);
                c.add("ret");
        } else if (s.id.lx == "main") {
                c.add("@epilog", false);
                c.add("ret 0");
        }
        c.add("}", 0);
}

// structs
// void compileStmtProc(StmtProc s, Context c) {
//         c.scpe.add(new Proc(s));
//         string procDef = "";
//         if (s.isExport) {
//                 procDef ~= "export ";
//         }
//         procDef ~= "function ";
//         procDef ~= scrTypeToQbeType(s.rtype);
//         procDef ~= " $" ~ s.id.lx ~ "(";
//         c.scpe.push();
//         for (size_t i = 0; i < s.pn.length; ++i) {
//                 if (i != 0) {
//                         procDef ~= ", ";
//                 }
//                 string scrTy = scrTypeToQbeType(s.pt[i]);
//                 if (s.pt[i].b == RuntimeTypeBase.Struct) {
//                         scrTy = ":" ~ s.pt[i].structName.idup;
//                 }
//                 procDef ~= scrTy ~ " %" ~ s.pn[i].lx;  // Use parameter name directly
//                 Var v = new Var(s.pn[i].lx, s.pt[i]);
//                 c.scpe.add(v);
//         }
//         procDef ~= ") {";
//         c.add(procDef, 0);
//         c.add("@start", false);

//         compileStmtBlock(s.b, c);
//         c.scpe.pop();

//         if (s.rtype.b == RuntimeTypeBase.Void) {
//                 c.add("@epilog", false);
//                 c.add("ret");
//         } else if (s.id.lx == "main") {
//                 c.add("@epilog", false);
//                 c.add("ret 0");
//         }
//         c.add("}", 0);
// }

// original
// void compileStmtProc(StmtProc s, Context c) {
//         c.scpe.add(new Proc(s));
//         string procDef = "";
//         if (s.isExport) {
//                 procDef ~= "export ";
//         }
//         procDef ~= "function ";
//         procDef ~= scrTypeToQbeType(s.rtype);
//         procDef ~= " $" ~ s.id.lx ~ "(";
//         c.scpe.push();
//         for (size_t i = 0; i < s.pn.length; ++i) {
//                 if (i != 0) {
//                         procDef ~= ", ";
//                 }
//                 string scrTy = scrTypeToQbeType(s.pt[i]);
//                 if (s.pt[i].b == RuntimeTypeBase.Struct) {
//                         scrTy = ":" ~ s.pt[i].structName.idup;
//                 }
//                 procDef ~= scrTy  ~ " %__" ~ s.pn[i].lx;
//                 Var v = new Var(s.pn[i].lx, s.pt[i]);
//                 c.scpe.add(v);
//         }
//         procDef ~= ") {";
//         c.add(procDef, 0);
//         c.add("@start", false);

//         // Stack alloc parameters
//         for (size_t i = 0; i < s.pn.length; ++i) {
//                 string sz = getTypeSize(s.pt[i]).to!string;
//                 if (s.pt[i].b == RuntimeTypeBase.Struct) {
//                         Sym sym = c.scpe.get(s.pt[i].structName.dup);
//                         assert(sym && sym.type == SymType.Struct);
//                         sz = (cast(Struct)sym).stmt.size.to!string;
//                 }
//                 c.add("%" ~ s.pn[i].lx.idup ~ " =l" ~ " alloc8 " ~ sz, true);
//         }

//         for (size_t i = 0; i < s.pn.length; ++i) {
//                 c.add("store" ~ scrTypeToQbeType(s.pt[i]) ~ " %__" ~ s.pn[i].lx.idup ~ ", %" ~ s.pn[i].lx.idup);
//         }

//         compileStmtBlock(s.b, c);

//         c.scpe.pop();

//         // Return checks
//         if (s.rtype.b == RuntimeTypeBase.Void) {
//                 c.add("@epilog", false);
//                 c.add("ret");
//         } else if (s.id.lx == "main") {
//                 c.add("@epilog", false);
//                 c.add("ret 0");
//         }

//         c.add("}", 0);
// }

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
