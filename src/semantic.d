module semantic;

import std.format;
import std.stdio;
import std.conv;
import std.algorithm;
import std.array;

import types;
import ir;
import grammar;
import visitor;
import utils;

class Symbol {
        string name;     // "p", "f"
        Type type;       // StructType, FunctionType, etc.
        Scope scope_;    // Reference to owning scope
        int address;     // Memory offset or register (set later)
        this(string name, Type type, Scope scope_) {
                this.name = name;
                this.type = type;
                this.scope_ = scope_;
                this.address = -1;  // Unassigned until codegen
        }
}

class Scope {
        Scope parent;            // Null for global scope
        Symbol[string] symbols;  // Hash map of name -> Symbol
        this(Scope parent) {
                this.parent = parent;
        }

        void addSymbol(Symbol sym) {
                if (sym.name in symbols) {
                        err(format("redeclaration of '%s' in same scope", sym.name));
                }
                symbols[sym.name] = sym;
        }

        Symbol lookup(string name) {
                if (name in symbols) return symbols[name];
                if (parent) return parent.lookup(name);
                return null;  // Not found
        }
}

class SemanticAnalyzer {
        Scope globalScope;
        Scope currentScope;
        ProgramIR programIR;
        int tmpCount;

        this() {
                globalScope = new Scope(null);
                currentScope = globalScope;
                programIR = ProgramIR();
                tmpCount = 0;
        }

        string newTmp() {
                return format("t%d", tmpCount++);
        }
}

void semanticAnalyze(Program p) {
        SemanticAnalyzer s = new SemanticAnalyzer;
        Visitor v = createVisitor(s);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
        }

        // Debugging
        foreach (instr; s.programIR.instructions) {
                stderr.writefln("%s %s, %s", instr.op, instr.result, instr.operands);
        }
}

private Visitor createVisitor(SemanticAnalyzer s) {
        Visitor v;
        v.context = cast(void*)s;
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

bool isTypeCompatible(Type t1, Type t2) {
                if (t1 is null || t2 is null) return false;
                if (t1 is t2) return true;  // Same object
                if (t1.kind != t2.kind) return false;

                final switch (t1.kind) {
                case TypeKind.Primitive:
                        string t1n = (cast(PrimitiveType)t1).name;
                        string t2n = (cast(PrimitiveType)t2).name;
                        return t1n == t2n;
                case TypeKind.Ptr:
                        return isTypeCompatible((cast(Ptr)t1).to, (cast(Ptr)t2).to);
                case TypeKind.Struct:
                        return (cast(StructType)t1).name == (cast(StructType)t2).name;
                case TypeKind.Proc:
                        ProcType p1 = cast(ProcType)t1;
                        ProcType p2 = cast(ProcType)t2;
                        if (!isTypeCompatible(p1.returnType, p2.returnType)) return false;
                        if (p1.paramTypes.length != p2.paramTypes.length) return false;
                        foreach (i; 0 .. p1.paramTypes.length) {
                                if (!isTypeCompatible(p1.paramTypes[i], p2.paramTypes[i])) return false;
                        }
                        return true;
                }
}

void visitStmtStruct(Visitor* v, StmtStruct s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        Field[] fields;
        foreach (fieldDecl; s.fields) {
                fields ~= new Field(fieldDecl.name, fieldDecl.type);
        }
        Type structType = new StructType(s.name, fields);
        ana.currentScope.addSymbol(new Symbol(s.name, structType, ana.currentScope));
}

void visitStmtLet(Visitor* v, StmtLet s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        s.expr.accept(s.expr, v);  // Populates s.expr.type

        // If s.type is provided (e.g., from parser), verify it against the scope
        if (s.type) {
                Symbol sym = ana.currentScope.lookup(s.type.name);
                if (sym && sym.type.kind == TypeKind.Struct) {
                        s.type = sym.type;  // Use the StructType from the scope (with fields)
                }
        } else if (s.expr.type) {
                s.type = s.expr.type;  // Infer from expression if no explicit type
        }

        if (s.type && s.expr.type && !isTypeCompatible(s.type, s.expr.type)) {
                err(format("type mismatch in let: expected %s, got %s", s.type.name, s.expr.type.name));
        }

        ana.currentScope.addSymbol(new Symbol(s.name, s.type, ana.currentScope));
        ana.programIR.add(Instruction(OpCode.Store, "@" ~ s.name, [s.expr.temp]));
}

// void visitStmtLet(Visitor* v, StmtLet s) {
//         SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

//         // Type check the initializer
//         s.expr.accept(s.expr, v);
//         if (s.type && s.expr.type && !isTypeCompatible(s.type, s.expr.type)) {
//                 err(format("type mismatch in let: expected %s, got %s", s.type.name, s.expr.type.name));
//         }
//         // If no explicit type, infer from expr
//         if (!s.type && s.expr.type) {
//                 s.type = s.expr.type;
//         }
//         ana.currentScope.addSymbol(new Symbol(s.name, s.type, ana.currentScope));

//         ana.programIR.add(Instruction(OpCode.Store, "@" ~ s.name, [s.expr.temp]));
// }

void visitStmtProc(Visitor* v, StmtProc s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        // Create procedure type
        Type[] paramTypes;
        foreach (param; s.params) {
                paramTypes ~= param.type;
        }
        Type procType = new ProcType(s.returnType, paramTypes, s.variadic, 8);  // 8 for function pointer
        ana.currentScope.addSymbol(new Symbol(s.name, procType, ana.currentScope));

        ana.programIR.add(Instruction(OpCode.Label, s.name, []));

        // Analyze body in a new scope
        Scope oldScope = ana.currentScope;
        ana.currentScope = new Scope(ana.currentScope);
        foreach (i, param; s.params) {
                ana.currentScope.addSymbol(new Symbol(param.name, param.type, ana.currentScope));
                ana.programIR.add(Instruction(OpCode.Param, param.name, [format("param%d", i)]));
        }
        s.block.accept(s.block, v);
        ana.currentScope = oldScope;
}

void visitStmtExtern(Visitor* v, StmtExtern s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        Type[] paramTypes;
        foreach (param; s.params) {
                paramTypes ~= param.type;
        }
        Type procType = new ProcType(s.returnType, paramTypes, s.variadic, 8);
        ana.currentScope.addSymbol(new Symbol(s.name, procType, ana.currentScope));
}

void visitStmtBlock(Visitor* v, StmtBlock s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        Scope oldScope = ana.currentScope;
        ana.currentScope = new Scope(ana.currentScope);
        foreach (stmt; s.stmts) {
                stmt.accept(stmt, v);
        }
        ana.currentScope = oldScope;
}

void visitStmtReturn(Visitor* v, StmtReturn s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        s.expr.accept(s.expr, v);
        // Return type checked in StmtProc context (not here)
        ana.programIR.add(Instruction(OpCode.Return, "", [s.expr.temp]));
}

void visitStmtIf(Visitor* v, StmtIf s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        s.expr.accept(s.expr, v);
        string thenLbl = ana.newTmp() ~ "_then";
        string endLbl = ana.newTmp() ~ "_end";

        // IR: Check condition and jump
        ana.programIR.add(Instruction(OpCode.Eq, ana.newTmp(), [s.expr.temp, "1"]));
        string condTemp = ana.programIR.instructions[$-1].result;
        ana.programIR.add(Instruction(OpCode.JumpIf, thenLbl, [condTemp]));
        ana.programIR.add(Instruction(OpCode.Jump, endLbl, []));

        // Then branch
        ana.programIR.add(Instruction(OpCode.Label, thenLbl, []));
        s.then.accept(s.then, v);

        if (s.else_) {
                string elseLbl = ana.newTmp() ~ "_else";
                // Update the previous jump to go to else instead of end
                ana.programIR.instructions[$-2] = Instruction(OpCode.Jump, elseLbl, []); // Replace Jump to endLabel
                ana.programIR.add(Instruction(OpCode.Label, elseLbl, []));
                s.else_.accept(s.else_, v);
        }

        ana.programIR.add(Instruction(OpCode.Label, endLbl, []));
}

void visitStmtWhile(Visitor* v, StmtWhile s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        string loopLbl = ana.newTmp() ~ "_loop";
        string endLbl = ana.newTmp() ~ "_end";

        // IR: Loop start
        ana.programIR.add(Instruction(OpCode.Label, loopLbl, []));
        s.expr.accept(s.expr, v);
        ana.programIR.add(Instruction(OpCode.Eq, ana.newTmp(), [s.expr.temp, "1"]));  // Check condition
        string condTemp = ana.programIR.instructions[$-1].result;
        ana.programIR.add(Instruction(OpCode.JumpIfNot, endLbl, [condTemp]));

        // Loop body
        s.stmt.accept(s.stmt, v);
        ana.programIR.add(Instruction(OpCode.Jump, loopLbl, []));

        // Loop end
        ana.programIR.add(Instruction(OpCode.Label, endLbl, []));
}

void visitStmtExpr(Visitor* v, StmtExpr s) {
        s.expr.accept(s.expr, v);
}

void visitStmtMod(Visitor* v, StmtMod s) {
        return;
}

void visitStmtImport(Visitor* v, StmtImport s) {
        // Imports handled later (e.g., linking phase)
        return;
}

// Expression Visitors

void visitExprMember(Visitor* v, ExprMember e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.left.accept(e.left, v);  // Resolve 'p' to its type and temp
        if (!e.left.type || e.left.type.kind != TypeKind.Struct) {
                err("member access requires a struct type");
        }
        StructType structType = cast(StructType)e.left.type;
        string fieldName = e.right;
        foreach (field; structType.fields) {
                if (field.name == fieldName) {
                        e.type = field.type;
                        e.temp = ana.newTmp();
                        ana.programIR.add(Instruction(OpCode.Load, e.temp,
                                                      [format("%s + %d", e.left.temp, field.offset)]));
                        return;
                }
        }
        err(format("field '%s' not found in struct '%s'", fieldName, structType.name));
}

void visitExprStructLit(Visitor* v, ExprStructLit e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        Symbol sym = ana.currentScope.lookup(e.structName);
        if (!sym || sym.type.kind != TypeKind.Struct) {
                err(format("unknown struct '%s'", e.structName));
        }
        StructType structType = cast(StructType)sym.type;
        e.type = structType;

        string structTemp = ana.newTmp();
        ana.programIR.add(Instruction(OpCode.Alloc, structTemp, [structType.size.to!string]));

        // Check field initializers
        foreach (init; e.fields) {
                init.expr.accept(init.expr, v);
                foreach (field; structType.fields) {
                        if (field.name == init.name) {
                                if (!isTypeCompatible(field.type, init.expr.type)) {
                                        err(format("type mismatch for field '%s': expected %s, got %s",
                                                   init.name, field.type.name, init.expr.type.name));
                                }
                                ana.programIR.add(Instruction(OpCode.Store,
                                                              format("%s + %d", structTemp, field.offset), [init.expr.temp]));
                                break;
                        }
                }
        }
        e.temp = structTemp;
}

void visitExprBin(Visitor* v, ExprBin e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
        if (!isTypeCompatible(e.left.type, e.right.type)) {
                err("binary op type mismatch");
        }
        e.type = e.left.type;
        e.temp = ana.newTmp();
        switch (e.op) {
        case "+": ana.programIR.add(Instruction(OpCode.Add, e.temp, [e.left.temp, e.right.temp])); break;
        case "-": ana.programIR.add(Instruction(OpCode.Sub, e.temp, [e.left.temp, e.right.temp])); break;
        case "*": ana.programIR.add(Instruction(OpCode.Mul, e.temp, [e.left.temp, e.right.temp])); break;
        case "/": ana.programIR.add(Instruction(OpCode.Div, e.temp, [e.left.temp, e.right.temp])); break;
        case "==":
                e.type = new PrimitiveType("bool", 1);
                ana.programIR.add(Instruction(OpCode.Eq, e.temp, [e.left.temp, e.right.temp]));
                break;
        default: err(format("unsupported binary operator '%s'", e.op));
        }
}

void visitExprUn(Visitor* v, ExprUn e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.expr.accept(e.expr, v);
        if (!e.expr.type) {
                err("unary operand must have a type");
        }
        switch (e.op) {
        case "-":
                if (e.expr.type.kind != TypeKind.Primitive) {
                        err("unary minus requires a primitive type");
                }
                e.type = e.expr.type;  // Same type as operand
                e.temp = ana.newTmp();
                ana.programIR.add(Instruction(OpCode.Sub, e.temp, ["0", e.expr.temp]));
                break;
        case "*":  // Dereference
                if (e.expr.type.kind != TypeKind.Ptr) {
                        err("dereference requires a pointer type");
                }
                e.type = (cast(Ptr)e.expr.type).to;
                e.temp = ana.newTmp();
                ana.programIR.add(Instruction(OpCode.Load, e.temp, [e.expr.temp]));
                break;
        case "&":  // Address-of
                e.type = new Ptr(e.expr.type);
                e.temp = e.expr.temp;
                break;
        default:
                err(format("unsupported unary operator '%s'", e.op));
        }
}

void visitExprStrLit(Visitor* v, ExprStrLit e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        // String literal as a pointer to char
        e.type = new Ptr(new PrimitiveType("u8", 1));
        e.temp = ana.newTmp();
        ana.programIR.add(Instruction(OpCode.LoadIm, e.temp, [e.str]));
}

void visitExprIntLit(Visitor* v, ExprIntLit e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        e.type = new PrimitiveType("i32", 4);  // Default to 32-bit integer
        e.temp = ana.newTmp();
        ana.programIR.add(Instruction(OpCode.LoadIm, e.temp, [e.num.to!string]));
}

void visitExprIdent(Visitor* v, ExprIdent e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        Symbol sym = ana.currentScope.lookup(e.name);
        if (!sym) {
                err(format("undefined identifier '%s'", e.name));
        }
        e.type = sym.type;
        e.temp = ana.newTmp();
        ana.programIR.add(Instruction(OpCode.Load, e.temp, ["@" ~ e.name]));
}

void visitExprMut(Visitor* v, ExprMut e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.left.accept(e.left, v);
        e.right.accept(e.right, v);

        if (!e.left.type || !e.right.type) {
                err("mutation operands must have types");
        }

        switch (e.op) {
        case "=":
                if (!isTypeCompatible(e.left.type, e.right.type)) {
                        err(format("assignment type mismatch: %s vs %s", e.left.type.name, e.right.type.name));
                }
                // Check if left is an l-value (simplified check)
                if (e.left.kind != ExprType.Ident && e.left.kind != ExprType.Member) {
                        err("left side of assignment must be an l-value");
                }
                string target = (e.left.kind == ExprType.Ident) ?
                        "@" ~ (cast(ExprIdent)e.left).name : e.left.temp;
                ana.programIR.add(Instruction(OpCode.Store, target, [e.right.temp]));
                e.type = e.left.type;  // Type of the assignment expression is the left type
                e.temp = e.right.temp;
                break;
        default:
                err(format("unsupported mutation operator '%s'", e.op));
        }
}

void visitExprProcCall(Visitor* v, ExprProcCall e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.call.accept(e.call, v);
        foreach (arg; e.args) {
                arg.accept(arg, v);
        }
        if (!e.call.type || e.call.type.kind != TypeKind.Proc) {
                err("call target must be a procedure");
        }
        ProcType procType = cast(ProcType)e.call.type;
        if (procType.paramTypes.length != e.args.length && !procType.variadic) {
                err(format("argument count mismatch: expected %d, got %d",
                           procType.paramTypes.length, e.args.length));
        }
        foreach (i, paramType; procType.paramTypes) {
                if (!isTypeCompatible(paramType, e.args[i].type)) {
                        err(format("argument %d type mismatch: expected %s, got %s",
                                   i, paramType.name, e.args[i].type.name));
                }
        }
        e.type = procType.returnType;
        e.temp = ana.newTmp();
        string[] operands = [(cast(ExprIdent)e.call).name] ~ e.args.map!(a => a.temp).array;
        ana.programIR.add(Instruction(OpCode.Call, e.temp, operands));
}
