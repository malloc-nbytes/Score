module semantic;

import std.format;
import std.stdio;

import types;
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

        this() {
                globalScope = new Scope(null);
                currentScope = globalScope;
        }

}

void semanticAnalyze(Program p) {
        SemanticAnalyzer s = new SemanticAnalyzer;
        Visitor v = createVisitor(s);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
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
                        return (cast(PrimitiveType)t1).name == (cast(PrimitiveType)t2).name;
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

        // Define the struct type with its fields
        Field[] fields;
        foreach (fieldDecl; s.fields) {
                fields ~= new Field(fieldDecl.name, fieldDecl.type);
        }
        Type structType = new StructType(s.name, fields);
        ana.currentScope.addSymbol(new Symbol(s.name, structType, ana.currentScope));
}

void visitStmtLet(Visitor* v, StmtLet s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        // Type check the initializer
        s.expr.accept(s.expr, v);
        if (s.type && s.expr.type && !isTypeCompatible(s.type, s.expr.type)) {
                err(format("type mismatch in let: expected %s, got %s", s.type.name, s.expr.type.name));
        }
        // If no explicit type, infer from expr
        if (!s.type && s.expr.type) {
                s.type = s.expr.type;
        }
        ana.currentScope.addSymbol(new Symbol(s.name, s.type, ana.currentScope));
}

void visitStmtProc(Visitor* v, StmtProc s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        // Create procedure type
        Type[] paramTypes;
        foreach (param; s.params) {
                paramTypes ~= param.type;
        }
        Type procType = new ProcType(s.returnType, paramTypes, 8);  // 8 for function pointer
        ana.currentScope.addSymbol(new Symbol(s.name, procType, ana.currentScope));

        // Analyze body in a new scope
        Scope oldScope = ana.currentScope;
        ana.currentScope = new Scope(ana.currentScope);
        foreach (param; s.params) {
                ana.currentScope.addSymbol(new Symbol(param.name, param.type, ana.currentScope));
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
        Type procType = new ProcType(s.returnType, paramTypes, 8);
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
        s.expr.accept(s.expr, v);
        // Return type checked in StmtProc context (not here)
}

void visitStmtIf(Visitor* v, StmtIf s) {
        s.expr.accept(s.expr, v);
        s.then.accept(s.then, v);
        if (s.else_) s.else_.accept(s.else_, v);
}

void visitStmtWhile(Visitor* v, StmtWhile s) {
        s.expr.accept(s.expr, v);
        s.stmt.accept(s.stmt, v);
}

void visitStmtExpr(Visitor* v, StmtExpr s) {
        s.expr.accept(s.expr, v);
}

void visitStmtMod(Visitor* v, StmtMod s) {
        // Module name doesn’t need type checking yet
}

void visitStmtImport(Visitor* v, StmtImport s) {
        // Imports handled later (e.g., linking phase)
}

// Expression Visitors
void visitExprMember(Visitor* v, ExprMember e) {
        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
        if (!e.left.type || e.left.type.kind != TypeKind.Struct) {
                err("member access requires a struct type");
        }
        StructType structType = cast(StructType)e.left.type;
        if (e.right.kind != ExprType.Ident) {
                err("member name must be an identifier");
        }
        string fieldName = (cast(ExprIdent)e.right).name;
        foreach (field; structType.fields) {
                if (field.name == fieldName) {
                        e.type = field.type;
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

        // Check field initializers
        foreach (init; e.fields) {
                init.expr.accept(init.expr, v);
                foreach (field; structType.fields) {
                        if (field.name == init.name) {
                                if (!isTypeCompatible(field.type, init.expr.type)) {
                                        err(format("type mismatch for field '%s': expected %s, got %s",
                                                   init.name, field.type.name, init.expr.type.name));
                                }
                                break;
                        }
                }
        }
}

void visitExprBin(Visitor* v, ExprBin e) {
        e.left.accept(e.left, v);
        e.right.accept(e.right, v);
        // TODO: Basic type checking (expand for operators)
        if (!isTypeCompatible(e.left.type, e.right.type)) {
                err("binary op type mismatch");
        }
        e.type = e.left.type;  // Simple assumption; refine per op
}

void visitExprUn(Visitor* v, ExprUn e) {
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
                break;
        case "*":  // Dereference
                if (e.expr.type.kind != TypeKind.Ptr) {
                        err("dereference requires a pointer type");
                }
                e.type = (cast(Ptr)e.expr.type).to;
                break;
        case "&":  // Address-of
                e.type = new Ptr(e.expr.type);
                break;
        default:
                err(format("unsupported unary operator '%s'", e.op));
        }
}

void visitExprStrLit(Visitor* v, ExprStrLit e) {
        // String literal as a pointer to char (or similar)
        e.type = new Ptr(new PrimitiveType("char", 1));  // Assuming char is 1 byte
}

void visitExprIntLit(Visitor* v, ExprIntLit e) {
        e.type = new PrimitiveType("i32", 4);  // Default to 32-bit integer
}

void visitExprIdent(Visitor* v, ExprIdent e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        Symbol sym = ana.currentScope.lookup(e.name);
        if (!sym) {
                err(format("undefined identifier '%s'", e.name));
        }
        e.type = sym.type;
}

void visitExprMut(Visitor* v, ExprMut e) {
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
                e.type = e.left.type;  // Type of the assignment expression is the left type
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
        if (procType.paramTypes.length != e.args.length) {
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
}
