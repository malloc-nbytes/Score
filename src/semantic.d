module semantic;

import std.format;
import std.stdio;
import std.conv;
import std.algorithm;
import std.array;

import types;
import grammar;
import visitor;
import utils;

class Symbol {
        string name;     // "p", "f"
        Type type;       // StructType, FunctionType, etc.
        Scope scope_;    // Reference to owning scope
        size_t address;     // Memory offset or register (set later)
        this(string name, Type type, Scope scope_) {
                this.name = name;
                this.type = type;
                this.scope_ = scope_;
                this.address = 0;  // Unassigned until codegen
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
        int tmpCount;
        size_t stackOffset;
        StmtProc curProc;
        string[] errs;
        bool ok;

        this() {
                globalScope = new Scope(null);
                currentScope = globalScope;
                tmpCount = 0;
                stackOffset = 0;
                curProc = null;
                errs = [];
                ok = true;
        }

        void writeErr(string msg) {
                errs ~= msg;
                ok = false;
        }

        // Allocate stack space, return offset
        size_t allocStack(size_t size) {
                stackOffset += size;
                return stackOffset;  // Return end of allocated space (grows downward)
        }

        // Reset stack for new function
        void resetStack() {
                stackOffset = 0;
        }
}

SemanticAnalyzer semanticAnalyze(Program p) {
        SemanticAnalyzer s = new SemanticAnalyzer;
        Visitor v = createVisitor(s);
        foreach (stmt; p.stmts) {
                stmt.accept(stmt, &v);
        }

        return s;
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

bool isTypeCompatible(Type t1, Type t2) {
        bool isNumberAndPrim = (t1.kind == TypeKind.Primitive && t2.kind == TypeKind.Number)
                || (t1.kind == TypeKind.Number && t2.kind == TypeKind.Primitive);

        if (t1.kind == TypeKind.Number && t2.kind == TypeKind.Number) {
                return true;
        }
        if (t1 is null || t2 is null) {
                return false;
        }
        if (t1 is t2) { // Same object
                return true;
        }
        if (!isNumberAndPrim && t1.kind != t2.kind) {
                return false;
        }

        if (isNumberAndPrim && t1.kind == TypeKind.Number) {
                //t1.size = t2.size;
                return true;
        } else if (isNumberAndPrim && t2.kind == TypeKind.Number) {
                //t2.size = t1.size;
                return true;
        }

        switch (t1.kind) {
        case TypeKind.Primitive:
                string t1n = (cast(PrimitiveType)t1).name;
                string t2n = (cast(PrimitiveType)t2).name;
                return t1n == t2n;
        case TypeKind.Never: return false;
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
        default: assert(0);
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

        if (s.type.kind == TypeKind.Never) {
                err(format("Type error in let: cannot be marked as `Never` (!)"));
        }

        s.expr.accept(s.expr, v);  // Populates s.expr.type and s.expr.temp

        if (s.type) {
                Symbol sym = ana.currentScope.lookup(s.type.name);
                if (sym && sym.type.kind == TypeKind.Struct) {
                        s.type = sym.type;
                }
        } else if (s.expr.type) {
                s.type = s.expr.type;
        }

        if (s.type && s.expr.type && !isTypeCompatible(s.type, s.expr.type)) {
                err(format("type mismatch in let: expected %s, got %s", s.type.name, s.expr.type.name));
        }

        size_t offset = ana.allocStack(s.type.size);
        s.offset = offset;
        Symbol sym = new Symbol(s.name, s.type, ana.currentScope);
        sym.address = offset;
        ana.currentScope.addSymbol(sym);

        if (s.expr.kind == ExprType.StructLit) {
                // For struct literals, allocate and initialize directly
                ExprStructLit structLit = cast(ExprStructLit)s.expr;
                foreach (init; structLit.fields) {
                        init.expr.accept(init.expr, v);  // Revisit to get field values
                        foreach (field; (cast(StructType)s.type).fields) {
                                if (field.name == init.name) {
                                        break;
                                }
                        }
                }
        }
}

void visitStmtProc(Visitor* v, StmtProc s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        ana.curProc = s;

        if (s.name == "_start" && s.returnType.kind != TypeKind.Never) {
                err(format("Entry procedure _start must be marked as `Never` (!)"));
        }

        Type[] paramTypes;
        foreach (param; s.params) {
                paramTypes ~= param.type;
        }
        Type procType = new ProcType(s.returnType, paramTypes, s.variadic, s.returnType.size);
        ana.currentScope.addSymbol(new Symbol(s.name, procType, ana.currentScope));

        Scope oldScope = ana.currentScope;
        ana.currentScope = new Scope(ana.currentScope);
        ana.resetStack();  // Reset stack offset for this function

        foreach (i, param; s.params) {
                size_t offset = ana.allocStack(param.type.size);
                Symbol sym = new Symbol(param.name, param.type, ana.currentScope);
                sym.address = offset;
                s.params[i].address = cast(int)offset;
                ana.currentScope.addSymbol(sym);
        }
        s.block.accept(s.block, v);

        ana.currentScope = oldScope;

        bool procIsVoid = s.returnType.size == 0;
        if ((!procIsVoid && s.block.stmts.length == 0) || (!procIsVoid && s.block.stmts[$-1].kind != StmtType.Return)) {
                err(format("Non-(void/Never) procedure missing return final statement."));
        }
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
        if (!isTypeCompatible(ana.curProc.returnType, s.expr.type)) {
                if (ana.curProc.returnType.kind == TypeKind.Never) {
                        err(format("Procedures marked as `never` (!) cannot have returns. Use `exit <expr>;` instead."));
                }
        }
}

void visitStmtIf(Visitor* v, StmtIf s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        s.expr.accept(s.expr, v);

        // Then branch
        s.then.accept(s.then, v);

        if (s.else_) {
                s.else_.accept(s.else_, v);
        }
}

void visitStmtWhile(Visitor* v, StmtWhile s) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        // IR: Loop start
        s.expr.accept(s.expr, v);

        // Loop body
        s.stmt.accept(s.stmt, v);

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

void visitStmtExit(Visitor* v, StmtExit s) {
        if (s.expr) {
                s.expr.accept(s.expr, v);
        }
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

        // Don’t allocate stack here; `visitStmtLet` will handle it
        // Just prepare the field initializations
        foreach (init; e.fields) {
                init.expr.accept(init.expr, v);
                foreach (field; structType.fields) {
                        if (field.name == init.name) {
                                if (!isTypeCompatible(field.type, init.expr.type)) {
                                        err(format("type mismatch for field '%s': expected %s, got %s",
                                                   init.name, field.type.name, init.expr.type.name));
                                }
                                // Store operations will be finalized in visitStmtLet
                                break;
                        }
                }
        }
}

void visitExprBin(Visitor* v, ExprBin e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;

        e.left.accept(e.left, v);
        e.right.accept(e.right, v);

        if (e.left.type.kind == TypeKind.Primitive && e.right.type.kind == TypeKind.Number) {
                e.right.type = e.left.type;
        } else if (e.right.type.kind == TypeKind.Number && e.left.type.kind == TypeKind.Primitive) {
                e.left.type = e.right.type;
        }

        if (!isTypeCompatible(e.left.type, e.right.type)) {
                err("binary op type mismatch");
        }
        e.type = e.left.type;
        switch (e.op) {
        case "+": break;
        case "-": break;
        case "*": break;
        case "/": break;
        case "==":
                e.type = new PrimitiveType("bool", 1);
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
                break;
        case "*":  // Dereference
                if (e.expr.type.kind != TypeKind.Ptr) {
                        err("dereference requires a pointer type");
                }
                e.type = (cast(Ptr)e.expr.type).to;
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
        e.type = new Ptr(new PrimitiveType("u8", 1));  // Pointer to u8

        // Generate a unique label for the string
        string strLabel = format("str%d", ana.tmpCount);  // Use tmpCount for uniqueness
}

void visitExprIntLit(Visitor* v, ExprIntLit e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        e.type = new NumberType();  // Default to 32-bit integer
}

void visitExprIdent(Visitor* v, ExprIdent e) {
        SemanticAnalyzer ana = cast(SemanticAnalyzer)v.context;
        Symbol sym = ana.currentScope.lookup(e.name);
        if (!sym) {
                err(format("undefined identifier '%s'", e.name));
        }
        e.address = sym.address;
        e.type = sym.type;
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
}
