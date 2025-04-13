module grammar;

import std.array;
import std.algorithm;
import utils;
import std.format;
import std.stdio;

import token;
import visitor;
import types;

enum ExprType {
        Bin,
        Un,
        StrLit,
        IntLit,
        Ident,
        Mut,
        ProcCall,
        StructLit,
        Member,
}

//////////////////
// Expressions
//////////////////

class Expr {
        ExprType kind;
        Type type;
        string temp;
        void function(Expr e, Visitor* v) accept;
        this(ExprType kind) {
                this.kind = kind;
                this.type = null;
                this.temp = null;
                switch (this.kind) {
                case ExprType.Bin:        this.accept = &acceptExprBin;        break;
                case ExprType.Un:         this.accept = &acceptExprUn;         break;
                case ExprType.StrLit:     this.accept = &acceptExprStrLit;     break;
                case ExprType.IntLit:     this.accept = &acceptExprIntLit;     break;
                case ExprType.Ident:      this.accept = &acceptExprIdent;      break;
                case ExprType.Mut:        this.accept = &acceptExprMut;        break;
                case ExprType.ProcCall:   this.accept = &acceptExprProcCall;   break;
                case ExprType.StructLit:  this.accept = &acceptExprStructLit;  break;
                case ExprType.Member:     this.accept = &acceptExprMember;     break;
                default: assert(0);
                }
        }
}

class ExprMember: Expr {
        Expr left;
        // Expr right;
        string right;
        // this(Expr left, Expr right) {
        this(Expr left, string right) {
                super(ExprType.Member);
                this.left = left;
                this.right = right;
        }
}

class ExprBin : Expr {
        Expr left;
        string op;
        Expr right;
        this(Expr left, string op, Expr right) {
                super(ExprType.Bin);
                this.left = left;
                this.op = op;
                this.right = right;
        }
}

class ExprUn : Expr {
        Expr expr;
        string op;
        this(Expr expr, string op) {
                super(ExprType.Un);
                this.expr = expr;
                this.op = op;
        }
}

class ExprStrLit : Expr {
        string str;
        this(string str) {
                super(ExprType.StrLit);
                this.str = str;
        }
}

class ExprIntLit : Expr {
        int num;
        this(int num) {
                super(ExprType.IntLit);
                this.num = num;
        }
}

class ExprIdent : Expr {
        string name;
        size_t address;
        this(string name) {
                super(ExprType.Ident);
                this.name = name;
                this.address = 0;
        }
}

class ExprMut : Expr {
        Expr left;
        string op;
        Expr right;
        this(Expr left, string op, Expr right) {
                super(ExprType.Mut);
                this.left = left;
                this.op = op;
                this.right = right;
        }
}

class ExprProcCall : Expr {
        Expr call;
        Expr[] args;
        this(Expr call, Expr[] args) {
                super(ExprType.ProcCall);
                this.call = call;
                this.args = args;
        }
}

class ExprStructLit : Expr {
        string structName;
        FieldInit[] fields;
        this(string structName, FieldInit[] fields) {
                super(ExprType.StructLit);
                this.structName = structName;
                this.fields = fields;
        }
}

class FieldInit {
        string name;
        Expr expr;
        this(string name, Expr expr) {
                this.name = name;
                this.expr = expr;
        }
}

//////////////////
// Statements
//////////////////

enum StmtType {
        Let,
        Expr,
        Proc,
        Block,
        Return,
        Extern,
        If,
        While,
        Struct,
        Mod,
        Import,
}

class Stmt {
        StmtType kind;
        void function(Stmt e, Visitor* v) accept;
        this(StmtType kind) {
                this.kind = kind;
                switch (this.kind) {
                case StmtType.Let:    this.accept = &acceptStmtLet;    break;
                case StmtType.Expr:   this.accept = &acceptStmtExpr;   break;
                case StmtType.Proc:   this.accept = &acceptStmtProc;   break;
                case StmtType.Block:  this.accept = &acceptStmtBlock;  break;
                case StmtType.Return: this.accept = &acceptStmtReturn; break;
                case StmtType.Extern: this.accept = &acceptStmtExtern; break;
                case StmtType.If:     this.accept = &acceptStmtIf;     break;
                case StmtType.While:  this.accept = &acceptStmtWhile;  break;
                case StmtType.Struct: this.accept = &acceptStmtStruct; break;
                case StmtType.Mod:    this.accept = &acceptStmtMod;    break;
                case StmtType.Import: this.accept = &acceptStmtImport; break;
                default: assert(0);
                }
        }
}

class StmtStruct : Stmt {
        string name;
        FieldDecl[] fields;
        this(string name, FieldDecl[] fields) {
                super(StmtType.Struct);
                this.name = name;
                this.fields = fields;
        }
}

class FieldDecl {
        string name;
        Type type;
        this(string name, Type type) {
                this.name = name;
                this.type = type;
        }
}

class StmtLet : Stmt {
        string name;
        Type type;
        Expr expr;
        size_t offset;
        this(string name, Type type, Expr expr) {
                super(StmtType.Let);
                this.name = name;
                this.type = type;
                this.expr = expr;
                this.offset = 0;
        }
}

class StmtExpr : Stmt {
        Expr expr;
        this(Expr expr) {
                super(StmtType.Expr);
                this.expr = expr;
        }
}

class StmtBlock : Stmt {
        Stmt[] stmts;
        this(Stmt[] stmts) {
                super(StmtType.Block);
                this.stmts = stmts;
        }
}

class Param {
        string name;
        Type type;
        this(string name, Type type) {
                this.name = name;
                this.type = type;
        }
}

class StmtProc : Stmt {
        string name;
        Param[] params;
        bool variadic;
        Type returnType;
        StmtBlock block;
        bool isExport;
        this(string name, Param[] params, bool variadic, Type returnType, StmtBlock block, bool isExport) {
                super(StmtType.Proc);
                this.name = name;
                this.params = params;
                this.variadic = variadic;
                this.returnType = returnType;
                this.block = block;
                this.isExport = isExport;
        }
}

class StmtReturn : Stmt {
        Expr expr;
        this(Expr expr) {
                super(StmtType.Return);
                this.expr = expr;
        }
}

class StmtExtern : Stmt {
        string name;
        Param[] params;
        bool variadic;
        Type returnType;
        this(string name, Param[] params, bool variadic, Type returnType) {
                super(StmtType.Extern);
                this.name = name;
                this.params = params;
                this.variadic = variadic;
                this.returnType = returnType;
        }
}

class StmtIf : Stmt {
        Expr expr;
        Stmt then;
        Stmt else_; // can be null
        this(Expr expr, Stmt then, Stmt else_) {
                super(StmtType.If);
                this.expr = expr;
                this.then = then;
                this.else_ = else_;
        }
}

class StmtWhile : Stmt {
        Expr expr;
        Stmt stmt;
        this(Expr expr, Stmt stmt) {
                super(StmtType.While);
                this.expr = expr;
                this.stmt = stmt;
        }
}

class StmtMod : Stmt {
        string name;
        this(string name) {
                super(StmtType.Mod);
                this.name = name;
        }
}

class StmtImport : Stmt {
        string name;
        this(string name) {
                super(StmtType.Import);
                this.name = name;
        }
}

struct Program {
        Stmt[] stmts;
        this(Stmt[] stmts) {
                this.stmts = stmts;
        }
}
