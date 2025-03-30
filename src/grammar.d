module grammar;

import std.array;
import std.algorithm;
import utils;
import std.format;

import token;
import runtimeTypes;
import visitor;

enum ExprType {
        Bin,
        Un,
        StrLit,
        IntLit,
        Ident,
        Mut,
        ProcCall,
        StructInst,
        Get,
}

//////////////////
// Expressions
//////////////////

class Expr {
        ExprType ty;
        void function(Expr e, Visitor* v) accept;
        this(ExprType ty) {
                this.ty = ty;
                switch (this.ty) {
                case ExprType.Bin:        this.accept = &acceptExprBin;        break;
                case ExprType.Un:         this.accept = &acceptExprUn;         break;
                case ExprType.StrLit:     this.accept = &acceptExprStrLit;     break;
                case ExprType.IntLit:     this.accept = &acceptExprIntLit;     break;
                case ExprType.Ident:      this.accept = &acceptExprIdent;      break;
                case ExprType.Mut:        this.accept = &acceptExprMut;        break;
                case ExprType.ProcCall:   this.accept = &acceptExprProcCall;   break;
                case ExprType.StructInst: this.accept = &acceptExprStructInst; break;
                case ExprType.Get:        this.accept = &acceptExprGet;        break;
                default: assert(0);
                }
        }
}

class ExprGet : Expr {
        Expr l, r;
        this(Expr l, Expr r) {
                super(ExprType.Get);
                this.l = l;
                this.r = r;
        }
}

class ExprBin : Expr {
        Expr l, r;
        Token* op;
        this(Expr l, Token* op, Expr r) {
                super(ExprType.Bin);
                this.l = l;
                this.op = op;
                this.r = r;
        }
}

class ExprUn : Expr {
        Token* op;
        Expr e;
        this(Token* op, Expr e) {
                super(ExprType.Un);
                this.op = op;
                this.e = e;
        }
}

class ExprStrLit : Expr {
        Token* s;
        this(Token* s) {
                super(ExprType.StrLit);
                this.s = s;
        }
}

class ExprIntLit : Expr {
        Token* i;
        this(Token* i) {
                super(ExprType.IntLit);
                this.i = i;
        }
}

class ExprIdent : Expr {
        Token* id;
        this(Token* id) {
                super(ExprType.Ident);
                this.id = id;
        }
}

class ExprMut : Expr {
        Expr l;
        Token* eqty;
        Expr r;
        this(Expr l, Token* eqty, Expr r) {
                super(ExprType.Mut);
                this.l = l;
                this.eqty = eqty;
                this.r = r;
        }
}

class ExprProcCall : Expr {
        Expr l;
        Expr[] exprs;
        this(Expr l, Expr[] exprs) {
                super(ExprType.ProcCall);
                this.l = l;
                this.exprs = exprs;
        }
}

class ExprStructInst : Expr {
        Token* structId;
        Token*[] structMemIds;
        Expr[] structMemExprs;
        Program* program;

        this(Token* structId, Token*[] structMemIds, Expr[] structMemExprs, Program* program) {
                super(ExprType.StructInst);
                this.structId = structId;
                this.structMemIds = structMemIds;
                this.structMemExprs = structMemExprs;
                this.program = program;
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
        StmtType ty;
        void function(Stmt e, Visitor* v) accept;
        this(StmtType ty) {
                this.ty = ty;
                switch (this.ty) {
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
        Token* id;
        Token*[] members;
        RuntimeType*[] memberTypes;
        size_t[] memberOffsets;
        size_t size;

        this(Token* id, Token*[] members, RuntimeType*[] memberTypes) {
                super(StmtType.Struct);
                this.id = id;
                this.members = members;
                this.memberTypes = memberTypes;
                this.memberOffsets = [];
                size_t off = 0;
                for (size_t i = 0; i < memberTypes.length; ++i) {
                        this.memberOffsets ~= off;
                        off += getTypeSize(memberTypes[i]);
                }
                this.size = off;
        }
}

// class StmtStruct : Stmt {
//         Token* id;
//         Token*[] members;
//         RuntimeType*[] memberTypes;
//         size_t[] memberOffsets;
//         size_t size;

//         this(Token* id, Token*[] members, RuntimeType*[] memberTypes) {
//                 super(StmtType.Struct);
//                 this.id = id;
//                 this.members = members;
//                 this.memberTypes = memberTypes;
//                 for (size_t i = 0; i < memberTypes.length; ++i) {
//                         size_t off = 0;
//                         if (i != 0) {
//                                 off = this.memberOffsets[i-1];
//                         }
//                         this.memberOffsets ~= getTypeSize(memberTypes[i]) + off;
//                         this.size += this.memberOffsets[i] - off;
//                 }
//         }
// }

class StmtLet : Stmt {
        Token* id;
        RuntimeType* t;
        Expr e;
        this(Token* id, RuntimeType* t, Expr e) {
                super(StmtType.Let);
                this.id = id;
                this.t = t;
                this.e = e;
        }
}

class StmtExpr : Stmt {
        Expr e;
        this(Expr e) {
                super(StmtType.Expr);
                this.e = e;
        }
}

class StmtBlock : Stmt {
        Stmt[] stmts;
        this(Stmt[] stmts) {
                super(StmtType.Block);
                this.stmts = stmts;
        }
}

class StmtProc : Stmt {
        Token* id;
        RuntimeType* rtype;
        Token*[] pn;
        RuntimeType*[] pt;
        bool variadic = false;
        StmtBlock b;
        bool isExport;

        this(Token* id, RuntimeType* rtype, Token*[] pn, RuntimeType*[] pt, bool variadic, StmtBlock b, bool isExport) {
                super(StmtType.Proc);
                this.id = id;
                this.rtype = rtype;
                this.pn = pn;
                this.pt = pt;
                this.variadic = variadic;
                this.b = b;
                this.isExport = isExport;
        }
}

class StmtReturn : Stmt {
        Expr e;
        this(Expr e) {
                super(StmtType.Return);
                this.e = e;
        }
}

class StmtExtern : Stmt {
        StmtProc proto;
        this(StmtProc proto) {
                super(StmtType.Extern);
                this.proto = proto;
        }
}

class StmtIf : Stmt {
        Expr e;
        Stmt then;
        Stmt else_; // optional

        this(Expr e, Stmt then, Stmt else_) {
                super(StmtType.If);
                this.e = e;
                this.then = then;
                this.else_ = else_;
        }
}

class StmtWhile : Stmt {
        Expr e;
        Stmt s;
        this(Expr e, Stmt s) {
                super(StmtType.While);
                this.e = e;
                this.s = s;
        }
}

class StmtMod : Stmt {
        Token* id;
        this(Token* id) {
                super(StmtType.Mod);
                this.id = id;
        }
}

class StmtImport : Stmt {
        Token* id;
        this(Token* id) {
                super(StmtType.Import);
                this.id = id;
        }
}

struct Program {
        Stmt[] stmts;
        StmtMod mod;
}
