module parser;

import std.stdio;
import std.format;
import std.conv;

import token;
import lexer;
import grammar;
import keywords;
import utils;
import types;

Token* expect(Lexer* l, TokenType e) {
        Token* hd = lexerPeek(l);
        if (!hd) {
                err(format("expected %s but got nothing", tokenTypeToStr(e)));
        } else if (hd.ty != e) {
                tokerr(hd);
                err(format("expected %s but got %s", tokenTypeToStr(e), tokenTypeToStr(hd.ty)));
        }
        return lexerNext(l);
}

private Token* expectkw(Lexer* l, Keyword k) {
        Token* hd = lexerPeek(l);
        if (!hd) {
                err(format("expected keyword %s but got nothing", k));
        } else if (hd.ty != TokenType.Keyword || hd.lx != k) {
                tokerr(hd);
                err(format("expected keyword %s but got %s", k, hd.lx));
        }
        return lexerNext(l);
}

Token* expectWoEat(Lexer* l, TokenType e) {
        Token* hd = lexerPeek(l);
        if (!hd) {
                err(format("expected %s but got nothing", tokenTypeToStr(e)));
        } else if (hd.ty != e) {
                tokerr(hd);
                err(format("expected %s but got %s", tokenTypeToStr(e), tokenTypeToStr(hd.ty)));
        }
        return hd;
}

void parseStructLitMembers(Lexer* l, ref Token*[] structMemIds, ref Expr[] structMemExprs) {
        assert(0);
}

Expr[] parseCommaSepExprs(Lexer* l, TokenType end) {
        Expr[] exprs = [];

        while (l.hd && lexerPeek(l).ty != end) {
                Expr e = parseExpr(l);
                exprs ~= e;

                if (l.hd && lexerPeek(l).ty == TokenType.Comma) {
                        cast(void)lexerDiscard(l); // ,
                } else {
                        expectWoEat(l, end);
                        break;
                }
        }
        return exprs;
}

private Expr parsePrimaryExpr(Lexer* l) {
        Expr left = null;

        while (true) {
                Token* cur = lexerPeek(l);
                if (!cur) return left;

                switch (cur.ty) {
                case TokenType.Ident: {
                        Token* ident = lexerNext(l);
                        if (l.hd && lexerPeek(l).ty == TokenType.LCurlyBracket) {
                                // expect(l, TokenType.LCurlyBracket); // {
                                // Token*[] structMemIds = [];
                                // Expr[] structMemExprs = [];
                                // parseStructInstMembers(l, structMemIds, structMemExprs, p);
                                // left = new ExprStructInst(ident, structMemIds, structMemExprs, p);
                                assert(0);
                        } else {
                                if (left) {
                                        err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                                }
                                left = new ExprIdent(ident.lx.idup);
                        }
                } break;
                case TokenType.Lparen: {
                        lexerDiscard(l); // (
                        if (left) {
                                Expr[] exprs = parseCommaSepExprs(l, TokenType.Rparen);
                                left = new ExprProcCall(left, exprs);
                        } else {
                                left = parseExpr(l);
                        }
                        cast(void)expect(l, TokenType.Rparen);
                } break;
                case TokenType.IntLit: {
                        if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                        }
                        left = new ExprIntLit(lexerNext(l).lx.to!int);
                } break;
                case TokenType.StrLit: {
                        if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                        }
                        left = new ExprStrLit(lexerNext(l).lx.idup);
                } break;
                case TokenType.Keyword: {
                        if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                        }
                        assert(0);
                } break;
                default: return left;
                }
        }
}

private Expr parseMemberExpr(Lexer* l) {
        Expr lhs = parsePrimaryExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && cur.ty == TokenType.Period) {
                lexerDiscard(l); // .
                Expr rhs = parsePrimaryExpr(l); // Right side should be an identifier or struct instantiation
                if (rhs is null) {
                        err(tokerrToStr(cur) ~ "Expected identifier or struct instantiation after '.'");
                }
                lhs = new ExprMember(lhs, rhs);

                // Check for procedure call after member access (e.g., p.f())
                cur = lexerPeek(l);
                if (cur && cur.ty == TokenType.Lparen) {
                        lexerDiscard(l); // (
                        Expr[] exprs = parseCommaSepExprs(l, TokenType.Rparen);
                        lhs = new ExprProcCall(lhs, exprs);
                        cast(void)expect(l, TokenType.Rparen);
                }
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseUnaryExpr(Lexer* l) {
        Token* cur = lexerPeek(l);
        if (cur && (cur.ty == TokenType.Minus
                    || cur.ty == TokenType.Plus
                    || cur.ty == TokenType.Bang
                    || cur.ty == TokenType.Asterisk
                    || cur.ty == TokenType.Ampersand)) {
                string op = lexerNext(l).lx.idup;
                Expr operand = parseUnaryExpr(l);
                return new ExprUn(operand, op);
        }
        return parseMemberExpr(l);
}

private Expr parseMultiplicitateExpr(Lexer *l) {
        Expr lhs = parseUnaryExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.Asterisk
                       || cur.ty == TokenType.ForwardSlash
                       || cur.ty == TokenType.Percent)) {
                string op = lexerNext(l).lx.idup;
                Expr rhs = parseUnaryExpr(l);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseAdditiveExpr(Lexer *l) {
        Expr lhs = parseMultiplicitateExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.Plus
                       || cur.ty == TokenType.Minus)) {
                string op = lexerNext(l).lx.idup;
                Expr rhs = parseMultiplicitateExpr(l);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseEqualitativeExpr(Lexer *l) {
        Expr lhs = parseAdditiveExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.DoubleEquals
                       || cur.ty == TokenType.GreaterthanEquals
                       || cur.ty == TokenType.Greaterthan
                       || cur.ty == TokenType.LessthanEquals
                       || cur.ty == TokenType.Lessthan
                       || cur.ty == TokenType.BangEquals)) {
                string op = lexerNext(l).lx.idup;
                Expr rhs = parseAdditiveExpr(l);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseLogicalExpr(Lexer *l) {
        Expr lhs = parseEqualitativeExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.DoubleAmpersand
                       || cur.ty == TokenType.DoublePipe)) {
                string op = lexerNext(l).lx.idup;
                Expr rhs = parseEqualitativeExpr(l);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseAssignmentExpr(Lexer* l) {
        Expr lhs = parseLogicalExpr(l);

        Token* cur = lexerPeek(l);
        if (!cur) return lhs;

        switch (cur.ty) {
        case TokenType.Equals:
        case TokenType.PlusEquals:
        case TokenType.MinusEquals:
        case TokenType.AsteriskEquals:
        case TokenType.ForwardSlashEquals:
        case TokenType.PercentEquals:
        case TokenType.AmpersandEquals:
        case TokenType.PipeEquals:
        case TokenType.CaretEquals: {
                string op = lexerNext(l).lx.idup;
                Expr rhs = parseAssignmentExpr(l);
                return new ExprMut(lhs, op, rhs);
        }
        default:
                return lhs;
        }
}

private Expr parseExpr(Lexer *l) {
        return parseAssignmentExpr(l);
}

private size_t typeToStr(Token* tykw) {
        switch (tykw.lx) {
        case TypeKeyword.I8:
        case TypeKeyword.U8: return 1;
        case TypeKeyword.I16:
        case TypeKeyword.U16: return 2;
        case TypeKeyword.I32:
        case TypeKeyword.U32: return 4;
        case TypeKeyword.I64:
        case TypeKeyword.U64: return 4;
        case TypeKeyword.Void: return 0;
        default: assert(0);
        }
}

private Type parseType(Lexer* l) {
        Token* t = lexerNext(l);
        Type type = null;

        if (t.ty == TokenType.TypeKeyword) {
                type = new types.PrimitiveType(t.lx.idup, typeToStr(t));
        } else {
                type = new types.StructType(t.lx.idup, []);
        }

        while (l.hd && lexerPeek(l).ty == TokenType.Asterisk) {
                lexerDiscard(l);
                type = new types.Ptr(type);
        }

        return type;
}

private StmtLet parseStmtLet(Lexer* l) {
        lexerDiscard(l); // let
        Token* name = expect(l, TokenType.Ident);
        cast(void)expect(l, TokenType.Colon);
        Type type = parseType(l);
        cast(void)expect(l, TokenType.Equals);
        Expr e = parseExpr(l);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtLet(name.lx.idup, type, e);
}

private StmtBlock parseStmtBlock(Lexer *l) {
        lexerDiscard(l); // {
        Stmt[] stmts = [];
        while (l.hd && lexerPeek(l).ty != TokenType.RCurlyBracket) {
                stmts ~= parseStmt(l);
        }
        lexerDiscard(l); // }
        return new StmtBlock(stmts);
}

private Param[] parseProcParams(Lexer *l, bool* variadic) {
        *variadic = false;
        Param[] params = [];
        cast(void)expect(l, TokenType.Lparen);


        if (lexerPeek(l).ty == TokenType.TypeKeyword && lexerPeek(l).lx == TypeKeyword.Void) {
                lexerDiscard(l); // void
                cast(void)expect(l, TokenType.Rparen);
                return params;
        }

        while (l.hd && lexerPeek(l).ty != TokenType.Rparen) {
                if (lexerPeek(l).ty == TokenType.TriplePeriod) {
                        lexerDiscard(l); // ...
                        *variadic = true;
                        cast(void)expect(l, TokenType.Rparen);
                        break;
                }
                string name = expect(l, TokenType.Ident).lx.idup;
                cast(void)expect(l, TokenType.Colon);
                Type type = parseType(l);
                params ~= new Param(name, type);
                if (lexerPeek(l).ty == TokenType.Comma) {
                        lexerDiscard(l); // ,
                } else {
                        cast(void)expect(l, TokenType.Rparen);
                        break;
                }
        }
        if (params.length == 0) {
                err(tokerrToStr(lexerPeek(l))
                        ~ " procedures with no parameters must be explicitly marked as `void`");
        }
        return params;
}

private StmtProc parseStmtProc(Lexer* l, bool isExport) {
        lexerDiscard(l); // proc
        string name = expect(l, TokenType.Ident).lx.idup;
        bool variadic = false;
        Param[] params = parseProcParams(l, &variadic);
        cast(void)expect(l, TokenType.Colon);
        Type returnType = parseType(l);
        StmtBlock block = parseStmtBlock(l);
        return new StmtProc(name, params, variadic, returnType, block, isExport);
}

private StmtReturn parseStmtReturn(Lexer* l) {
        lexerDiscard(l); // return
        Expr e = parseExpr(l);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtReturn(e);
}

private StmtExtern parseStmtExtern(Lexer* l) {
        assert(0);
}

private StmtIf parseStmtIf(Lexer* l) {
        assert(0);
}

private StmtWhile parseStmtWhile(Lexer* l) {
        assert(0);
}

private StmtStruct parseStmtStruct(Lexer* l) {
        assert(0);
}

private StmtMod parseStmtMod(Lexer* l) {
        lexerDiscard(l); // module
        Token* t = expect(l, TokenType.Ident);
        cast(void)expectkw(l, Keyword.Where);
        return new StmtMod(t.lx.dup);
}

private StmtImport parseStmtImport(Lexer* l) {
        lexerDiscard(l); // import
        Token* t = expect(l, TokenType.Ident);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtImport(t.lx.dup);
}

private Stmt parseStmtKW(Lexer* l) {
        switch (lexerPeek(l).lx) {
        case Keyword.Export: {
                lexerDiscard(l); // export
                return parseStmtProc(l, /*isExport=*/true);
        } break;
        case Keyword.Proc: {
                return parseStmtProc(l, /*isExport=*/false);
        } break;
        case Keyword.If: {
                return parseStmtIf(l);
        } break;
        case Keyword.Return: {
                return parseStmtReturn(l);
        } break;
        case Keyword.Let: {
                return parseStmtLet(l);
        } break;
        case Keyword.Extern: {
                return parseStmtExtern(l);
        } break;
        case Keyword.While: {
                return parseStmtWhile(l);
        } break;
        case Keyword.Struct: {
                return parseStmtStruct(l);
        } break;
        case Keyword.Module: {
                return parseStmtMod(l);
        } break;
        case Keyword.Import: {
                return parseStmtImport(l);
        } break;
        default: {
                err(tokerrToStr(l.hd) ~ format("invalid keyword '%s' for statement", lexerPeek(l).lx));
        } break;
        }
        assert(0 && "unreachable");
}

private StmtExpr parseStmtExpr(Lexer* l) {
        Expr e = parseExpr(l);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtExpr(e);
}

Stmt parseStmt(Lexer* l) {
        switch (lexerPeek(l).ty) {
        case TokenType.Keyword: {
                return parseStmtKW(l);
        } break;
        case TokenType.LCurlyBracket: {
                return parseStmtBlock(l);
        } break;
        default: {
                return parseStmtExpr(l);
        } break;
        }
        assert(0 && "unreachable");
}

Program parseProgram(Lexer* l) {
        Program p;

        while (l.hd && lexerPeek(l).ty != TokenType.Eof) {
                Stmt s = parseStmt(l);
                p.stmts ~= s;
        }

        return p;
}
