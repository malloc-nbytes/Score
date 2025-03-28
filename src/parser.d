module parser;

import std.stdio;
import std.format;

import token;
import lexer;
import runtimeTypes;
import grammar;
import keywords;
import utils;

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

private Expr[] parseCommaSepExprs(Lexer* l, TokenType end, Program* p) {
        Expr[] exprs = [];

        while (l.hd && lexerPeek(l).ty != end) {
                Expr e = parseExpr(l, p);
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

void parseStructInstMembers(Lexer* l, ref Token*[] structMemIds, ref Expr[] structMemExprs, Program* p) {
        while (l.hd && lexerPeek(l).ty != TokenType.RCurlyBracket) {
                Token* t = expect(l, TokenType.Ident);
                cast(void)expect(l, TokenType.Equals);
                Expr e = parseExpr(l, p);
                structMemIds ~= t;
                structMemExprs ~= e;
                if (l.hd && lexerPeek(l).ty == TokenType.Comma) {
                        lexerDiscard(l); // ,
                } else {
                        cast(void)expectWoEat(l, TokenType.RCurlyBracket);
                        break;
                }
        }
        cast(void)expect(l, TokenType.RCurlyBracket);
}

private Expr parsePrimaryExpr(Lexer* l, Program* p) {
        Expr left = null;

        while (lexerPeek(l)
               && (lexerPeek(l).ty == TokenType.Minus
               || lexerPeek(l).ty == TokenType.Bang)) {
                assert(0 && "parsing unary expressions are unimplemented");
        }

        while (true) {
                Token* cur = lexerPeek(l);
                if (!cur) return left;

                switch (cur.ty) {
                case TokenType.Ident: {
                        Token* ident = lexerNext(l);
                        if (l.hd && lexerPeek(l).ty == TokenType.LCurlyBracket) {
                                expect(l, TokenType.LCurlyBracket); // {
                                Token*[] structMemIds = [];
                                Expr[] structMemExprs = [];
                                parseStructInstMembers(l, structMemIds, structMemExprs, p);
                                left = new ExprStructInst(ident, structMemIds, structMemExprs, p);
                        } else {
                                if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                                }
                                left = new ExprIdent(ident);
                        }
                } break;
                case TokenType.Lparen: {
                        lexerDiscard(l); // (
                        if (left) {
                                Expr[] exprs = parseCommaSepExprs(l, TokenType.Rparen, p);
                                left = new ExprProcCall(left, exprs);
                        } else {
                                left = parseExpr(l, p);
                        }
                        cast(void)expect(l, TokenType.Rparen);
                } break;
                case TokenType.IntLit: {
                        if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                        }
                        left = new ExprIntLit(lexerNext(l));
                } break;
                case TokenType.StrLit: {
                        if (left) {
                                err(tokerrToStr(cur) ~ "Invalid expression, maybe missing ','?");
                        }
                        left = new ExprStrLit(lexerNext(l));
                } break;
                case TokenType.Period: {
                        lexerDiscard(l); // .
                        Expr right = parseExpr(l, p);
                        left = new ExprGet(left, right);
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

private Expr parseMultiplicitateExpr(Lexer *l, Program* p) {
        Expr lhs = parsePrimaryExpr(l, p);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.Asterisk
                       || cur.ty == TokenType.ForwardSlash
                       || cur.ty == TokenType.Percent)) {
                Token* op = lexerNext(l);
                Expr rhs = parsePrimaryExpr(l, p);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseAdditiveExpr(Lexer *l, Program* p) {
        Expr lhs = parseMultiplicitateExpr(l, p);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.Plus
                       || cur.ty == TokenType.Minus)) {
                Token* op = lexerNext(l);
                Expr rhs = parseMultiplicitateExpr(l, p);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseEqualitativeExpr(Lexer *l, Program* p) {
        Expr lhs = parseAdditiveExpr(l, p);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.DoubleEquals
                       || cur.ty == TokenType.GreaterthanEquals
                       || cur.ty == TokenType.Greaterthan
                       || cur.ty == TokenType.LessthanEquals
                       || cur.ty == TokenType.Lessthan
                       || cur.ty == TokenType.BangEquals)) {
                Token* op = lexerNext(l);
                Expr rhs = parseAdditiveExpr(l, p);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseLogicalExpr(Lexer *l, Program* p) {
        Expr lhs = parseEqualitativeExpr(l, p);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.DoubleAmpersand
                       || cur.ty == TokenType.DoublePipe)) {
                Token* op = lexerNext(l);
                Expr rhs = parseEqualitativeExpr(l, p);
                ExprBin bin = new ExprBin(lhs, op, rhs);
                lhs = bin;
                cur = lexerPeek(l);
        }
        return lhs;
}

private Expr parseAssignmentExpr(Lexer* l, Program* p) {
        Expr lhs = parseLogicalExpr(l, p);

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
                Token* op = lexerNext(l);
                Expr rhs = parseAssignmentExpr(l, p);
                return new ExprMut(lhs, op, rhs);
        }
        default:
                return lhs;
        }
}

private Expr parseExpr(Lexer *l, Program* p) {
        return parseAssignmentExpr(l, p);
}

private RuntimeType* parseType(Lexer* l, Program* p) {
        Token* name = lexerNext(l);
        RuntimeTypeBase base = getBaseTypeFromStr(name.lx);
        RuntimeType* type = new RuntimeType(base, null);

        while (true) {
                if (l.hd && lexerPeek(l).ty == TokenType.Asterisk) {
                        typeToPtr(type);
                        lexerDiscard(l); // *
                } else if (l.hd && lexerPeek(l).ty == TokenType.LSquareBracket) {
                        assert(0);
                } else {
                        break;
                }
        }

        return type;
}

private StmtLet parseStmtLet(Lexer* l, Program* p) {
        lexerDiscard(l); // let

        Token* id = expect(l, TokenType.Ident);

        cast(void)expect(l, TokenType.Colon);
        RuntimeType* ty = parseType(l, p);

        cast(void)expect(l, TokenType.Equals);
        Expr e = parseExpr(l, p);

        cast(void)expect(l, TokenType.SemiColon);

        return new StmtLet(id, ty, e);
}

private void parseFunctionArgs(Lexer* l, ref Token*[] pn, ref RuntimeType*[] pt, ref bool variadic, Program* p) {
        variadic = false;

        cast(void)expect(l, TokenType.Lparen);

        if (l.hd && lexerPeek(l).ty == TokenType.Rparen) {
                err(tokerrToStr(l.hd) ~ "a proc accepting no args must have `void`");
        }

        if (l.hd && lexerPeek(l).ty == TokenType.TypeKeyword && lexerPeek(l).lx == TypeKeyword.Void) {
                lexerDiscard(l); // void
                cast(void)expect(l, TokenType.Rparen);
                return;
        }

        while (l.hd && lexerPeek(l).ty != TokenType.Rparen) {
                if (lexerPeek(l).ty == TokenType.TriplePeriod) {
                        variadic = true;
                        lexerDiscard(l); // ...
                        cast(void)expect(l, TokenType.Rparen);
                        break;
                }

                Token* id = expect(l, TokenType.Ident);
                pn ~= id;

                cast(void)expect(l, TokenType.Colon);
                RuntimeType* ty = parseType(l, p);
                pt ~= ty;

                if (lexerPeek(l).ty != TokenType.Comma) {
                        cast(void)expect(l, TokenType.Rparen);
                        break;
                } else {
                        cast(void)expect(l, TokenType.Comma);
                }
        }
}

private StmtBlock parseStmtBlock(Lexer *l, Program* p) {
        Stmt[] stmts = [];

        cast(void)expect(l, TokenType.LCurlyBracket);

        while (l.hd && lexerPeek(l).ty != TokenType.RCurlyBracket) {
                stmts ~= parseStmt(l, p);
        }

        cast(void)expect(l, TokenType.RCurlyBracket);

        return new StmtBlock(stmts);
}

private StmtProc parseStmtProc(Lexer* l, Program* p, bool isProto, bool isExport) {
        lexerDiscard(l); // proc
        Token* id = expect(l, TokenType.Ident);

        Token*[] pn = [];
        RuntimeType*[] pt = [];
        StmtBlock b = null;

        bool variadic = false;
        parseFunctionArgs(l, pn, pt, variadic, p);

        cast(void)expect(l, TokenType.Colon);
        RuntimeType* rtype = parseType(l, p);
        if (!isProto) {
                b = parseStmtBlock(l, p);
        }

        return new StmtProc(id, rtype, pn, pt, variadic, b, isExport);
}

private StmtReturn parseStmtReturn(Lexer* l, Program* p) {
        lexerDiscard(l); // return
        Expr e = parseExpr(l, p);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtReturn(e);
}

private StmtExtern parseStmtExtern(Lexer* l, Program* p) {
        StmtProc pr = parseStmtProc(l, p, /*isProto=*/true, /*isExport=*/false);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtExtern(pr);
}

private StmtIf parseStmtIf(Lexer* l, Program* p) {
        lexerDiscard(l); // if

        Expr e = parseExpr(l, p);
        Stmt then = parseStmt(l, p);
        Stmt else_ = null;

        Token *t1 = lexerPeek(l);
        Token *t2 = lexerPeek(l);

        bool t1_else = t1 && t1.ty == TokenType.Keyword && t1.lx == Keyword.Else;
        bool t2_if = t2 && t2.ty == TokenType.Keyword && t2.lx == Keyword.If;

        if (t1_else && t2_if) {
                lexerDiscard(l); // else
                else_ = parseStmtIf(l, p);
        }
        else if (t1_else) {
                lexerDiscard(l); // else
                else_ = parseStmt(l, p);
        }

        return new StmtIf(e, then, else_);
}

private StmtWhile parseStmtWhile(Lexer* l, Program* p) {
        lexerDiscard(l); // while
        Expr e = parseExpr(l, p);
        Stmt s = parseStmt(l, p);
        return new StmtWhile(e, s);
}

private StmtStruct parseStmtStruct(Lexer* l, Program* p) {
        lexerDiscard(l); // struct
        Token* id = expect(l, TokenType.Ident);
        Token*[] members = [];
        RuntimeType*[] memberTypes = [];
        cast(void)expect(l, TokenType.LCurlyBracket);
        while (l.hd && lexerPeek(l).ty != TokenType.RCurlyBracket) {
                Token* member = expect(l, TokenType.Ident);
                cast(void)expect(l, TokenType.Colon);
                RuntimeType* memberType = parseType(l, p);
                members ~= member;
                memberTypes ~= memberType;
                if (l.hd && lexerPeek(l).ty == TokenType.Comma) {
                        lexerDiscard(l); // ,
                } else {
                        cast(void)expectWoEat(l, TokenType.RCurlyBracket);
                        break;
                }
        }
        cast(void)expect(l, TokenType.RCurlyBracket);

        StmtStruct stmt = new StmtStruct(id, members, memberTypes);

        return stmt;
}

private StmtMod parseStmtMod(Lexer* l, Program* p) {
        lexerDiscard(l); // module
        Token* id = expect(l, TokenType.Ident);
        cast(void)expectkw(l, Keyword.Where);
        return new StmtMod(id);
}

private StmtImport parseStmtImport(Lexer* l, Program* p) {
        lexerDiscard(l); // import
        Token* id = expect(l, TokenType.Ident);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtImport(id);
}

private Stmt parseStmtKW(Lexer* l, Program* p) {
        switch (lexerPeek(l).lx) {
        case Keyword.Export: {
                lexerDiscard(l); // export
                return parseStmtProc(l, p, /*isProto=*/false, /*isExport=*/true);
        } break;
        case Keyword.Proc: {
                return parseStmtProc(l, p, /*isProto=*/false, /*isExport=*/false);
        } break;
        case Keyword.If: {
                return parseStmtIf(l, p);
        } break;
        case Keyword.Return: {
                return parseStmtReturn(l, p);
        } break;
        case Keyword.Let: {
                return parseStmtLet(l, p);
        } break;
        case Keyword.Extern: {
                return parseStmtExtern(l, p);
        } break;
        case Keyword.While: {
                return parseStmtWhile(l, p);
        } break;
        case Keyword.Struct: {
                return parseStmtStruct(l, p);
        } break;
        case Keyword.Module: {
                return parseStmtMod(l, p);
        } break;
        case Keyword.Import: {
                return parseStmtImport(l, p);
        } break;
        default: {
                err(tokerrToStr(l.hd) ~ format("invalid keyword '%s' for statement", lexerPeek(l).lx));
        } break;
        }
        assert(0 && "unreachable");
}

private StmtExpr parseStmtExpr(Lexer* l, Program* p) {
        Expr e = parseExpr(l, p);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtExpr(e);
}

Stmt parseStmt(Lexer* l, Program* p) {
        switch (lexerPeek(l).ty) {
        case TokenType.Keyword: {
                return parseStmtKW(l, p);
        } break;
        case TokenType.LCurlyBracket: {
                return parseStmtBlock(l, p);
        } break;
        default: {
                return parseStmtExpr(l, p);
        } break;
        }
        assert(0 && "unreachable");
}

Program parseProgram(Lexer* l) {
        Program p;

        while (l.hd && lexerPeek(l).ty != TokenType.Eof) {
                Stmt s = parseStmt(l, &p);
                p.stmts ~= s;
        }

        return p;
}
