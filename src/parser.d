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

private Expr[] parseCommaSepExprs(Lexer* l, TokenType end) {
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
                        if (l.hd && lexerPeek(l).ty == TokenType.LCurlyBracket) {
                                assert(0 && "struct instants are unimplemented");
                        } else {
                                left = new ExprIdent(lexerNext(l));
                        }
                } break;
                case TokenType.Lparen: {
                        lexerDiscard(l); // (
                        if (left) {
                                // size_t len = 0, cap = 0;
                                // bool unused = false;
                                // Expr **exprs = parse_comma_sep_exprs(lexer, &len, &cap,
                                //                                      nullptr, TOKEN_TYPE_RIGHT_PARENTHESIS);
                                // left = (Expr *)expr_proc_call_alloc(left, exprs, len, cap);
                                Expr[] exprs = parseCommaSepExprs(l, TokenType.Rparen);
                                left = new ExprProcCall(left, exprs);
                        } else {
                                left = parseExpr(l);
                        }
                        cast(void)expect(l, TokenType.Rparen);
                } break;
                case TokenType.IntLit: {
                        left = new ExprIntLit(lexerNext(l));
                } break;
                case TokenType.StrLit: {
                        left = new ExprStrLit(lexerNext(l));
                } break;
                case TokenType.Keyword: {
                        assert(0);
                } break;
                default: return left;
                }
        }
}

private Expr parseMultiplicitateExpr(Lexer *l) {
        Expr lhs = parsePrimaryExpr(l);
        Token* cur = lexerPeek(l);
        while (cur && (cur.ty == TokenType.Asterisk
                       || cur.ty == TokenType.ForwardSlash
                       || cur.ty == TokenType.Percent)) {
                Token* op = lexerNext(l);
                Expr rhs = parsePrimaryExpr(l);
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
                Token* op = lexerNext(l);
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
                Token* op = lexerNext(l);
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
                Token* op = lexerNext(l);
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
                Token* op = lexerNext(l);
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

private RuntimeType* parseType(Lexer* l) {
        Token* name = lexerNext(l);
        RuntimeTypeBase base = getBaseTypeFromStr(name.lx);
        RuntimeType* type = new RuntimeType(base, null);

        if (type.b == RuntimeTypeBase.Custom) {
                assert(0);
        }

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

private StmtLet parseStmtLet(Lexer* l) {
        lexerDiscard(l); // let

        Token* id = expect(l, TokenType.Ident);

        cast(void)expect(l, TokenType.Colon);
        RuntimeType* ty = parseType(l);

        cast(void)expect(l, TokenType.Equals);
        Expr e = parseExpr(l);

        cast(void)expect(l, TokenType.SemiColon);

        return new StmtLet(id, ty, e);
}

private void parseFunctionArgs(Lexer* l, ref Token*[] pn, ref RuntimeType*[] pt, ref bool variadic) {
        variadic = false;

        cast(void)expect(l, TokenType.Lparen);

        if (l.hd && lexerPeek(l).ty == TokenType.Rparen) {
                err("a proc accepting no args must have `void`");
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
                RuntimeType* ty = parseType(l);
                pt ~= ty;

                if (lexerPeek(l).ty != TokenType.Comma) {
                        cast(void)expect(l, TokenType.Rparen);
                        break;
                } else {
                        cast(void)expect(l, TokenType.Comma);
                }
        }
}

private StmtBlock parseStmtBlock(Lexer *l) {
        Stmt[] stmts = [];

        cast(void)expect(l, TokenType.LCurlyBracket);

        while (l.hd && lexerPeek(l).ty != TokenType.RCurlyBracket) {
                stmts ~= parseStmt(l);
        }

        cast(void)expect(l, TokenType.RCurlyBracket);

        return new StmtBlock(stmts);
}

private StmtProc parseStmtProc(Lexer* l, bool isProto) {
        lexerDiscard(l); // proc
        Token* id = expect(l, TokenType.Ident);

        Token*[] pn = [];
        RuntimeType*[] pt = [];
        StmtBlock b = null;

        bool variadic = false;
        parseFunctionArgs(l, pn, pt, variadic);

        cast(void)expect(l, TokenType.Colon);
        RuntimeType* rtype = parseType(l);
        if (!isProto) {
                b = parseStmtBlock(l);
        }

        return new StmtProc(id, pn, pt, variadic, b);
}

private StmtReturn parseStmtReturn(Lexer* l) {
        lexerDiscard(l); // return
        Expr e = parseExpr(l);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtReturn(e);
}

private StmtExtern parseStmtExtern(Lexer* l) {
        StmtProc p = parseStmtProc(l, true);
        cast(void)expect(l, TokenType.SemiColon);
        return new StmtExtern(p);
}

private StmtIf parseStmtIf(Lexer* l) {
        lexerDiscard(l); // if

        Expr e = parseExpr(l);
        Stmt then = parseStmt(l);
        Stmt else_ = null;

        Token *t1 = lexerPeek(l);
        Token *t2 = lexerPeek(l);

        bool t1_else = t1 && t1.ty == TokenType.Keyword && t1.lx == Keyword.Else;
        bool t2_if = t2 && t2.ty == TokenType.Keyword && t2.lx == Keyword.If;

        if (t1_else && t2_if) {
                lexerDiscard(l); // else
                else_ = parseStmtIf(l);
        }
        else if (t1_else) {
                lexerDiscard(l); // else
                else_ = parseStmt(l);
        }

        return new StmtIf(e, then, else_);
}

private StmtWhile parseStmtWhile(Lexer* l) {
        lexerDiscard(l); // while
        Expr e = parseExpr(l);
        Stmt s = parseStmt(l);
        return new StmtWhile(e, s);
}

private Stmt parseStmtKW(Lexer* l) {
        switch (lexerPeek(l).lx) {
        case Keyword.Proc: {
                return parseStmtProc(l, false);
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
        default: {
                err(format("invalid keyword '%s' for statement", lexerPeek(l).lx));
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
        Stmt[] stmts;

        while (l.hd && lexerPeek(l).ty != TokenType.Eof) {
                stmts ~= parseStmt(l);
        }

        return Program(stmts);
}
