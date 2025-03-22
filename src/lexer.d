module lexer;

import std.stdio;
import std.ascii;
import std.format;
import core.stdc.stdlib : exit;

import token;
import keywords;
import utils;

struct Lexer {
        Token *hd, tl;
}

private TokenType[string] gOps;

private void initOps() {
        gOps["("] = TokenType.Lparen;
        gOps[")"] = TokenType.Rparen;
        gOps["{"] = TokenType.LCurlyBracket;
        gOps["}"] = TokenType.RCurlyBracket;
        gOps["["] = TokenType.LSquareBracket;
        gOps["]"] = TokenType.RSquareBracket;
        gOps[":"] = TokenType.Colon;
        gOps[";"] = TokenType.SemiColon;
        gOps["."] = TokenType.Period;
        gOps["!"] = TokenType.Bang;
        gOps["<"] = TokenType.Lessthan;
        gOps[">"] = TokenType.Greaterthan;
        gOps["<="] = TokenType.LessthanEquals;
        gOps[">="] = TokenType.GreaterthanEquals;
        gOps["+="] = TokenType.PlusEquals;
        gOps["-="] = TokenType.MinusEquals;
        gOps["/="] = TokenType.ForwardSlashEquals;
        gOps["%="] = TokenType.PercentEquals;
        gOps["*="] = TokenType.AsteriskEquals;
        gOps["*"] = TokenType.Asterisk;
        gOps["/"] = TokenType.ForwardSlash;
        gOps["%"] = TokenType.Percent;
        gOps["&"] = TokenType.Ampersand;
        gOps["|"] = TokenType.Pipe;
        gOps["^"] = TokenType.Uptick;
        gOps["~"] = TokenType.Tilde;
        gOps["@"] = TokenType.At;
        gOps["?"] = TokenType.QuestionMark;
        gOps["&&"] = TokenType.DoubleAmpersand;
        gOps["||"] = TokenType.DoublePipe;
        gOps["="] = TokenType.Equals;
        gOps["=="] = TokenType.DoubleEquals;
        gOps["!="] = TokenType.BangEquals;
        gOps["&="] = TokenType.AmpersandEquals;
        gOps["|="] = TokenType.PipeEquals;
        gOps["^="] = TokenType.CaretEquals;
        gOps["+"] = TokenType.Plus;
        gOps["-"] = TokenType.Minus;
        gOps[","] = TokenType.Comma;
        gOps["..."] = TokenType.TriplePeriod;
}

void lexerAppend(Lexer *l, Token *t) {
        if (!l.hd || !l.tl) {
                l.hd = l.tl = t;
        } else {
                l.tl.next = t;
                l.tl = t;
        }
}

void lexerDump(Lexer *l) {
        Token *it = l.hd;
        while (it) {
                writeln(*it);
                it = it.next;
        }
}

Token* lexerPeek(Lexer *l, size_t p = 0) {
        Token* it = l.hd;
        for (size_t i = 0; it && i < p; ++i) {
                it = it.next;
        }
        return it;
}

void lexerDiscard(Lexer *l) {
        assert(l.hd);
        l.hd = l.hd.next;
}

Token* lexerNext(Lexer *l) {
        assert(l.hd);
        Token* t = l.hd;
        l.hd = l.hd.next;
        return t;
}

private char[] consumeWhile(const char[] src, bool delegate(dchar) pred) {
        char[] s;
        size_t i = 0;
        while (src[i] && pred(src[i])) s ~= src[i++];
        return s;
}

private bool isOp(dchar c) {
        return !isDigit(c)
                && !isAlpha(c)
                && c != '_'
                && c != ' '
                && c != '\n'
                && c != '\t'
                && c != '\r';
}

private TokenType determineTypeFromOp(const ref char[] op, size_t *sz) {
        *sz = 0;

        for (int i = cast(int)op.length; i > 0; --i) {
                const char[] actual = op[0..i];
                if (actual in gOps) {
                        *sz = cast(size_t)i;
                        return gOps[actual];
                }
        }

        *sz = 0;
        err(format("invalid operator: '%s'", op));
        return cast(TokenType)0; // unreachable
}

Lexer lexFile(const ref string src, const ref string fp) {
        initOps();

        Lexer lexer = {null, null};

        size_t i = 0, r = 1, c = 1;
        while (i < src.length) {
                char ch = src[i];
                if (ch == ' ' || ch == '\t') {
                        ++c, ++i;
                } else if (ch == '\n' || ch == '\r') {
                        ++r, c = 1, ++i;
                } else if (ch == '-' && src[i+1] == '-') {
                        char[] comment = consumeWhile(src[i..$], (dchar c) { return c != '\n'; });
                        c += comment.length, i += comment.length;
                } else if (isAlpha(ch) || ch == '_') {
                        char[] ident = consumeWhile(src[i..$], (dchar c) {
                                return isAlpha(c) || isDigit(c) || c == '_';
                        });
                        TokenType ty = TokenType.Ident;
                        if (isKeyword(ident)) {
                                ty = TokenType.Keyword;
                        } else if (isTypeKeyword(ident)) {
                                ty = TokenType.TypeKeyword;
                        }
                        Token *t = tokenCreate(ident, ty, r, c, fp);
                        lexerAppend(&lexer, t);
                        i += ident.length, c += ident.length;
                } else if (isDigit(ch)) {
                        char[] num = consumeWhile(src[i..$], (dchar c) { return isDigit(c); });
                        Token *t = tokenCreate(num, TokenType.IntLit, r, c, fp);
                        lexerAppend(&lexer, t);
                        i += num.length, c += num.length;
                } else if (ch == '"') {
                        i += 1, r += 1; // "
                        char[] s = consumeWhile(src[i..$], (dchar c) { return c != '"'; });
                        Token *t = tokenCreate(s, TokenType.StrLit, r, c, fp);
                        lexerAppend(&lexer, t);
                        i += s.length + 1, r += s.length + 1; // +1 for "
                } else if (ch == '\'') {
                        assert(0);
                } else {
                        char[] op = consumeWhile(src[i..$], (dchar c) { return isOp(c); });
                        size_t sz = 0;
                        TokenType ty = determineTypeFromOp(op, &sz);
                        Token *t = tokenCreate(src[i..i+sz].dup, ty, r, c, fp);
                        lexerAppend(&lexer, t);
                        i += sz, c += sz;
                }
        }

        Token* t = tokenCreate(src[i..$].dup, TokenType.Eof, r, c, fp);
        lexerAppend(&lexer, t);

        return lexer;
}
