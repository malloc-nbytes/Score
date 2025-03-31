module token;

import std.stdio;
import std.conv;

import utils;

enum TokenType {
        Ident,
        StrLit,
        IntLit,
        Keyword,
        TypeKeyword,
        Eof,
        Lparen,
        Rparen,
        LCurlyBracket,
        RCurlyBracket,
        LSquareBracket,
        RSquareBracket,
        Colon,
        SemiColon,
        Period,
        Bang,
        Lessthan,
        Greaterthan,
        LessthanEquals,
        GreaterthanEquals,
        PlusEquals,
        MinusEquals,
        ForwardSlashEquals,
        PercentEquals,
        AsteriskEquals,
        Asterisk,
        ForwardSlash,
        Percent,
        Ampersand,
        Pipe,
        Uptick,
        Tilde,
        At,
        QuestionMark,
        DoubleAmpersand,
        DoublePipe,
        Equals,
        DoubleEquals,
        BangEquals,
        AmpersandEquals,
        PipeEquals,
        CaretEquals,
        Plus,
        Minus,
        Comma,
        TriplePeriod,
}

struct Token {
        char[] lx;
        TokenType ty;
        size_t r, c;
        string fp;
        Token *next;
}

Token* tokenCreate(char[] lx, TokenType ty, size_t r, size_t c, string fp) {
        // char[] result;

        // for (size_t i = 0; i < lx.length; ++i) {
        //         if (i < lx.length - 1 && lx[i] == '\\') {
        //                 if (lx[i + 1] == 'n') {
        //                         result ~= '\n';
        //                         i++;
        //                 } else if (lx[i + 1] == '\\') {
        //                         result ~= '\\';
        //                         i++;
        //                 } else {
        //                         err("unsupported escape sequence: '"~lx[i]~lx[i + 1]~"'");
        //                         result ~= lx[i];
        //                 }
        //         } else {
        //                 result ~= lx[i];
        //         }
        // }

        Token* t = new Token;
        // t.lx = result;
        t.lx = lx;
        t.ty = ty;
        t.r = r;
        t.c = c;
        t.fp = fp;
        t.next = null;
        return t;
}

string tokenTypeToStr(TokenType ty) {
        switch (ty) {
        case TokenType.Ident: return "Ident";
        case TokenType.StrLit: return "StrLit";
        case TokenType.IntLit: return "IntLit";
        case TokenType.Keyword: return "Keyword";
        case TokenType.TypeKeyword: return "TypeKeyword";
        case TokenType.Eof: return "Eof";
        case TokenType.Lparen: return "Lparen";
        case TokenType.Rparen: return "Rparen";
        case TokenType.LCurlyBracket: return "LCurlyBracket";
        case TokenType.RCurlyBracket: return "RCurlyBracket";
        case TokenType.LSquareBracket: return "LSquareBracket";
        case TokenType.RSquareBracket: return "RSquareBracket";
        case TokenType.Colon: return "Colon";
        case TokenType.SemiColon: return "SemiColon";
        case TokenType.Period: return "Period";
        case TokenType.Bang: return "Bang";
        case TokenType.Lessthan: return "Lessthan";
        case TokenType.Greaterthan: return "Greaterthan";
        case TokenType.LessthanEquals: return "LessthanEquals";
        case TokenType.GreaterthanEquals: return "GreaterthanEquals";
        case TokenType.PlusEquals: return "PlusEquals";
        case TokenType.MinusEquals: return "MinusEquals";
        case TokenType.ForwardSlashEquals: return "ForwardSlashEquals";
        case TokenType.PercentEquals: return "PercentEquals";
        case TokenType.AsteriskEquals: return "AsteriskEquals";
        case TokenType.Asterisk: return "Asterisk";
        case TokenType.ForwardSlash: return "ForwardSlash";
        case TokenType.Percent: return "Percent";
        case TokenType.Ampersand: return "Ampersand";
        case TokenType.Pipe: return "Pipe";
        case TokenType.Uptick: return "Uptick";
        case TokenType.Tilde: return "Tilde";
        case TokenType.At: return "At";
        case TokenType.QuestionMark: return "QuestionMark";
        case TokenType.DoubleAmpersand: return "DoubleAmpersand";
        case TokenType.DoublePipe: return "DoublePipe";
        case TokenType.Equals: return "Equals";
        case TokenType.DoubleEquals: return "DoubleEquals";
        case TokenType.BangEquals: return "BangEquals";
        case TokenType.AmpersandEquals: return "AmpersandEquals";
        case TokenType.PipeEquals: return "PipeEquals";
        case TokenType.CaretEquals: return "CaretEquals";
        case TokenType.Plus: return "Plus";
        case TokenType.Minus: return "Minus";
        case TokenType.Comma: return "Comma";
        case TokenType.TriplePeriod: return "TriplePeriod";
        default: assert(0);
        }
}

void tokerr(const Token* t) {
        write(t.fp, ":", t.r, ":", t.c, ": ");
}

string tokerrToStr(const Token* t) {
        return t.fp ~ ":" ~ t.r.to!string ~ ":" ~ t.c.to!string ~ ": ";
}

