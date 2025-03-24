module keywords;

static const string[] typeKeywords = [
        "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "usize",
        "void",
];

static const string[] keywords = [
        "proc",
        "if",
        "return",
        "let",
        "else",
        "extern",
        "while",
        "export",
];

enum Keyword {
        Proc = keywords[0],
        If = keywords[1],
        Return = keywords[2],
        Let = keywords[3],
        Else = keywords[4],
        Extern = keywords[5],
        While = keywords[6],
        Export = keywords[7],
}

enum TypeKeyword {
        I8 = typeKeywords[0],
        I16 = typeKeywords[1],
        I32 = typeKeywords[2],
        I64 = typeKeywords[3],
        U8 = typeKeywords[4],
        U16 = typeKeywords[5],
        U32 = typeKeywords[6],
        U64 = typeKeywords[7],
        Usize = typeKeywords[8],
        Void = typeKeywords[9],
}

bool isKeyword(const char[] s) {
        foreach (const ref string kw; keywords) {
                if (kw == s) {
                        return true;
                }
        }
        return false;
}

bool isTypeKeyword(const char[] s) {
        foreach (const ref string t; typeKeywords) {
                if (t == s) {
                        return true;
                }
        }
        return false;
}
