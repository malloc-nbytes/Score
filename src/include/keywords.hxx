#ifndef KEYWORDS_HXX
#define KEYWORDS_HXX

#define KEYWORD_LET "let"
#define KEYWORD_IF "if"
#define KEYWORD_PROC "proc"

#define KEYWORD_AS_CPL {                        \
                KEYWORD_LET,                    \
                KEYWORD_IF,                     \
                KEYWORD_PROC,                   \
}

#define PRIMITIVE_I8 "i8"
#define PRIMITIVE_I16 "i16"
#define PRIMITIVE_I32 "i32"
#define PRIMITIVE_I64 "i64"
#define PRIMITIVE_U8 "u8"
#define PRIMITIVE_U16 "u16"
#define PRIMITIVE_U32 "u32"
#define PRIMITIVE_U64 "u64"
#define PRIMITIVE_STR "str"
#define PRIMITIVE_VOID "void"

#define PRIMITIVE_AS_CPL {                      \
                PRIMITIVE_I8,                   \
                PRIMITIVE_I16,                  \
                PRIMITIVE_I32,                  \
                PRIMITIVE_I64,                  \
                PRIMITIVE_U8,                   \
                PRIMITIVE_U16,                  \
                PRIMITIVE_U32,                  \
                PRIMITIVE_U64,                  \
                PRIMITIVE_STR,                  \
                PRIMITIVE_VOID,                 \
}

bool is_keyword(const char *s);
bool is_type(const char *s);

#endif // KEYWORDS_HXX
