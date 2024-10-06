#ifndef COMMON_HXX
#define COMMON_HXX

#define COMMON_SCR_I32 "i32"
#define COMMON_SCR_U8 "u8"
#define COMMON_SCR_STR "str"
#define COMMON_SCR_VOID "void"
#define COMMON_SCR_PRIMITIVE_TYPES {COMMON_SCR_I32, COMMON_SCR_U8, COMMON_SCR_STR, \
                                    COMMON_SCR_VOID}

#define COMMON_SCR_PROC "proc"
#define COMMON_SCR_LET "let"
#define COMMON_SCR_CONST "const"
#define COMMON_SCR_IMPORT "import"
#define COMMON_SCR_EXTERN "extern"
#define COMMON_SCR_FOR "for"
#define COMMON_SCR_WHILE "while"
#define COMMON_SCR_IF "if"
#define COMMON_SCR_ELSE "else"
#define COMMON_SCR_MODULE "module"
#define COMMON_SCR_WHERE "where"
#define COMMON_SCR_KEYWORDS {COMMON_SCR_PROC, COMMON_SCR_LET, \
                             COMMON_SCR_CONST, COMMON_SCR_IMPORT, COMMON_SCR_EXTERN, \
                             COMMON_SCR_FOR, COMMON_SCR_WHILE, COMMON_SCR_IF, \
                             COMMON_SCR_ELSE, COMMON_SCR_MODULE, COMMON_SCR_WHERE}

#endif // COMMON_HXX
