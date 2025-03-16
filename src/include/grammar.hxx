#ifndef GRAMMAR_HXX
#define GRAMMAR_HXX

#include "token.hxx"
#include "types.hxx"
#include "ds/array.hxx"

#define IS_TOPLVL_STMT(s) \
        ((s).ty == STMT_TYPE_LET \
         || (s).ty == STMT_TYPE_PROC \
         || (s).ty == STMT_TYPE_DEF \
         || (s).ty == STMT_TYPE_STRUCT)

typedef enum {
        EXPR_TYPE_BIN = 0,
        EXPR_TYPE_UNARY,
        EXPR_TYPE_IDENT,
        EXPR_TYPE_STR_LIT,
        EXPR_TYPE_INT_LIT,
        EXPR_TYPE_PROC_CALL,
        EXPR_TYPE_MUT,
        EXPR_TYPE_STRUCT_INST,
        EXPR_TYPE_GET,
} Expr_Type;

typedef enum {
        STMT_TYPE_EXPR = 0,
        STMT_TYPE_LET,
        STMT_TYPE_PROC,
        STMT_TYPE_BLOCK,
        STMT_TYPE_RETURN,
        STMT_TYPE_DEF,
        STMT_TYPE_IF,
        STMT_TYPE_EMPTY,
        STMT_TYPE_WHILE,
        STMT_TYPE_FOR,
        STMT_TYPE_STRUCT,
} Stmt_Type;

typedef struct Expr_Get Expr_Get;
typedef struct Expr_Struct_Inst Expr_Struct_Inst;
typedef struct Expr_Mut Expr_Mut;
typedef struct Expr_Proc_Call Expr_Proc_Call;
typedef struct Expr_Mut Expr_Mut;
typedef struct Expr_Ident Expr_Ident;
typedef struct Expr_Str_Lit Expr_Str_Lit;
typedef struct Expr_Int_Lit Expr_Int_Lit;
typedef struct Expr_Term Expr_Term;
typedef struct Expr_Un Expr_Un;
typedef struct Expr_Bin Expr_Bin;
typedef struct Expr Expr;

typedef struct Stmt_Struct Stmt_Struct;
typedef struct Stmt_For Stmt_For;
typedef struct Stmt_While Stmt_While;
typedef struct Stmt_If Stmt_If;
typedef struct Stmt_Def Stmt_Def;
typedef struct Stmt_Return Stmt_Return;
typedef struct Stmt_Proc Stmt_Proc;
typedef struct Stmt_Block Stmt_Block;
typedef struct Stmt_Let Stmt_Let;
typedef struct Stmt Stmt;

///////////////////
// Expressions ////
///////////////////

typedef struct Expr {
        Expr_Type ty;
} Expr;

typedef struct Expr_Get {
        Expr base;
        Expr *l;
        Expr *r;
} Expr_Get;

typedef struct Expr_Struct_Inst {
        Expr base;
        Token *struct_name;
        Token **ids;
        Expr **exprs;
        size_t len, cap;
} Expr_Struct_Inst;

typedef struct Expr_Proc_Call {
        Expr base;
        Expr *left;
        struct {
                Expr **exprs;
                size_t len, cap;
        } args;
} Expr_Proc_Call;

typedef struct Expr_Mut {
        Expr base;
        Expr *l;
        Token *op;
        Expr *r;
} Expr_Mut;

typedef struct Expr_Ident {
        Expr base;
        Token *id;
} Expr_Ident;

typedef struct Expr_Str_Lit {
        Expr base;
        Token *s;
} Expr_Str_Lit;

typedef struct Expr_Int_Lit {
        Expr base;
        int i;
} Expr_Int_Lit;

typedef struct Expr_Un {
        Expr base;
        Token *op;
        Expr *e;
} Expr_Un;

typedef struct Expr_Bin {
        Expr base;
        Expr *l, *r;
        Token *op;
} Expr_Bin;

Expr_Get *expr_get_alloc(Expr *l, Expr *r);
Expr_Struct_Inst *expr_struct_inst(Token *struct_name, Token **ids, Expr **exprs, size_t len, size_t cap);
Expr_Mut *expr_mut_alloc(Expr *l, Token *op, Expr *r);
Expr_Proc_Call *expr_proc_call_alloc(Expr *left, Expr **exprs, size_t len, size_t cap);
Expr_Ident *expr_ident_alloc(Token *id);
Expr_Str_Lit *expr_str_lit_alloc(Token *s);
Expr_Int_Lit *expr_int_lit_alloc(Token *i);
Expr_Bin *expr_bin_alloc(Expr *l, Token *op, Expr *r);

///////////////////
// Statements /////
///////////////////

typedef struct Stmt {
        Stmt_Type ty;
} Stmt;

typedef struct Stmt_Struct {
        Stmt base;
        Token *id;
        struct {
                Token **ids;
                Scr_Type **types;
                size_t len, cap;
        } fields;
} Stmt_Struct;

typedef struct Stmt_For {
        Stmt base;
        Stmt *init;
        Expr *cond;
        Expr *end;
        Stmt *body;
} Stmt_For;

typedef struct Stmt_While {
        Stmt base;
        Expr *e;
        Stmt *s;
} Stmt_While;

typedef struct {
        Stmt base;
} Stmt_Empty;

typedef struct Stmt_Block {
        Stmt base;
        Stmt **stmts;
        size_t len, cap;
} Stmt_Block;

typedef struct Stmt_If {
        Stmt base;
        Expr *e;
        Stmt *then;
        Stmt *else_; // (optional) can be NULL.
} Stmt_If;

typedef struct Stmt_Def {
        Stmt base;
        Stmt_Proc *proto;
} Stmt_Def;

typedef struct Stmt_Return {
        Stmt base;
        Expr *e;
} Stmt_Return;

typedef struct Stmt_Proc {
        Stmt base;
        Token *id;
        struct {
                Token **ids;
                Scr_Type **types;
                size_t len, cap;
        } args;
        Scr_Type *rtype;
        Stmt_Block *block;
        bool variadic;
} Stmt_Proc;

typedef struct Stmt_Let {
        Stmt base;
        Token *id;
        Scr_Type *type;
        Expr *e;
} Stmt_Let;

typedef struct Stmt_Expr {
        Stmt base;
        Expr *e;
} Stmt_Expr;

typedef struct {
        Stmt **stmts;
        size_t len, cap;
} Program;

Stmt_Struct *stmt_struct_alloc(Token *id, Token **ids, Scr_Type **types, size_t len, size_t cap);
Stmt_For *stmt_for_alloc(Stmt *init, Expr *cond, Expr *end, Stmt *body);
Stmt_While *stmt_while_alloc(Expr *e, Stmt *s);
Stmt_Empty *stmt_empty_alloc(void);
Stmt_If *stmt_if_alloc(Expr *e, Stmt *then, Stmt *else_);
Stmt_Def *stmt_def_alloc(Stmt_Proc *proto);
Stmt_Return *stmt_return_alloc(Expr *e);
Stmt_Expr *stmt_expr_alloc(Expr *e);
Stmt_Block *stmt_block_alloc(Stmt **stmts, size_t len, size_t cap);
Stmt_Proc *stmt_proc_alloc(Token *id,
                           Token **ids,
                           Scr_Type **id_types,
                           size_t len,
                           size_t cap,
                           Scr_Type *rtype,
                           Stmt_Block *block,
                           bool variadic);
Stmt_Let *stmt_let_alloc(Token *id, Scr_Type *type, Expr *e);

void program_dump(Program *p);

#endif // GRAMMAR_HXX
