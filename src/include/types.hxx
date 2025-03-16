#ifndef TYPES_HXX
#define TYPES_HXX

typedef enum {
        SCR_BASE_TYPE_I8 = 0,
        SCR_BASE_TYPE_I16,
        SCR_BASE_TYPE_I32,
        SCR_BASE_TYPE_I64,

        SCR_BASE_TYPE_U8,
        SCR_BASE_TYPE_U16,
        SCR_BASE_TYPE_U32,
        SCR_BASE_TYPE_U64,

        SCR_BASE_TYPE_STR,
        SCR_BASE_TYPE_VOID,

        SCR_BASE_TYPE_PTR,

        SCR_BASE_TYPE_CUSTOM,
} Scr_Base_Type;

typedef struct Scr_Type {
        Scr_Base_Type base;
        Scr_Type *ptrn;
        char *custom_name;

        // Scr_Type(void);
        // Scr_Type(const Scr_Type &other);
        // Scr_Type &operator=(const Scr_Type &other);
        // ~Scr_Type(void);
} Scr_Type;

void scr_type_dump(Scr_Type *ty, bool newline = true);

#endif // TYPES_HXX
