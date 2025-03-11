#include <stdio.h>
#include <stdlib.h>

#include "types.hxx"
#include "utils.hxx"
#include "err.hxx"

static const char *scr_base_type_to_cstr(Scr_Base_Type ty) {
        switch (ty) {
        case SCR_BASE_TYPE_I8: return "i8";
        case SCR_BASE_TYPE_I16: return "i16";
        case SCR_BASE_TYPE_I32: return "i32";
        case SCR_BASE_TYPE_I64: return "i64";
        case SCR_BASE_TYPE_U8: return "u8";
        case SCR_BASE_TYPE_U16: return "u16";
        case SCR_BASE_TYPE_U32: return "u32";
        case SCR_BASE_TYPE_U64: return "u64";
        case SCR_BASE_TYPE_STR: return "str";
        case SCR_BASE_TYPE_VOID: return "void";
        case SCR_BASE_TYPE_PTR: return "ptr";
        default: {
                err_wargs("unkown type %d", (int)ty);
        } break;
        }
}

Scr_Type::Scr_Type(void) {
        this->base = (Scr_Base_Type)0;
        this->ptrn = nullptr;
}

Scr_Type::Scr_Type(const Scr_Type &other) : base(other.base), ptrn(nullptr) {
    if (other.ptrn) {
        ptrn = (Scr_Type *)malloc(sizeof(Scr_Type));
        *ptrn = *other.ptrn;
        Scr_Type *current = ptrn;
        Scr_Type *other_current = other.ptrn->ptrn;

        while (other_current) {
            current->ptrn = (Scr_Type *)malloc(sizeof(Scr_Type));
            *current->ptrn = *other_current;
            current = current->ptrn;
            other_current = other_current->ptrn;
        }
    }
}

Scr_Type &Scr_Type::operator=(const Scr_Type &other) {
    if (this != &other) {
        this->~Scr_Type();

        base = other.base;
        ptrn = nullptr;

        if (other.ptrn) {
            ptrn = (Scr_Type *)malloc(sizeof(Scr_Type));
            *ptrn = *other.ptrn;
            Scr_Type *current = ptrn;
            Scr_Type *other_current = other.ptrn->ptrn;

            while (other_current) {
                current->ptrn = (Scr_Type *)malloc(sizeof(Scr_Type));
                *current->ptrn = *other_current;
                current = current->ptrn;
                other_current = other_current->ptrn;
            }
        }
    }
    return *this;
}

Scr_Type::~Scr_Type(void) {
        auto it = this->ptrn;
        while (it) {
                auto tmp = it->ptrn;
                free(it);
                it = tmp;
        }
}

void scr_type_dump(Scr_Type *ty, bool newline) {
        if (ty->base == SCR_BASE_TYPE_PTR) {
                printf("Ptr<");
                scr_type_dump(ty->ptrn, newline);
                printf(">");
        } else {
                printf("%s", scr_base_type_to_cstr(ty->base));
        }
        if (newline) putchar('\n');
}
