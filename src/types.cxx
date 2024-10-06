#include <cassert>
#include <memory>

#include "lexer.hxx"
#include "types.hxx"
#include "err.hxx"
#include "common.hxx"

scr_type::t::t(scr_type::base base, un_ptr<scr_type::t> next)
    : base(base), next(std::move(next)) {}

str scr_type::to_cxxstr(scr_type::t *ty) {
    if (!ty)
        return "";
    switch (ty->base) {
        case scr_type::base::I32:  return COMMON_SCR_I32  + scr_type::to_cxxstr(ty->next.get());
        case scr_type::base::Str:  return COMMON_SCR_STR  + scr_type::to_cxxstr(ty->next.get());
        case scr_type::base::Void: return COMMON_SCR_VOID + scr_type::to_cxxstr(ty->next.get());
        case scr_type::base::Ptr:  return "Ptr<"          + scr_type::to_cxxstr(ty->next.get()) + ">";
        default: ERRW("invalid base type `%d`", (int)ty->base);
    }
}

void scr_type::to_pointer(scr_type::t *ty) {
    scr_type::base old_base = ty->base;
    un_ptr<scr_type::t> old_next = std::move(ty->next);
    ty->base = scr_type::base::Ptr;
    ty->next = std::make_unique<scr_type::t>(old_base, std::move(old_next));
}

static scr_type::base get_base(token::t *tok) {
    assert(tok->ty == token::type::Type);

    if (tok->lx == COMMON_SCR_I32)  return scr_type::base::I32;
    if (tok->lx == COMMON_SCR_STR)  return scr_type::base::Str;
    if (tok->lx == COMMON_SCR_VOID) return scr_type::base::Void;

    ERRW("unknown base type `%s`", tok->lx.c_str());
    return (scr_type::base)0; // unreachable
}

bool scr_type::is_void(scr_type::t *ty) {
    assert(ty);
    return ty->base == scr_type::base::Void;
}

un_ptr<scr_type::t> scr_type::parse(lexer::t &lexer) {
    auto base_tok = lexer::next(lexer);
    auto base = get_base(base_tok.get());
    auto type = std::make_unique<scr_type::t>(base, nullptr);

    while (true) {
        if (lexer_speek(lexer)->ty == token::type::Asterisk) {
            scr_type::to_pointer(type.get());
            lexer::discard(lexer);
        }
        else if (lexer_speek(lexer)->ty == token::type::LBracket) {
            assert(false && "array type parsing unimplemented");
        }
        else
            break;
    }

    return std::move(type);
}
