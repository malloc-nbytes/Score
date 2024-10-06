#ifndef TYPES_HXX
#define TYPES_HXX

#include "lexer.hxx"
#include "utils.hxx"

namespace scr_type {
    enum class base {
        I32,
        Str,
        Void,
        Ptr,
    };

    struct t {
        scr_type::base base;
        un_ptr<scr_type::t> next;
        t(scr_type::base base, un_ptr<scr_type::t> next);
    };

    un_ptr<scr_type::t> parse(lexer::t &lexer);
    bool is_void(scr_type::t *ty);
    void to_pointer(scr_type::t *ty);
    str to_cxxstr(scr_type::t *ty);
}

#endif // TYPES_HXX
