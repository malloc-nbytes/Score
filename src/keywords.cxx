#include <string.h>

#include "keywords.hxx"

#include <stdio.h>

bool is_keyword(const char *s) {
        const char *keywords[] = KEYWORD_AS_CPL;
        for (size_t i = 0; i < sizeof(keywords) / sizeof(*keywords); ++i) {
                if (!strcmp(s, keywords[i])) {
                        return true;
                }
        }
        return false;
}

bool is_type(const char *s) {
        const char *types[] = PRIMITIVE_AS_CPL;
        for (size_t i = 0; i < sizeof(types) / sizeof(*types); ++i) {
                if (!strcmp(s, types[i])) {
                        return true;
                }
        }
        return false;
}
