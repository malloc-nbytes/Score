#include <iostream>

#include "err.hxx"
#include "token.hxx"

void err::wtok(token::t *tok) {
    if (!tok)
        return;
    std::cerr << tok->fp << ':' << tok->row << ':' << tok->col << ":";
}
