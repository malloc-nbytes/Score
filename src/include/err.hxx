#ifndef ERR_HXX
#define ERR_HXX

#include <stdio.h>

#include "token.hxx"

#define ERRW(msg, ...)                                  \
    do {                                                \
        fprintf(stderr, "error: " msg, __VA_ARGS__);    \
        fprintf(stderr, "\n");                          \
        std::exit(EXIT_FAILURE);                        \
    } while (0)

#define ERR(msg)                                \
    do {                                        \
        fprintf(stderr, "error: " msg);         \
        fprintf(stderr, "\n");                  \
        std::exit(EXIT_FAILURE);                \
    } while (0)

namespace err {
    // namespace token{struct t;}

    void wtok(token::t *tok);
};

#endif // ERR_HXX
