#ifndef ERR_HXX
#define ERR_HXX

#include <stdio.h>

#define ERRW(msg, ...) \
    do { \
        fprintf(stderr, "error: " msg, __VA_ARGS__); \
        std::exit(EXIT_FAILURE); \
    } while (0)

#define ERR(msg) \
    do { \
        fprintf(stderr, "error: " msg); \
        std::exit(EXIT_FAILURE); \
    } while (0)

#endif // ERR_HXX