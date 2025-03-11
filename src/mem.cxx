#include <stdio.h>
#include <stdlib.h>

#include "mem.hxx"
#include "err.hxx"

void *s_malloc(size_t b) {
        void *p = malloc(b);
        if (!p) {
                err_wargs("could not alloc %zu bytes", b);
        }
        return p;
}
