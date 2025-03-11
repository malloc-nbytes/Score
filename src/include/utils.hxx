#ifndef UTILS_HXX
#define UTILS_HXX

/*
#define da_append(arr, len, cap, ty, value)                       \
    do {                                                          \
         if ((len) >= (cap)) {                                     \
             (cap) = !(cap) ? 2 : (cap) * 2;                       \
             (arr) = (ty)realloc((arr), (cap) * sizeof((arr)[0])); \
         }                                                         \
         (arr)[(len)] = (value);                                   \
         (len) += 1;                                               \
     } while (0)
*/

/*
#define da_append(arr, len, cap, ty, value)                       \
   do {                                                          \
       if ((len) >= (cap)) {                                      \
           (cap) = !(cap) ? 2 : (cap) * 2;                        \
           ty *tmp = new ty[(cap)];                               \
           for (size_t i = 0; i < (len); ++i) {                   \
               new (&tmp[i]) ty((arr)[i]);                        \
               (arr)[i].~ty();                                    \
           }                                                      \
           delete[] (arr);                                        \
           (arr) = tmp;                                           \
       }                                                         \
       new (&(arr)[(len)]) ty(value);                            \
       (len) += 1;                                               \
   } while (0)
*/

#define da_append(arr, len, cap, ty, value)                       \
        do { \
                if ((len) >= (cap)) { \
                        (cap) = !(cap) ? 2 : (cap) * 2; \
                        ty *__tmp = new ty[(cap)]; \
                        for (size_t __i = 0; __i < (len); ++__i) { \
                                __tmp[__i] = (arr)[__i];             \
                        } \
                        if ((arr)) delete[] (arr); \
                        (arr) = __tmp; \
                } \
                (arr)[(len)] = (value); \
                (len) += 1; \
        } while (0)

char *file_to_cstr(const char *filename);

#endif // UTILS_HXX
