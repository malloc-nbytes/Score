#include <stdio.h>
#include <stdlib.h>

#include "utils.hxx"

char *file_to_cstr(const char *filename) {
    FILE *file = fopen(filename, "rb");

    if (!file) {
        perror("Failed to open file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    rewind(file);

    char *buffer = (char *)malloc(length + 1);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return NULL;
    }

    fread(buffer, 1, length, file);
    fclose(file);

    buffer[length] = '\0';
    return buffer;
}
