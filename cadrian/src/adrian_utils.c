#include <stdio.h>
#include <stdlib.h>
#include "adrian_utils.h"


void* safe_malloc(size_t size) {
    void* result = malloc(size);
    if (result == NULL) {
        puts("Not enough memory");
        exit(EXIT_FAILURE);
    }
    return result;
}
