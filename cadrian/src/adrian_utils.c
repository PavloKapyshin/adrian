#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "adrian_utils.h"


String* String_new(size_t size, char* data) {
    String* result = safe_malloc(sizeof(String));
    result->size = size;
    result->data = data;
    return result;
}


String* String_copy(String* source) {
    String* dest = safe_malloc(sizeof(String));
    dest->size = source->size;
    dest->data = safe_malloc(source->size * sizeof(char));
    memmove(dest->data, source->data, source->size);
    return dest;
}


StringIterator* String_iter(String* self) {
    StringIterator* iterator = StringIterator_new(
        0, String_copy(self));
    return iterator;
}


void String_free(String* self) {
    free(self->data);
    free(self);
}


StringIterator* StringIterator_new(size_t index, String* string) {
    StringIterator* result = safe_malloc(sizeof(StringIterator));
    result->index = index;
    result->string = string;
    return result;
}


char StringIterator_next(StringIterator* self) {
    if (self->index <= self->string->size) {
        char result = self->string->data[self->index];
        self->index += 1;
        return result;
    }
    return 0;
}


void StringIterator_free(StringIterator* self) {
    free(self->string);
    free(self);
}


void* safe_malloc(size_t size) {
    void* result = malloc(size);
    if (result == NULL) {
        puts("ERROR: out of memory.");
        exit(EXIT_FAILURE);
    }
    return result;
}
