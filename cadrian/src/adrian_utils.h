#ifndef ADRIAN_UTILS_INCLUDED
#define ADRIAN_UTILS_INCLUDED


typedef struct {
    size_t size;
    char* data;
} String;


typedef struct {
    size_t index;
    String* string;
} StringIterator;


extern String* String_new(size_t size, char* data);
extern String* String_copy(String* source);
extern StringIterator* String_iter(String* self);
extern void String_free(String* self);

extern StringIterator* StringIterator_new(size_t index, String* string);
extern char StringIterator_next(StringIterator* self);
extern void StringIterator_free(StringIterator* self);

extern void* safe_malloc(size_t size);

#endif
