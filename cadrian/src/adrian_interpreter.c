#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "utarray.h"
#include "adrian_utils.h"
#include "adrian_ast.h"
#include "adrian_parser.h"

#define CHUNK_SIZE 1000*1000*100


String* read_file(FILE* file) {
    char* data = safe_malloc(sizeof(char) * CHUNK_SIZE);
    size_t already_read = 0;
    char c;
    while (((c = fgetc(file)) != EOF) && (already_read < CHUNK_SIZE)) {
        data[already_read++] = c;
    }
    char* res = safe_malloc(sizeof(already_read-1));
    memmove(res, data, already_read-1);
    free(data);
    return String_new(already_read-1, res);
}


int main(int argc, char** argv) {
    if (argc < 2) {
        puts("ERROR: pass Adrian source file path.");
        exit(EXIT_FAILURE);
    }

    char* source_file_path = argv[1];
    FILE* source_file = fopen(source_file_path, "r");
    String* source_code = read_file(source_file);
    UT_array* result = parse(String_iter(source_code));

    void* p = NULL;
    while ((p = (void*)utarray_next(result, p))) {
        printf(
            "GOT stmt (let-declaration): name = %s, expr = %s\n",
            ((Node*)p)->let_declaration->name,
            ((Node*)p)->let_declaration->expr->literal->text);
    }

    String_free(source_code);
    utarray_free(result);
    fclose(source_file);
    return 0;
}
