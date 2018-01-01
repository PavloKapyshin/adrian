#include <stdint.h>
#include <stdlib.h>
#include <adrian_c.h>


IntFast8* IntFast8__init__(int_fast8_t literal) {
    IntFast8* self = malloc(sizeof(IntFast8));
    self->literal = literal;
    return self;
}

void IntFast8__deinit__(IntFast8* self) {
    free(self);
}

IntFast8* IntFast8__copy__(IntFast8* self) {
    IntFast8* new = malloc(sizeof(IntFast8));
    new->literal = self->literal;
    return new;
}

IntFast16* IntFast16__init__(int_fast16_t literal) {
    IntFast16* self = malloc(sizeof(IntFast16));
    self->literal = literal;
    return self;
}

void IntFast16__deinit__(IntFast16* self) {
    free(self);
}

IntFast16* IntFast16__copy__(IntFast16* self) {
    IntFast16* new = malloc(sizeof(IntFast16));
    new->literal = self->literal;
    return new;
}

IntFast32* IntFast32__init__(int_fast32_t literal) {
    IntFast32* self = malloc(sizeof(IntFast32));
    self->literal = literal;
    return self;
}

void IntFast32__deinit__(IntFast32* self) {
    free(self);
}

IntFast32* IntFast32__copy__(IntFast32* self) {
    IntFast32* new = malloc(sizeof(IntFast32));
    new->literal = self->literal;
    return new;
}

IntFast64* IntFast64__init__(int_fast64_t literal) {
    IntFast64* self = malloc(sizeof(IntFast64));
    self->literal = literal;
    return self;
}

void IntFast64__deinit__(IntFast64* self) {
    free(self);
}

IntFast64* IntFast64__copy__(IntFast64* self) {
    IntFast64* new = malloc(sizeof(IntFast64));
    new->literal = self->literal;
    return new;
}

UIntFast8* UIntFast8__init__(uint_fast8_t literal) {
    UIntFast8* self = malloc(sizeof(UIntFast8));
    self->literal = literal;
    return self;
}

void UIntFast8__deinit__(UIntFast8* self) {
    free(self);
}

UIntFast8* UIntFast8__copy__(UIntFast8* self) {
    UIntFast8* new = malloc(sizeof(UIntFast8));
    new->literal = self->literal;
    return new;
}

UIntFast16* UIntFast16__init__(uint_fast16_t literal) {
    UIntFast16* self = malloc(sizeof(UIntFast16));
    self->literal = literal;
    return self;
}

void UIntFast16__deinit__(UIntFast16* self) {
    free(self);
}

UIntFast16* UIntFast16__copy__(UIntFast16* self) {
    UIntFast16* new = malloc(sizeof(UIntFast16));
    new->literal = self->literal;
    return new;
}

UIntFast32* UIntFast32__init__(uint_fast32_t literal) {
    UIntFast32* self = malloc(sizeof(UIntFast32));
    self->literal = literal;
    return self;
}

void UIntFast32__deinit__(UIntFast32* self) {
    free(self);
}

UIntFast32* UIntFast32__copy__(UIntFast32* self) {
    UIntFast32* new = malloc(sizeof(UIntFast32));
    new->literal = self->literal;
    return new;
}

UIntFast64* UIntFast64__init__(uint_fast64_t literal) {
    UIntFast64* self = malloc(sizeof(UIntFast64));
    self->literal = literal;
    return self;
}

void UIntFast64__deinit__(UIntFast64* self) {
    free(self);
}

UIntFast64* UIntFast64__copy__(UIntFast64* self) {
    UIntFast64* new = malloc(sizeof(UIntFast64));
    new->literal = self->literal;
    return new;
}
