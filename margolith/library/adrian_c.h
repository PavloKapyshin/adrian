#ifndef ADRIAN_C_DEFINED
#define ADRIAN_C_DEFINED

typedef struct {
    int_fast8_t literal;
} IntFast8;

typedef struct {
    int_fast16_t literal;
} IntFast16;

typedef struct {
    int_fast32_t literal;
} IntFast32;

typedef struct {
    int_fast64_t literal;
} IntFast64;

typedef struct {
    uint_fast8_t literal;
} UIntFast8;

typedef struct {
    uint_fast16_t literal;
} UIntFast16;

typedef struct {
    uint_fast32_t literal;
} UIntFast32;

typedef struct {
    uint_fast64_t literal;
} UIntFast64;



extern IntFast8*    IntFast8__init__(int_fast8_t literal);
extern void         IntFast8__deinit__(IntFast8* self);
extern IntFast8*    IntFast8__copy__(IntFast8* self);

extern IntFast16*   IntFast16__init__(int_fast16_t literal);
extern void         IntFast16__deinit__(IntFast16* self);
extern IntFast16*   IntFast16__copy__(IntFast16* self);

extern IntFast32*   IntFast32__init__(int_fast32_t literal);
extern void         IntFast32__deinit__(IntFast32* self);
extern IntFast32*   IntFast32__copy__(IntFast32* self);

extern IntFast64*   IntFast64__init__(int_fast64_t literal);
extern void         IntFast64__deinit__(IntFast64* self);
extern IntFast64*   IntFast64__copy__(IntFast64* self);


extern UIntFast8*   UIntFast8__init__(uint_fast8_t literal);
extern void         UIntFast8__deinit__(UIntFast8* self);
extern UIntFast8*   UIntFast8__copy__(UIntFast8* self);

extern UIntFast16*  UIntFast16__init__(uint_fast16_t literal);
extern void         UIntFast16__deinit__(UIntFast16* self);
extern UIntFast16*  UIntFast16__copy__(UIntFast16* self);

extern UIntFast32*  UIntFast32__init__(uint_fast32_t literal);
extern void         UIntFast32__deinit__(UIntFast32* self);
extern UIntFast32*  UIntFast32__copy__(UIntFast32* self);

extern UIntFast64*  UIntFast64__init__(uint_fast64_t literal);
extern void         UIntFast64__deinit__(UIntFast64* self);
extern UIntFast64*  UIntFast64__copy__(UIntFast64* self);

#endif