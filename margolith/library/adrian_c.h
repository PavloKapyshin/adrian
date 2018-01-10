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



extern IntFast8*    IntFast8___init__(int_fast8_t literal);
extern void         IntFast8___deinit__(IntFast8* self);
extern IntFast8*    IntFast8___copy__(IntFast8* self);

extern IntFast16*   IntFast16___init__(int_fast16_t literal);
extern void         IntFast16___deinit__(IntFast16* self);
extern IntFast16*   IntFast16___copy__(IntFast16* self);

extern IntFast32*   IntFast32___init__(int_fast32_t literal);
extern void         IntFast32___deinit__(IntFast32* self);
extern IntFast32*   IntFast32___copy__(IntFast32* self);

extern IntFast64*   IntFast64___init__(int_fast64_t literal);
extern void         IntFast64___deinit__(IntFast64* self);
extern IntFast64*   IntFast64___copy__(IntFast64* self);


extern UIntFast8*   UIntFast8___init__(uint_fast8_t literal);
extern void         UIntFast8___deinit__(UIntFast8* self);
extern UIntFast8*   UIntFast8___copy__(UIntFast8* self);

extern UIntFast16*  UIntFast16___init__(uint_fast16_t literal);
extern void         UIntFast16___deinit__(UIntFast16* self);
extern UIntFast16*  UIntFast16___copy__(UIntFast16* self);

extern UIntFast32*  UIntFast32___init__(uint_fast32_t literal);
extern void         UIntFast32___deinit__(UIntFast32* self);
extern UIntFast32*  UIntFast32___copy__(UIntFast32* self);

extern UIntFast64*  UIntFast64___init__(uint_fast64_t literal);
extern void         UIntFast64___deinit__(UIntFast64* self);
extern UIntFast64*  UIntFast64___copy__(UIntFast64* self);

#endif