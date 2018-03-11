#ifndef ADRIAN_C_DEFINED
#define ADRIAN_C_DEFINED

struct IntFast8 {
    int_fast8_t literal;
};

struct IntFast16 {
    int_fast16_t literal;
};

struct IntFast32 {
    int_fast32_t literal;
};

struct IntFast64 {
    int_fast64_t literal;
};

struct UIntFast8 {
    uint_fast8_t literal;
};

struct UIntFast16 {
    uint_fast16_t literal;
};

struct UIntFast32 {
    uint_fast32_t literal;
};

struct UIntFast64 {
    uint_fast64_t literal;
};



extern struct IntFast8*    IntFast8___init__(int_fast8_t literal);
extern void                IntFast8___deinit__(struct IntFast8* self);
extern struct IntFast8*    IntFast8___copy__(struct IntFast8* self);
extern struct IntFast8*    IntFast8___add__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8*    IntFast8___sub__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8*    IntFast8___mul__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8*    IntFast8___div__(struct IntFast8* self, struct IntFast8* src);

extern struct IntFast16*   IntFast16___init__(int_fast16_t literal);
extern void                IntFast16___deinit__(struct IntFast16* self);
extern struct IntFast16*   IntFast16___copy__(struct IntFast16* self);
extern struct IntFast16*   IntFast16___add__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16*   IntFast16___sub__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16*   IntFast16___mul__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16*   IntFast16___div__(struct IntFast16* self, struct IntFast16* src);

extern struct IntFast32*   IntFast32___init__(int_fast32_t literal);
extern void                IntFast32___deinit__(struct IntFast32* self);
extern struct IntFast32*   IntFast32___copy__(struct IntFast32* self);
extern struct IntFast32*   IntFast32___add__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32*   IntFast32___sub__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32*   IntFast32___mul__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32*   IntFast32___div__(struct IntFast32* self, struct IntFast32* src);

extern struct IntFast64*   IntFast64___init__(int_fast64_t literal);
extern void                IntFast64___deinit__(struct IntFast64* self);
extern struct IntFast64*   IntFast64___copy__(struct IntFast64* self);
extern struct IntFast64*   IntFast64___add__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64*   IntFast64___sub__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64*   IntFast64___mul__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64*   IntFast64___div__(struct IntFast64* self, struct IntFast64* src);


extern struct UIntFast8*   UIntFast8___init__(uint_fast8_t literal);
extern void                UIntFast8___deinit__(struct UIntFast8* self);
extern struct UIntFast8*   UIntFast8___copy__(struct UIntFast8* self);
extern struct UIntFast8*   UIntFast8___add__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8*   UIntFast8___sub__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8*   UIntFast8___mul__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8*   UIntFast8___div__(struct UIntFast8* self, struct UIntFast8* src);

extern struct UIntFast16*  UIntFast16___init__(uint_fast16_t literal);
extern void                UIntFast16___deinit__(struct UIntFast16* self);
extern struct UIntFast16*  UIntFast16___copy__(struct UIntFast16* self);
extern struct UIntFast16*  UIntFast16___add__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16*  UIntFast16___sub__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16*  UIntFast16___mul__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16*  UIntFast16___div__(struct UIntFast16* self, struct UIntFast16* src);

extern struct UIntFast32*  UIntFast32___init__(uint_fast32_t literal);
extern void                UIntFast32___deinit__(struct UIntFast32* self);
extern struct UIntFast32*  UIntFast32___copy__(struct UIntFast32* self);
extern struct UIntFast32*  UIntFast32___add__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32*  UIntFast32___sub__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32*  UIntFast32___mul__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32*  UIntFast32___div__(struct UIntFast32* self, struct UIntFast32* src);

extern struct UIntFast64*  UIntFast64___init__(uint_fast64_t literal);
extern void                UIntFast64___deinit__(struct UIntFast64* self);
extern struct UIntFast64*  UIntFast64___copy__(struct UIntFast64* self);
extern struct UIntFast64*  UIntFast64___add__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64*  UIntFast64___sub__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64*  UIntFast64___mul__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64*  UIntFast64___div__(struct UIntFast64* self, struct UIntFast64* src);

#endif