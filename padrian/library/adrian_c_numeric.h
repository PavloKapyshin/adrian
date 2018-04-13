#ifndef ADRIAN_C_DEFINED
#define ADRIAN_C_DEFINED

struct IntFast8 {
    int_fast8_t literal;
    uint_fast64_t type_tag;
};


extern struct IntFast8* IntFast8__init__(int_fast8_t literal);
extern void IntFast8__deinit__(struct IntFast8* self);
extern struct IntFast8* IntFast8__copy__(struct IntFast8* self);
extern struct IntFast8* IntFast8__add__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__sub__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__mul__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__div__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__eq__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__neq__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__gte__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__lte__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__gt__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__lt__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__not__(struct IntFast8* self);
extern struct IntFast8* IntFast8__and__(struct IntFast8* self, struct IntFast8* src);
extern struct IntFast8* IntFast8__or__(struct IntFast8* self, struct IntFast8* src);

struct IntFast16 {
    int_fast16_t literal;
    uint_fast64_t type_tag;
};


extern struct IntFast16* IntFast16__init__(int_fast16_t literal);
extern void IntFast16__deinit__(struct IntFast16* self);
extern struct IntFast16* IntFast16__copy__(struct IntFast16* self);
extern struct IntFast16* IntFast16__add__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16* IntFast16__sub__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16* IntFast16__mul__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast16* IntFast16__div__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__eq__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__neq__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__gte__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__lte__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__gt__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__lt__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__not__(struct IntFast16* self);
extern struct IntFast8* IntFast16__and__(struct IntFast16* self, struct IntFast16* src);
extern struct IntFast8* IntFast16__or__(struct IntFast16* self, struct IntFast16* src);

struct IntFast32 {
    int_fast32_t literal;
    uint_fast64_t type_tag;
};


extern struct IntFast32* IntFast32__init__(int_fast32_t literal);
extern void IntFast32__deinit__(struct IntFast32* self);
extern struct IntFast32* IntFast32__copy__(struct IntFast32* self);
extern struct IntFast32* IntFast32__add__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32* IntFast32__sub__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32* IntFast32__mul__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast32* IntFast32__div__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__eq__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__neq__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__gte__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__lte__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__gt__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__lt__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__not__(struct IntFast32* self);
extern struct IntFast8* IntFast32__and__(struct IntFast32* self, struct IntFast32* src);
extern struct IntFast8* IntFast32__or__(struct IntFast32* self, struct IntFast32* src);

struct IntFast64 {
    int_fast64_t literal;
    uint_fast64_t type_tag;
};


extern struct IntFast64* IntFast64__init__(int_fast64_t literal);
extern void IntFast64__deinit__(struct IntFast64* self);
extern struct IntFast64* IntFast64__copy__(struct IntFast64* self);
extern struct IntFast64* IntFast64__add__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64* IntFast64__sub__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64* IntFast64__mul__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast64* IntFast64__div__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__eq__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__neq__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__gte__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__lte__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__gt__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__lt__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__not__(struct IntFast64* self);
extern struct IntFast8* IntFast64__and__(struct IntFast64* self, struct IntFast64* src);
extern struct IntFast8* IntFast64__or__(struct IntFast64* self, struct IntFast64* src);

struct UIntFast8 {
    uint_fast8_t literal;
    uint_fast64_t type_tag;
};


extern struct UIntFast8* UIntFast8__init__(uint_fast8_t literal);
extern void UIntFast8__deinit__(struct UIntFast8* self);
extern struct UIntFast8* UIntFast8__copy__(struct UIntFast8* self);
extern struct UIntFast8* UIntFast8__add__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8* UIntFast8__sub__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8* UIntFast8__mul__(struct UIntFast8* self, struct UIntFast8* src);
extern struct UIntFast8* UIntFast8__div__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__eq__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__neq__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__gte__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__lte__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__gt__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__lt__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__not__(struct UIntFast8* self);
extern struct IntFast8* UIntFast8__and__(struct UIntFast8* self, struct UIntFast8* src);
extern struct IntFast8* UIntFast8__or__(struct UIntFast8* self, struct UIntFast8* src);

struct UIntFast16 {
    uint_fast16_t literal;
    uint_fast64_t type_tag;
};


extern struct UIntFast16* UIntFast16__init__(uint_fast16_t literal);
extern void UIntFast16__deinit__(struct UIntFast16* self);
extern struct UIntFast16* UIntFast16__copy__(struct UIntFast16* self);
extern struct UIntFast16* UIntFast16__add__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16* UIntFast16__sub__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16* UIntFast16__mul__(struct UIntFast16* self, struct UIntFast16* src);
extern struct UIntFast16* UIntFast16__div__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__eq__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__neq__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__gte__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__lte__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__gt__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__lt__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__not__(struct UIntFast16* self);
extern struct IntFast8* UIntFast16__and__(struct UIntFast16* self, struct UIntFast16* src);
extern struct IntFast8* UIntFast16__or__(struct UIntFast16* self, struct UIntFast16* src);

struct UIntFast32 {
    uint_fast32_t literal;
    uint_fast64_t type_tag;
};


extern struct UIntFast32* UIntFast32__init__(uint_fast32_t literal);
extern void UIntFast32__deinit__(struct UIntFast32* self);
extern struct UIntFast32* UIntFast32__copy__(struct UIntFast32* self);
extern struct UIntFast32* UIntFast32__add__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32* UIntFast32__sub__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32* UIntFast32__mul__(struct UIntFast32* self, struct UIntFast32* src);
extern struct UIntFast32* UIntFast32__div__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__eq__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__neq__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__gte__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__lte__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__gt__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__lt__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__not__(struct UIntFast32* self);
extern struct IntFast8* UIntFast32__and__(struct UIntFast32* self, struct UIntFast32* src);
extern struct IntFast8* UIntFast32__or__(struct UIntFast32* self, struct UIntFast32* src);

struct UIntFast64 {
    uint_fast64_t literal;
    uint_fast64_t type_tag;
};


extern struct UIntFast64* UIntFast64__init__(uint_fast64_t literal);
extern void UIntFast64__deinit__(struct UIntFast64* self);
extern struct UIntFast64* UIntFast64__copy__(struct UIntFast64* self);
extern struct UIntFast64* UIntFast64__add__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64* UIntFast64__sub__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64* UIntFast64__mul__(struct UIntFast64* self, struct UIntFast64* src);
extern struct UIntFast64* UIntFast64__div__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__eq__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__neq__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__gte__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__lte__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__gt__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__lt__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__not__(struct UIntFast64* self);
extern struct IntFast8* UIntFast64__and__(struct UIntFast64* self, struct UIntFast64* src);
extern struct IntFast8* UIntFast64__or__(struct UIntFast64* self, struct UIntFast64* src);

#endif