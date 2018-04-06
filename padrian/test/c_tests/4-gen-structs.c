#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct u9b4dcfSome {
void* u9b4dcffield;
uint_fast64_t type_tag;
};
struct u9b4dcfLList {
void* u9b4dcfvalue;
uint_fast64_t type_tag;
};
int main(void) {
struct IntFast8* t0 = IntFast8__init__(1);
struct u9b4dcfSome* i0_self = malloc(sizeof(struct u9b4dcfSome));
i0_self->u9b4dcffield = IntFast8__copy__(t0);
i0_self->type_tag = 51;
struct u9b4dcfSome* u9b4dcfjustAVar = i0_self;
struct IntFast8* u9b4dcff = IntFast8__copy__(u9b4dcfjustAVar->u9b4dcffield);
struct IntFast8* u9b4dcffByMethod = u9b4dcfjustAVar->u9b4dcffield;
struct IntFast8* t1 = IntFast8__init__(3);
struct IntFast8* u9b4dcfsame = t1;
struct IntFast8* t2 = IntFast8__init__(1);
struct IntFast8* i3_u9b4dcfb = IntFast8__copy__(t2);
struct IntFast8* u9b4dcfl = i3_u9b4dcfb;
struct IntFast8* t3 = IntFast8__init__(1);
struct u9b4dcfSome* i8_self = malloc(sizeof(struct u9b4dcfSome));
i8_self->u9b4dcffield = IntFast8__copy__(t3);
i8_self->type_tag = 51;
struct u9b4dcfSome* t4 = i8_self;
struct u9b4dcfLList* i10_self = malloc(sizeof(struct u9b4dcfLList));
struct u9b4dcfSome* i10_i9_new = malloc(sizeof(struct u9b4dcfSome));
i10_i9_new->u9b4dcffield = IntFast8__copy__(t4->u9b4dcffield);
i10_i9_new->type_tag = 51;
i10_self->u9b4dcfvalue = i10_i9_new;
i10_self->type_tag = 52;
struct u9b4dcfLList* u9b4dcfhoping = i10_self;
struct u9b4dcfSome* u9b4dcfanotherHope = u9b4dcfhoping->u9b4dcfvalue;
struct u9b4dcfSome* i12_i11_self = malloc(sizeof(struct u9b4dcfSome));
i12_i11_self->u9b4dcffield = IntFast8__copy__(((struct u9b4dcfSome*)(u9b4dcfhoping->u9b4dcfvalue))->u9b4dcffield);
i12_i11_self->type_tag = 51;
struct u9b4dcfSome* i12_u9b4dcfvariable = i12_i11_self;
struct u9b4dcfSome* u9b4dcfhope = i12_u9b4dcfvariable;
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2);
IntFast8__deinit__(t3);
IntFast8__deinit__(t4->u9b4dcffield);
free(t4);
IntFast8__deinit__(u9b4dcff);
IntFast8__deinit__(u9b4dcfhope->u9b4dcffield);
free(u9b4dcfhope);
IntFast8__deinit__(((struct u9b4dcfSome*)(u9b4dcfhoping->u9b4dcfvalue))->u9b4dcffield);
free(u9b4dcfhoping->u9b4dcfvalue);
free(u9b4dcfhoping);
IntFast8__deinit__(u9b4dcfjustAVar->u9b4dcffield);
free(u9b4dcfjustAVar);
IntFast8__deinit__(u9b4dcfl);
return 0;
}
