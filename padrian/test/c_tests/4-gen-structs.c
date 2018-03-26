#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct Some {
void* field;
uint_fast64_t type_tag;
};
struct LList {
void* value;
uint_fast64_t type_tag;
};
int main(void) {
struct IntFast8* t0 = IntFast8__init__(1);
struct Some* i0_self = malloc(sizeof(struct Some));
i0_self->field = IntFast8__copy__(t0);
i0_self->type_tag = 8;
struct Some* justAVar = i0_self;
struct IntFast8* f = IntFast8__copy__(justAVar->field);
struct IntFast8* fByMethod = justAVar->field;
struct IntFast8* t1 = IntFast8__init__(3);
struct IntFast8* same = t1;
struct IntFast8* t2 = IntFast8__init__(1);
struct IntFast8* i3_b = IntFast8__copy__(t2);
struct IntFast8* l = i3_b;
struct IntFast8* t3 = IntFast8__init__(1);
struct Some* i8_self = malloc(sizeof(struct Some));
i8_self->field = IntFast8__copy__(t3);
i8_self->type_tag = 8;
struct Some* t4 = i8_self;
struct LList* i10_self = malloc(sizeof(struct LList));
struct Some* i10_i9_new = malloc(sizeof(struct Some));
i10_i9_new->field = IntFast8__copy__(t4->field);
t4->type_tag = 8;
i10_self->value = i10_i9_new;
i10_self->type_tag = 9;
struct LList* hoping = i10_self;
struct Some* anotherHope = hoping->value;
struct Some* i12_i11_self = malloc(sizeof(struct Some));
i12_i11_self->field = IntFast8__copy__(((struct Some*)(hoping->value))->field);
i12_i11_self->type_tag = 8;
struct Some* i12_variable = i12_i11_self;
struct Some* hope = i12_variable;
IntFast8__deinit__(f);
IntFast8__deinit__(hope->field);
free(hope);
IntFast8__deinit__(((struct Some*)(hoping->value))->field);
free(hoping->value);
free(hoping);
IntFast8__deinit__(justAVar->field);
free(justAVar);
IntFast8__deinit__(l);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2);
IntFast8__deinit__(t3);
IntFast8__deinit__(t4->field);
free(t4);
return 0;
}
