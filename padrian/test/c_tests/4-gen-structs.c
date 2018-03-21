#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct Some {
void* field;
};
struct LList {
void* value;
};
int main(void) {
struct IntFast8* t0 = IntFast8__init__(1);
struct Some* i0_self = malloc(sizeof(struct Some));
i0_self->field = IntFast8__copy__(t0);
struct Some* justAVar = i0_self;
struct IntFast8* f = IntFast8__copy__(justAVar->field);
struct IntFast8* fByMethod = justAVar->field;
struct IntFast8* t1 = IntFast8__init__(3);
struct IntFast8* same = t1;
struct IntFast8* t2 = IntFast8__init__(1);
struct IntFast8* i3_b = IntFast8__copy__(t2);
struct IntFast8* l = i3_b;
struct IntFast8* t3 = IntFast8__init__(1);
struct Some* i4_self = malloc(sizeof(struct Some));
i4_self->field = IntFast8__copy__(t3);
struct Some* t4 = i4_self;
struct LList* i6_self = malloc(sizeof(struct LList));
struct Some* i6_i5_new = malloc(sizeof(struct Some));
i6_i5_new->field = IntFast8__copy__(t4->field);
i6_self->value = i6_i5_new;
struct LList* hoping = i6_self;
struct Some* anotherHope = hoping->value;
struct Some* i8_i7_self = malloc(sizeof(struct Some));
i8_i7_self->field = IntFast8__copy__(((struct Some*)(hoping->value))->field);
struct Some* i8_variable = i8_i7_self;
struct Some* hope = i8_variable;
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
