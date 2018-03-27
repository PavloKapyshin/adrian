#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct u77c3e0Some {
void* u77c3e0data;
uint_fast64_t type_tag;
};
struct u77c3e0None {
uint_fast64_t type_tag;
};
struct u77c3e0None* u77c3e0None__init__() {
struct u77c3e0None* self = malloc(sizeof(struct u77c3e0None));
self->type_tag = 9;
return self;
}
struct u77c3e0None* u77c3e0None__copy__(struct u77c3e0None* self) {
struct u77c3e0None* new = malloc(sizeof(struct u77c3e0None));
self->type_tag = 9;
return new;
}
void u77c3e0None__deinit__(struct u77c3e0None* self) {
free(self);
}
union u77c3e0Maybe {
struct u77c3e0Some* f0;
struct u77c3e0None* f1;
};
int main(void) {
union u77c3e0Maybe u77c3e0b;
struct IntFast8* t0 = IntFast8__init__(1);
struct u77c3e0Some* i0_self = malloc(sizeof(struct u77c3e0Some));
i0_self->u77c3e0data = IntFast8__copy__(t0);
i0_self->type_tag = 8;
u77c3e0b.f0 = i0_self;
IntFast8__deinit__(u77c3e0b.f0->u77c3e0data);
free(u77c3e0b.f0);
u77c3e0b.f1 = u77c3e0None__init__();
struct IntFast8* t1 = IntFast8__init__(2);
u77c3e0None__deinit__(u77c3e0b.f1);
struct u77c3e0Some* i2_self = malloc(sizeof(struct u77c3e0Some));
i2_self->u77c3e0data = IntFast8__copy__(t1);
i2_self->type_tag = 8;
u77c3e0b.f0 = i2_self;
struct u77c3e0Some* i3_new = malloc(sizeof(struct u77c3e0Some));
i3_new->u77c3e0data = IntFast8__copy__(u77c3e0b.f0->u77c3e0data);
u77c3e0b.f0->type_tag = 8;
struct u77c3e0Some* t2 = i3_new;
struct IntFast8* u77c3e0c = IntFast8__copy__(t2->u77c3e0data);
struct u77c3e0Some* i4_new = malloc(sizeof(struct u77c3e0Some));
i4_new->u77c3e0data = IntFast8__copy__(u77c3e0b.f0->u77c3e0data);
u77c3e0b.f0->type_tag = 8;
struct u77c3e0Some* u77c3e0d = i4_new;
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2->u77c3e0data);
free(t2);
IntFast8__deinit__(u77c3e0b.f0->u77c3e0data);
free(u77c3e0b.f0);
IntFast8__deinit__(u77c3e0c);
IntFast8__deinit__(u77c3e0d->u77c3e0data);
free(u77c3e0d);
return 0;
}
