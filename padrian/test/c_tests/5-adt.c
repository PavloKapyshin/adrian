#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct u87a4f1Some {
void* u87a4f1data;
uint_fast64_t type_tag;
};
struct u87a4f1None {
uint_fast64_t type_tag;
};
struct u87a4f1None* u87a4f1None__init__() {
struct u87a4f1None* self = malloc(sizeof(struct u87a4f1None));
self->type_tag = 52;
return self;
}
struct u87a4f1None* u87a4f1None__copy__(struct u87a4f1None* self) {
struct u87a4f1None* new = malloc(sizeof(struct u87a4f1None));
self->type_tag = 52;
return new;
}
void u87a4f1None__deinit__(struct u87a4f1None* self) {
free(self);
}
union u87a4f1Maybe {
struct u87a4f1Some* f0;
struct u87a4f1None* f1;
};
int main(void) {
union u87a4f1Maybe u87a4f1b;
struct IntFast8* t0 = IntFast8__init__(1);
struct u87a4f1Some* i0_self = malloc(sizeof(struct u87a4f1Some));
i0_self->u87a4f1data = IntFast8__copy__(t0);
i0_self->type_tag = 51;
u87a4f1b.f0 = i0_self;
IntFast8__deinit__(u87a4f1b.f0->u87a4f1data);
free(u87a4f1b.f0);
u87a4f1b.f1 = u87a4f1None__init__();
struct IntFast8* t1 = IntFast8__init__(2);
u87a4f1None__deinit__(u87a4f1b.f1);
struct u87a4f1Some* i2_self = malloc(sizeof(struct u87a4f1Some));
i2_self->u87a4f1data = IntFast8__copy__(t1);
i2_self->type_tag = 51;
u87a4f1b.f0 = i2_self;
struct u87a4f1Some* i3_new = malloc(sizeof(struct u87a4f1Some));
i3_new->u87a4f1data = IntFast8__copy__(u87a4f1b.f0->u87a4f1data);
u87a4f1b.f0->type_tag = 51;
struct u87a4f1Some* t2 = i3_new;
struct IntFast8* u87a4f1c = IntFast8__copy__(t2->u87a4f1data);
struct u87a4f1Some* i4_new = malloc(sizeof(struct u87a4f1Some));
i4_new->u87a4f1data = IntFast8__copy__(u87a4f1b.f0->u87a4f1data);
u87a4f1b.f0->type_tag = 51;
struct u87a4f1Some* u87a4f1d = i4_new;
union u87a4f1Maybe u87a4f1e;
struct u87a4f1Some* i5_new = malloc(sizeof(struct u87a4f1Some));
i5_new->u87a4f1data = IntFast8__copy__(u87a4f1b.f0->u87a4f1data);
u87a4f1b.f0->type_tag = 51;
u87a4f1e.f0 = i5_new;
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2->u87a4f1data);
free(t2);
IntFast8__deinit__(u87a4f1b.f0->u87a4f1data);
free(u87a4f1b.f0);
IntFast8__deinit__(u87a4f1c);
IntFast8__deinit__(u87a4f1d->u87a4f1data);
free(u87a4f1d);
IntFast8__deinit__(u87a4f1e.f0->u87a4f1data);
free(u87a4f1e.f0);
return 0;
}
