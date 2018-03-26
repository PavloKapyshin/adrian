#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct Some {
void* data;
void* type_tag;
};
struct None {
uint_fast64_t type_tag;
};
struct None* None__init__() {
struct None* self = malloc(sizeof(struct None));
self->type_tag = 9;
return self;
}
struct None* None__copy__(struct None* self) {
struct None* new = malloc(sizeof(struct None));
self->type_tag = 9;
return new;
}
void None__deinit__(struct None* self) {
free(self);
}
union Maybe {
struct Some* f0;
struct None* f1;
};
int main(void) {
union Maybe b;
struct IntFast8* t0 = IntFast8__init__(1);
struct Some* i0_self = malloc(sizeof(struct Some));
i0_self->data = IntFast8__copy__(t0);
i0_self->type_tag = 8;
b.f0 = i0_self;
IntFast8__deinit__(b.f0->data);
free(b.f0);
b.f1 = None__init__();
struct IntFast8* t1 = IntFast8__init__(2);
None__deinit__(b.f1);
struct Some* i2_self = malloc(sizeof(struct Some));
i2_self->data = IntFast8__copy__(t1);
i2_self->type_tag = 8;
b.f0 = i2_self;
struct Some* i3_new = malloc(sizeof(struct Some));
i3_new->data = IntFast8__copy__(b.f0->data);
b.f0->type_tag = 8;
struct Some* t2 = i3_new;
struct IntFast8* c = IntFast8__copy__(t2->data);
struct Some* i4_new = malloc(sizeof(struct Some));
i4_new->data = IntFast8__copy__(b.f0->data);
b.f0->type_tag = 8;
struct Some* d = i4_new;
IntFast8__deinit__(b.f0->data);
free(b.f0);
IntFast8__deinit__(c);
IntFast8__deinit__(d->data);
free(d);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2->data);
free(t2);
return 0;
}
