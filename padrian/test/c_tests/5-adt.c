#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct Some {
void* data;
};
struct None {

};
struct None* None__init__() {
struct None* self = malloc(sizeof(struct None));
return self;
}
struct None* None__copy__(struct None* self) {
struct None* new = malloc(sizeof(struct None));
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
b.f0 = i0_self;
IntFast8__deinit__(b.f0->data);
free(b.f0);
b.f1 = None__init__();
None__deinit__(b.f1);
IntFast8__deinit__(t0);
return 0;
}
