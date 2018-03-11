#include <stdlib.h>
#include <stdint.h>
#include "adrian_c.h"
struct adr_u_T {
struct UIntFast64* adr_u_field;
};
struct adr_u_T* adr_u_T___copy__(struct adr_u_T* adr_u_self) {
struct adr_u_T* adr_u_new = malloc(sizeof(struct adr_u_T));
adr_u_new->adr_u_field = UIntFast64___copy__(adr_u_self->adr_u_field);
return adr_u_new;
}
void adr_u_T___deinit__(struct adr_u_T* adr_u_self) {
UIntFast64___deinit__(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_T* adr_u_T___init__(struct UIntFast64* adr_u_field) {
struct adr_u_T* adr_u_self = malloc(sizeof(struct adr_u_T));
adr_u_self->adr_u_field = UIntFast64___copy__(adr_u_field);
return adr_u_self;
}
int main(void) {
struct IntFast8* adr_u_a = IntFast8___init__(0);
struct IntFast8* adr_u_b = adr_u_a;
struct UIntFast64* adr_t0 = UIntFast64___init__(123);
struct adr_u_T* adr_u_c = adr_u_T___init__(adr_t0);
struct UIntFast64* adr_u_d = adr_u_c->adr_u_field;
IntFast8___deinit__(adr_u_a);
UIntFast64___deinit__(adr_t0);
adr_u_T___deinit__(adr_u_c);
return 0;
}
