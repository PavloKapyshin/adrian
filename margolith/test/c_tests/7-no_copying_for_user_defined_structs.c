#include <stdlib.h>
#include <stdint.h>
#include "adrian_c.h"
struct adr_u_T {
struct IntFast8* adr_u_data;
};
struct adr_u_T* adr_u_T___copy__(struct adr_u_T* adr_u_self) {
struct adr_u_T* adr_u_new = malloc(sizeof(struct adr_u_T));
adr_u_new->adr_u_data = IntFast8___copy__(adr_u_self->adr_u_data);
return adr_u_new;
}
void adr_u_T___deinit__(struct adr_u_T* adr_u_self) {
IntFast8___deinit__(adr_u_self->adr_u_data);
free(adr_u_self);
}
struct adr_u_T* adr_u_T___init__(struct IntFast8* adr_u_data) {
struct adr_u_T* adr_u_self = malloc(sizeof(struct adr_u_T));
adr_u_self->adr_u_data = IntFast8___copy__(adr_u_data);
return adr_u_self;
}
int main(void) {
struct IntFast8* adr_t0 = IntFast8___init__(0);
struct adr_u_T* adr_u_a = adr_u_T___init__(adr_t0);
struct adr_u_T* adr_u_b = adr_u_T___copy__(adr_u_a);
IntFast8___deinit__(adr_t0);
adr_u_T___deinit__(adr_u_a);
adr_u_T___deinit__(adr_u_b);
return 0;
}
