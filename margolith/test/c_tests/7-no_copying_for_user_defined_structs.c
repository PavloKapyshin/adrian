#include <stdint.h>
#include <stdlib.h>
struct adr_u_T {
int_fast8_t* adr_u_data;
};
struct adr_u_T* adr_u_T___copy__(struct adr_u_T* adr_u_self) {
struct adr_u_T* adr_u_new = malloc(sizeof(struct adr_u_T));
adr_u_new->adr_u_data = malloc(sizeof(int_fast8_t));
*adr_u_new->adr_u_data = *adr_u_self->adr_u_data;
return adr_u_new;
}
void adr_u_T___deinit__(struct adr_u_T* adr_u_self) {
free(adr_u_self->adr_u_data);
free(adr_u_self);
}
struct adr_u_T* adr_u_T___init__(int_fast8_t* adr_u_data) {
struct adr_u_T* adr_u_self = malloc(sizeof(struct adr_u_T));
adr_u_self->adr_u_data = malloc(sizeof(int_fast8_t));
*adr_u_self->adr_u_data = *adr_u_data;
return adr_u_self;
}
int main(void) {
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = 0;
struct adr_u_T* adr_u_a = adr_u_T___init__(adr_t0);
struct adr_u_T* adr_u_b = adr_u_T___copy__(adr_u_a);
adr_u_T___deinit__(adr_u_a);
adr_u_T___deinit__(adr_u_b);
free(adr_t0);
return 0;
}