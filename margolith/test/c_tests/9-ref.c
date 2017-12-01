#include <stdint.h>
#include <stdlib.h>
struct adr_u_T {
uint_fast64_t* adr_u_field;
};
struct adr_u_T* adr_u_T___copy__(struct adr_u_T* adr_u_self) {
struct adr_u_T* adr_u_new = malloc(sizeof(struct adr_u_T));
adr_u_new->adr_u_field = malloc(sizeof(uint_fast64_t));
*adr_u_new->adr_u_field = *adr_u_self->adr_u_field;
return adr_u_new;
}
void adr_u_T___deinit__(struct adr_u_T* adr_u_self) {
free(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_T* adr_u_T___init__(uint_fast64_t* adr_u_field) {
struct adr_u_T* adr_u_self = malloc(sizeof(struct adr_u_T));
adr_u_self->adr_u_field = malloc(sizeof(uint_fast64_t));
*adr_u_self->adr_u_field = *adr_u_field;
return adr_u_self;
}
int main(void) {
int_fast8_t* adr_u_a = malloc(sizeof(int_fast8_t));
*adr_u_a = 0;
int_fast8_t* adr_u_b = adr_u_a;
uint_fast64_t* adr_t0 = malloc(sizeof(uint_fast64_t));
*adr_t0 = 123;
struct adr_u_T* adr_u_c = adr_u_T___init__(adr_t0);
uint_fast64_t* adr_u_d = adr_u_c->adr_u_field;
free(adr_u_a);
adr_u_T___deinit__(adr_u_c);
free(adr_t0);
return 0;
}