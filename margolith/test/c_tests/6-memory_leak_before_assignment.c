#include <stdint.h>
#include <stdlib.h>
struct adr_u_Test {
int_fast8_t* adr_u_field;
};
struct adr_u_Test* adr_u_Test___copy__(struct adr_u_Test* adr_u_self) {
struct adr_u_Test* adr_u_new = malloc(sizeof(struct adr_u_Test));
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = *adr_u_self->adr_u_field;
adr_u_new->adr_u_field = malloc(sizeof(int_fast8_t));
*adr_u_new->adr_u_field = *adr_t0;
free(adr_t0);
return adr_u_new;
}
void adr_u_Test___deinit__(struct adr_u_Test* adr_u_self) {
free(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_Test* adr_u_Test___init__(int_fast8_t* adr_u_field) {
struct adr_u_Test* adr_u_self = malloc(sizeof(struct adr_u_Test));
adr_u_self->adr_u_field = malloc(sizeof(int_fast8_t));
*adr_u_self->adr_u_field = *adr_u_field;
return adr_u_self;
}
int main(void) {
int_fast8_t* adr_t1 = malloc(sizeof(int_fast8_t));
*adr_t1 = 10;
struct adr_u_Test* adr_u_t = adr_u_Test___init__(adr_t1);
*adr_u_t->adr_u_field = 40;
int_fast8_t* adr_t2 = malloc(sizeof(int_fast8_t));
*adr_t2 = 0;
adr_u_Test___deinit__(adr_u_t);
adr_u_t = adr_u_Test___init__(adr_t2);
adr_u_Test___deinit__(adr_u_t);
free(adr_t1);
free(adr_t2);
return 0;
}