#include <stdint.h>
#include <stdlib.h>
struct adr_u_Another {
int_fast8_t* adr_u_anotherField;
};
struct adr_u_Another* adr_u_Another___copy__(struct adr_u_Another* adr_u_self) {
struct adr_u_Another* adr_u_new = malloc(sizeof(struct adr_u_Another));
adr_u_new->adr_u_anotherField = malloc(sizeof(int_fast8_t));
*adr_u_new->adr_u_anotherField = *adr_u_self->adr_u_anotherField;
return adr_u_new;
}
void adr_u_Another___deinit__(struct adr_u_Another* adr_u_self) {
free(adr_u_self->adr_u_anotherField);
free(adr_u_self);
}
struct adr_u_Another* adr_u_Another___init__(int_fast8_t* adr_u_anotherField) {
struct adr_u_Another* adr_u_self = malloc(sizeof(struct adr_u_Another));
adr_u_self->adr_u_anotherField = malloc(sizeof(int_fast8_t));
*adr_u_self->adr_u_anotherField = *adr_u_anotherField;
return adr_u_self;
}
int_fast8_t* adr_u_Another_anotherMethod(struct adr_u_Another* adr_u_self) {
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = *adr_u_self->adr_u_anotherField;
return adr_t0;
}
struct adr_u_Some {
struct adr_u_Another* adr_u_field;
};
struct adr_u_Some* adr_u_Some___copy__(struct adr_u_Some* adr_u_self) {
struct adr_u_Some* adr_u_new = malloc(sizeof(struct adr_u_Some));
struct adr_u_Another* adr_t1 = adr_u_Another___copy__(adr_u_self->adr_u_field);
adr_u_new->adr_u_field = adr_u_Another___copy__(adr_t1);
adr_u_Another___deinit__(adr_t1);
return adr_u_new;
}
void adr_u_Some___deinit__(struct adr_u_Some* adr_u_self) {
adr_u_Another___deinit__(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_Some* adr_u_Some___init__(struct adr_u_Another* adr_u_field) {
struct adr_u_Some* adr_u_self = malloc(sizeof(struct adr_u_Some));
adr_u_self->adr_u_field = adr_u_Another___copy__(adr_u_field);
return adr_u_self;
}
struct adr_u_Another* adr_u_Some_method(struct adr_u_Some* adr_u_self) {
struct adr_u_Another* adr_t2 = adr_u_Another___copy__(adr_u_self->adr_u_field);
return adr_t2;
}
int main(void) {
int_fast8_t* adr_t3 = malloc(sizeof(int_fast8_t));
*adr_t3 = 10;
struct adr_u_Another* adr_t4 = adr_u_Another___init__(adr_t3);
struct adr_u_Some* adr_t5 = adr_u_Some___init__(adr_t4);
struct adr_u_Another* adr_t6 = adr_u_Some_method(adr_t5);
int_fast8_t* adr_u_aField = malloc(sizeof(int_fast8_t));
*adr_u_aField = *adr_t6->adr_u_anotherField;
free(adr_u_aField);
free(adr_t3);
adr_u_Another___deinit__(adr_t4);
adr_u_Some___deinit__(adr_t5);
adr_u_Another___deinit__(adr_t6);
return 0;
}