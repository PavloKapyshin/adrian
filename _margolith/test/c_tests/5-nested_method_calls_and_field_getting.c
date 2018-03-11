#include <stdlib.h>
#include <stdint.h>
#include "adrian_c.h"
struct adr_u_Another {
struct IntFast8* adr_u_anotherField;
};
struct adr_u_Another* adr_u_Another___copy__(struct adr_u_Another* adr_u_self) {
struct adr_u_Another* adr_u_new = malloc(sizeof(struct adr_u_Another));
adr_u_new->adr_u_anotherField = IntFast8___copy__(adr_u_self->adr_u_anotherField);
return adr_u_new;
}
void adr_u_Another___deinit__(struct adr_u_Another* adr_u_self) {
IntFast8___deinit__(adr_u_self->adr_u_anotherField);
free(adr_u_self);
}
struct adr_u_Another* adr_u_Another___init__(struct IntFast8* adr_u_anotherField) {
struct adr_u_Another* adr_u_self = malloc(sizeof(struct adr_u_Another));
adr_u_self->adr_u_anotherField = IntFast8___copy__(adr_u_anotherField);
return adr_u_self;
}
struct IntFast8* adr_u_Another_anotherMethod(struct adr_u_Another* adr_u_self) {
struct IntFast8* adr_t0 = IntFast8___copy__(adr_u_self->adr_u_anotherField);
return adr_t0;
}
struct adr_u_Some {
struct adr_u_Another* adr_u_field;
};
struct adr_u_Some* adr_u_Some___copy__(struct adr_u_Some* adr_u_self) {
struct adr_u_Some* adr_u_new = malloc(sizeof(struct adr_u_Some));
adr_u_new->adr_u_field = adr_u_Another___copy__(adr_u_self->adr_u_field);
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
struct adr_u_Another* adr_t1 = adr_u_Another___copy__(adr_u_self->adr_u_field);
return adr_t1;
}
int main(void) {
struct IntFast8* adr_t2 = IntFast8___init__(10);
struct adr_u_Another* adr_t3 = adr_u_Another___init__(adr_t2);
struct adr_u_Some* adr_t4 = adr_u_Some___init__(adr_t3);
struct adr_u_Another* adr_t5 = adr_u_Some_method(adr_t4);
struct IntFast8* adr_u_aField = IntFast8___copy__(adr_t5->adr_u_anotherField);
IntFast8___deinit__(adr_t2);
adr_u_Another___deinit__(adr_t3);
adr_u_Some___deinit__(adr_t4);
adr_u_Another___deinit__(adr_t5);
IntFast8___deinit__(adr_u_aField);
return 0;
}
