#include <stdlib.h>
#include <stdint.h>
#include "adrian_c.h"
struct adr_u_Test {
struct IntFast8* adr_u_field;
};
struct adr_u_Test* adr_u_Test___copy__(struct adr_u_Test* adr_u_self) {
struct adr_u_Test* adr_u_new = malloc(sizeof(struct adr_u_Test));
adr_u_new->adr_u_field = IntFast8___copy__(adr_u_self->adr_u_field);
return adr_u_new;
}
void adr_u_Test___deinit__(struct adr_u_Test* adr_u_self) {
IntFast8___deinit__(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_Test* adr_u_Test___init__(struct IntFast8* adr_u_field) {
struct adr_u_Test* adr_u_self = malloc(sizeof(struct adr_u_Test));
adr_u_self->adr_u_field = IntFast8___copy__(adr_u_field);
return adr_u_self;
}
struct adr_u_Another {
struct IntFast8* adr_u_field;
};
struct adr_u_Another* adr_u_Another___copy__(struct adr_u_Another* adr_u_self) {
struct adr_u_Another* adr_u_new = malloc(sizeof(struct adr_u_Another));
adr_u_new->adr_u_field = IntFast8___copy__(adr_u_self->adr_u_field);
return adr_u_new;
}
void adr_u_Another___deinit__(struct adr_u_Another* adr_u_self) {
IntFast8___deinit__(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_Another* adr_u_Another___init__(struct IntFast8* adr_u_field) {
struct adr_u_Another* adr_u_self = malloc(sizeof(struct adr_u_Another));
adr_u_self->adr_u_field = IntFast8___copy__(adr_u_field);
return adr_u_self;
}
struct adr_u_Lol {
struct adr_u_Another* adr_u_data;
};
struct adr_u_Lol* adr_u_Lol___copy__(struct adr_u_Lol* adr_u_self) {
struct adr_u_Lol* adr_u_new = malloc(sizeof(struct adr_u_Lol));
adr_u_new->adr_u_data = adr_u_Another___copy__(adr_u_self->adr_u_data);
return adr_u_new;
}
void adr_u_Lol___deinit__(struct adr_u_Lol* adr_u_self) {
adr_u_Another___deinit__(adr_u_self->adr_u_data);
free(adr_u_self);
}
struct adr_u_Lol* adr_u_Lol___init__(struct adr_u_Another* adr_u_data) {
struct adr_u_Lol* adr_u_self = malloc(sizeof(struct adr_u_Lol));
adr_u_self->adr_u_data = adr_u_Another___copy__(adr_u_data);
return adr_u_self;
}
int main(void) {
struct IntFast8* adr_t0 = IntFast8___init__(10);
struct adr_u_Test* adr_u_t = adr_u_Test___init__(adr_t0);
IntFast8___deinit__(adr_u_t->adr_u_field);
adr_u_t->adr_u_field = IntFast8___init__(40);
struct IntFast8* adr_t1 = IntFast8___init__(0);
adr_u_Test___deinit__(adr_u_t);
adr_u_t = adr_u_Test___init__(adr_t1);
struct IntFast8* adr_t2 = IntFast8___init__(1);
struct adr_u_Another* adr_t3 = adr_u_Another___init__(adr_t2);
struct adr_u_Lol* adr_u_test = adr_u_Lol___init__(adr_t3);
struct IntFast8* adr_t4 = IntFast8___init__(0);
adr_u_Another___deinit__(adr_u_test->adr_u_data);
adr_u_test->adr_u_data = adr_u_Another___init__(adr_t4);
IntFast8___deinit__(adr_t0);
adr_u_Test___deinit__(adr_u_t);
IntFast8___deinit__(adr_t1);
IntFast8___deinit__(adr_t2);
adr_u_Another___deinit__(adr_t3);
adr_u_Lol___deinit__(adr_u_test);
IntFast8___deinit__(adr_t4);
return 0;
}
