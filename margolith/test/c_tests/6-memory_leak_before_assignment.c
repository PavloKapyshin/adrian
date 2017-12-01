#include <stdint.h>
#include <stdlib.h>
struct adr_u_Test {
int_fast8_t* adr_u_field;
};
struct adr_u_Test* adr_u_Test___copy__(struct adr_u_Test* adr_u_self) {
struct adr_u_Test* adr_u_new = malloc(sizeof(struct adr_u_Test));
adr_u_new->adr_u_field = malloc(sizeof(int_fast8_t));
*adr_u_new->adr_u_field = *adr_u_self->adr_u_field;
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
struct adr_u_Another {
int_fast8_t* adr_u_field;
};
struct adr_u_Another* adr_u_Another___copy__(struct adr_u_Another* adr_u_self) {
struct adr_u_Another* adr_u_new = malloc(sizeof(struct adr_u_Another));
adr_u_new->adr_u_field = malloc(sizeof(int_fast8_t));
*adr_u_new->adr_u_field = *adr_u_self->adr_u_field;
return adr_u_new;
}
void adr_u_Another___deinit__(struct adr_u_Another* adr_u_self) {
free(adr_u_self->adr_u_field);
free(adr_u_self);
}
struct adr_u_Another* adr_u_Another___init__(int_fast8_t* adr_u_field) {
struct adr_u_Another* adr_u_self = malloc(sizeof(struct adr_u_Another));
adr_u_self->adr_u_field = malloc(sizeof(int_fast8_t));
*adr_u_self->adr_u_field = *adr_u_field;
return adr_u_self;
}
struct adr_u_Lol {
struct adr_u_Another* adr_u_data;
};
struct adr_u_Lol* adr_u_Lol___copy__(struct adr_u_Lol* adr_u_self) {
struct adr_u_Lol* adr_u_new = malloc(sizeof(struct adr_u_Lol));
struct adr_u_Another* adr_t2 = adr_u_Another___copy__(adr_u_self->adr_u_data);
adr_u_new->adr_u_data = adr_u_Another___copy__(adr_t2);
adr_u_Another___deinit__(adr_t2);
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
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = 10;
struct adr_u_Test* adr_u_t = adr_u_Test___init__(adr_t0);
*adr_u_t->adr_u_field = 40;
int_fast8_t* adr_t1 = malloc(sizeof(int_fast8_t));
*adr_t1 = 0;
adr_u_Test___deinit__(adr_u_t);
adr_u_t = adr_u_Test___init__(adr_t1);
int_fast8_t* adr_t3 = malloc(sizeof(int_fast8_t));
*adr_t3 = 1;
struct adr_u_Another* adr_t4 = adr_u_Another___init__(adr_t3);
struct adr_u_Lol* adr_u_test = adr_u_Lol___init__(adr_t4);
int_fast8_t* adr_t5 = malloc(sizeof(int_fast8_t));
*adr_t5 = 0;
adr_u_Another___deinit__(adr_u_test->adr_u_data);
adr_u_test->adr_u_data = adr_u_Another___init__(adr_t5);
adr_u_Test___deinit__(adr_u_t);
free(adr_t0);
free(adr_t1);
free(adr_t3);
adr_u_Another___deinit__(adr_t4);
free(adr_t5);
adr_u_Lol___deinit__(adr_u_test);
return 0;
}