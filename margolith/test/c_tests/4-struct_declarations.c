#include <stdint.h>
#include <stdlib.h>
struct adr_u_Person1 {
uint_fast64_t* adr_u_id;
uint_fast8_t* adr_u_age;
};
struct adr_u_Person1* adr_u_Person1___copy__(struct adr_u_Person1* adr_u_self) {
struct adr_u_Person1* adr_u_new = malloc(sizeof(struct adr_u_Person1));
adr_u_new->adr_u_id = malloc(sizeof(uint_fast64_t));
*adr_u_new->adr_u_id = *adr_u_self->adr_u_id;
adr_u_new->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_new->adr_u_age = *adr_u_self->adr_u_age;
return adr_u_new;
}
void adr_u_Person1___deinit__(struct adr_u_Person1* adr_u_self) {
free(adr_u_self->adr_u_id);
free(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person1* adr_u_Person1___init__(uint_fast64_t* adr_u_id, uint_fast8_t* adr_u_age) {
struct adr_u_Person1* adr_u_self = malloc(sizeof(struct adr_u_Person1));
adr_u_self->adr_u_id = malloc(sizeof(uint_fast64_t));
*adr_u_self->adr_u_id = *adr_u_id;
adr_u_self->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_self->adr_u_age = *adr_u_age;
return adr_u_self;
}
struct adr_u_Person2 {
uint_fast8_t* adr_u_age;
};
struct adr_u_Person2* adr_u_Person2___copy__(struct adr_u_Person2* adr_u_self) {
struct adr_u_Person2* adr_u_new = malloc(sizeof(struct adr_u_Person2));
adr_u_new->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_new->adr_u_age = *adr_u_self->adr_u_age;
return adr_u_new;
}
void adr_u_Person2___deinit__(struct adr_u_Person2* adr_u_self) {
free(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person2* adr_u_Person2___init__(uint_fast8_t* adr_u_age) {
struct adr_u_Person2* adr_u_self = malloc(sizeof(struct adr_u_Person2));
adr_u_self->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_self->adr_u_age = *adr_u_age;
return adr_u_self;
}
struct adr_u_Person3 {
uint_fast64_t* adr_u_id;
uint_fast8_t* adr_u_age;
};
struct adr_u_Person3* adr_u_Person3___copy__(struct adr_u_Person3* adr_u_self) {
struct adr_u_Person3* adr_u_new = malloc(sizeof(struct adr_u_Person3));
adr_u_new->adr_u_id = malloc(sizeof(uint_fast64_t));
*adr_u_new->adr_u_id = *adr_u_self->adr_u_id;
adr_u_new->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_new->adr_u_age = *adr_u_self->adr_u_age;
return adr_u_new;
}
void adr_u_Person3___deinit__(struct adr_u_Person3* adr_u_self) {
free(adr_u_self->adr_u_id);
free(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person3* adr_u_Person3___init__(uint_fast8_t* adr_u_age, uint_fast64_t* adr_u_id) {
struct adr_u_Person3* adr_u_self = malloc(sizeof(struct adr_u_Person3));
adr_u_self->adr_u_age = malloc(sizeof(uint_fast8_t));
*adr_u_self->adr_u_age = *adr_u_age;
adr_u_self->adr_u_id = malloc(sizeof(uint_fast64_t));
*adr_u_self->adr_u_id = *adr_u_id;
return adr_u_self;
}
int main(void) {
return 0;
}