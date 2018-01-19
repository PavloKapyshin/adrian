#include <stdlib.h>
#include <stdint.h>
#include "adrian_c.h"
struct adr_u_Person1 {
struct UIntFast64* adr_u_id;
struct UIntFast8* adr_u_age;
};
struct adr_u_Person1* adr_u_Person1___copy__(struct adr_u_Person1* adr_u_self) {
struct adr_u_Person1* adr_u_new = malloc(sizeof(struct adr_u_Person1));
adr_u_new->adr_u_id = UIntFast64___copy__(adr_u_self->adr_u_id);
adr_u_new->adr_u_age = UIntFast8___copy__(adr_u_self->adr_u_age);
return adr_u_new;
}
void adr_u_Person1___deinit__(struct adr_u_Person1* adr_u_self) {
UIntFast64___deinit__(adr_u_self->adr_u_id);
UIntFast8___deinit__(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person1* adr_u_Person1___init__(struct UIntFast64* adr_u_id, struct UIntFast8* adr_u_age) {
struct adr_u_Person1* adr_u_self = malloc(sizeof(struct adr_u_Person1));
adr_u_self->adr_u_id = UIntFast64___copy__(adr_u_id);
adr_u_self->adr_u_age = UIntFast8___copy__(adr_u_age);
return adr_u_self;
}
struct adr_u_Person2 {
struct UIntFast8* adr_u_age;
};
struct adr_u_Person2* adr_u_Person2___copy__(struct adr_u_Person2* adr_u_self) {
struct adr_u_Person2* adr_u_new = malloc(sizeof(struct adr_u_Person2));
adr_u_new->adr_u_age = UIntFast8___copy__(adr_u_self->adr_u_age);
return adr_u_new;
}
void adr_u_Person2___deinit__(struct adr_u_Person2* adr_u_self) {
UIntFast8___deinit__(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person2* adr_u_Person2___init__(struct UIntFast8* adr_u_age) {
struct adr_u_Person2* adr_u_self = malloc(sizeof(struct adr_u_Person2));
adr_u_self->adr_u_age = UIntFast8___copy__(adr_u_age);
return adr_u_self;
}
struct adr_u_Person3 {
struct UIntFast64* adr_u_id;
struct UIntFast8* adr_u_age;
};
struct adr_u_Person3* adr_u_Person3___copy__(struct adr_u_Person3* adr_u_self) {
struct adr_u_Person3* adr_u_new = malloc(sizeof(struct adr_u_Person3));
adr_u_new->adr_u_id = UIntFast64___copy__(adr_u_self->adr_u_id);
adr_u_new->adr_u_age = UIntFast8___copy__(adr_u_self->adr_u_age);
return adr_u_new;
}
void adr_u_Person3___deinit__(struct adr_u_Person3* adr_u_self) {
UIntFast64___deinit__(adr_u_self->adr_u_id);
UIntFast8___deinit__(adr_u_self->adr_u_age);
free(adr_u_self);
}
struct adr_u_Person3* adr_u_Person3___init__(struct UIntFast8* adr_u_age, struct UIntFast64* adr_u_id) {
struct adr_u_Person3* adr_u_self = malloc(sizeof(struct adr_u_Person3));
adr_u_self->adr_u_age = UIntFast8___copy__(adr_u_age);
adr_u_self->adr_u_id = UIntFast64___copy__(adr_u_id);
return adr_u_self;
}
int main(void) {
return 0;
}
