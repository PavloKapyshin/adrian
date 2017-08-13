#include <stdint.h>
#include <stdlib.h>
struct adr__User {
int_fast8_t adr__id;
};
struct adr__User* adr__User__init__(int_fast8_t adr__id) {
struct adr__User* adr__self = malloc(sizeof(struct adr__User));
adr__self->adr__id = adr__id;
return adr__self;
}
void adr__User__deinit__(struct adr__User* adr__self) {
free(adr__self);
}
struct adr__User* adr__User__copy__(struct adr__User* adr__self) {
struct adr__User* adr__new = malloc(sizeof(struct adr__User));
adr__new->adr__id = adr__self->adr__id;
return adr__new;
}
struct adr__User* adr__User__deepCopy__(struct adr__User* adr__self) {
struct adr__User* adr__new = malloc(sizeof(struct adr__User));
adr__new->adr__id = adr__self->adr__id;
return adr__new;
}
struct adr__Person {
void* adr__data;
};
int main(void) {
struct adr__User* adr__tmp_0 = adr__User__init__(10);
struct adr__User* adr__tmp_1 = adr__User__copy__(adr__tmp_0);
struct adr__Person* adr__self = malloc(sizeof(struct adr__Person));
adr__self->adr__data = adr__User__copy__(adr__tmp_1);
struct adr__Person* adr__p = adr__self;
adr__User__deinit__(adr__p->adr__data);
free(adr__p);
adr__User__deinit__(adr__tmp_0);
adr__User__deinit__(adr__tmp_1);
return 0;
}