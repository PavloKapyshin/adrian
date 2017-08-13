#include <stdint.h>
#include <stdlib.h>
struct adr__Person {
uint_fast64_t adr__id;
uint_fast8_t adr__age;
};
struct adr__Person* adr__Person__init__(uint_fast64_t adr__id, uint_fast8_t adr__age) {
struct adr__Person* adr__self = malloc(sizeof(struct adr__Person));
adr__self->adr__id = adr__id;
adr__self->adr__age = adr__age;
return adr__self;
}
void adr__Person__deinit__(struct adr__Person* adr__self) {
free(adr__self);
}
struct adr__Person* adr__Person__copy__(struct adr__Person* adr__self) {
struct adr__Person* adr__new = malloc(sizeof(struct adr__Person));
adr__new->adr__id = adr__self->adr__id;
adr__new->adr__age = adr__self->adr__age;
return adr__new;
}
struct adr__Person* adr__Person__deepCopy__(struct adr__Person* adr__self) {
struct adr__Person* adr__new = malloc(sizeof(struct adr__Person));
adr__new->adr__id = adr__self->adr__id;
adr__new->adr__age = adr__self->adr__age;
return adr__new;
}
struct adr__HighPerson {
struct adr__Person* adr__person;
};
void adr__HighPerson__deinit__(struct adr__HighPerson* adr__self) {
free(adr__self);
}
struct adr__HighPerson* adr__HighPerson__copy__(struct adr__HighPerson* adr__self) {
struct adr__HighPerson* adr__new = malloc(sizeof(struct adr__HighPerson));
adr__new->adr__person = adr__self->adr__person;
return adr__new;
}
struct adr__HighPerson* adr__HighPerson__deepCopy__(struct adr__HighPerson* adr__self) {
struct adr__HighPerson* adr__new = malloc(sizeof(struct adr__HighPerson));
adr__new->adr__person = adr__Person__deepCopy__(adr__self->adr__person);
return adr__new;
}
struct adr__HighPerson* adr__HighPerson__init__(struct adr__Person* adr__person) {
struct adr__HighPerson* adr__self = malloc(sizeof(struct adr__HighPerson));
adr__self->adr__person = adr__person;
return adr__self;
}
struct adr__HighPerson* adr__doSomethingWithJohn(struct adr__Person* adr__john) {
return adr__HighPerson__init__(adr__john);
}
int main(void) {
struct adr__Person* adr__john = adr__Person__init__(1, 20);
struct adr__Person* adr__secondJohn = adr__john;
struct adr__HighPerson* adr__anotherJohn = adr__doSomethingWithJohn(adr__secondJohn);
adr__HighPerson__deinit__(adr__anotherJohn);
adr__Person__deinit__(adr__john);
return 0;
}