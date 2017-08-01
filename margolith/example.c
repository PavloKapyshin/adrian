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
int main(void) {
struct adr__Person* adr__john = adr__Person__init__(1, 30);
adr__Person__deinit__(adr__john);
return 0;
}