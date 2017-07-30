#include <stdint.h>
#include <stdlib.h>
struct adr__Id {
uint_fast64_t adr__realId;
};
struct adr__Id* adr__Id__init__(uint_fast64_t adr__realId) {
struct adr__Id* adr__self = malloc(sizeof(struct adr__Id));
adr__self->adr__realId = adr__realId;
return adr__self;
}
void adr__Id__deinit__(struct adr__Id* adr__self) {
free(adr__self);
}
struct adr__Id* adr__Id__copy__(struct adr__Id* adr__self) {
struct adr__Id* adr__new = malloc(sizeof(struct adr__Id));
adr__new->adr__realId = adr__self->adr__realId;
return adr__new;
}
struct adr__Id* adr__Id__deepCopy__(struct adr__Id* adr__self) {
struct adr__Id* adr__new = malloc(sizeof(struct adr__Id));
adr__new->adr__realId = adr__self->adr__realId;
return adr__new;
}
uint_fast64_t adr__IdgetId(struct adr__Id* adr__self) {
return adr__self->adr__realId;
}
struct adr__Person {
struct adr__Id* adr__id;
uint_fast8_t adr__age;
};
struct adr__Person* adr__Person__init__(struct adr__Id* adr__id, uint_fast8_t adr__age) {
struct adr__Person* adr__self = malloc(sizeof(struct adr__Person));
adr__self->adr__id = adr__Id__copy__(adr__id);
adr__self->adr__age = adr__age;
return adr__self;
}
void adr__Person__deinit__(struct adr__Person* adr__self) {
adr__Id__deinit__(adr__self->adr__id);
free(adr__self);
}
struct adr__Person* adr__Person__copy__(struct adr__Person* adr__self) {
struct adr__Person* adr__new = malloc(sizeof(struct adr__Person));
adr__new->adr__id = adr__Id__copy__(adr__self->adr__id);
adr__new->adr__age = adr__self->adr__age;
return adr__new;
}
struct adr__Person* adr__Person__deepCopy__(struct adr__Person* adr__self) {
struct adr__Person* adr__new = malloc(sizeof(struct adr__Person));
adr__new->adr__id = adr__Id__deepCopy__(adr__self->adr__id);
adr__new->adr__age = adr__self->adr__age;
return adr__new;
}
struct adr__HighLevelPerson {
struct adr__Person* adr__lowLevelPerson;
};
struct adr__HighLevelPerson* adr__HighLevelPerson__init__(struct adr__Person* adr__lowLevelPerson) {
struct adr__HighLevelPerson* adr__self = malloc(sizeof(struct adr__HighLevelPerson));
adr__self->adr__lowLevelPerson = adr__Person__copy__(adr__lowLevelPerson);
return adr__self;
}
void adr__HighLevelPerson__deinit__(struct adr__HighLevelPerson* adr__self) {
adr__Person__deinit__(adr__self->adr__lowLevelPerson);
free(adr__self);
}
struct adr__HighLevelPerson* adr__HighLevelPerson__copy__(struct adr__HighLevelPerson* adr__self) {
struct adr__HighLevelPerson* adr__new = malloc(sizeof(struct adr__HighLevelPerson));
adr__new->adr__lowLevelPerson = adr__Person__copy__(adr__self->adr__lowLevelPerson);
return adr__new;
}
struct adr__HighLevelPerson* adr__HighLevelPerson__deepCopy__(struct adr__HighLevelPerson* adr__self) {
struct adr__HighLevelPerson* adr__new = malloc(sizeof(struct adr__HighLevelPerson));
adr__new->adr__lowLevelPerson = adr__Person__deepCopy__(adr__self->adr__lowLevelPerson);
return adr__new;
}
int main(void) {
struct adr__Id* adr__tmp_0 = adr__Id__init__(1);
struct adr__Id* adr__tmp_2 = adr__Id__copy__(adr__tmp_0);
struct adr__Person* adr__tmp_1 = adr__Person__init__(adr__tmp_2, 20);
struct adr__Person* adr__tmp_3 = adr__Person__copy__(adr__tmp_1);
struct adr__HighLevelPerson* adr__jack = adr__HighLevelPerson__init__(adr__tmp_3);
struct adr__Id* adr__johnId = adr__Id__init__(2);
uint_fast8_t adr__johnAge = 30;
struct adr__Id* adr__tmp_4 = adr__Id__copy__(adr__johnId);
struct adr__Person* adr__johnPerson = adr__Person__init__(adr__tmp_4, adr__johnAge);
struct adr__Person* adr__tmp_5 = adr__Person__copy__(adr__johnPerson);
struct adr__HighLevelPerson* adr__john = adr__HighLevelPerson__init__(adr__tmp_5);
adr__HighLevelPerson__deinit__(adr__jack);
adr__HighLevelPerson__deinit__(adr__john);
adr__Id__deinit__(adr__johnId);
adr__Person__deinit__(adr__johnPerson);
adr__Id__deinit__(adr__tmp_0);
adr__Person__deinit__(adr__tmp_1);
adr__Id__deinit__(adr__tmp_2);
adr__Person__deinit__(adr__tmp_3);
adr__Id__deinit__(adr__tmp_4);
adr__Person__deinit__(adr__tmp_5);
return 0;
}