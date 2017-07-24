#include <stdint.h>
#include <stdlib.h>
struct Person {
uint_fast64_t id;
uint_fast8_t age;
};
struct Person* __copy__Person(struct Person* self) {
struct Person* new = malloc(sizeof(struct Person));
new->id = self->id;
new->age = self->age;
return new;
}
void __deinit__Person(struct Person* self) {
free(self);
}
struct Person* __init__Person(uint_fast64_t id, uint_fast8_t age) {
struct Person* self = malloc(sizeof(struct Person));
self->id = id;
self->age = age;
return self;
}
void setAgePerson(struct Person* self, uint_fast8_t age) {
self->age = age;
}
void addOneYearPerson(struct Person* self) {
setAgePerson(self, self->age + 1);
}
uint_fast8_t getAgePerson(struct Person* self) {
return self->age;
}
int main(void) {
struct Person* john = __init__Person(1, 30);
struct Person* jack = __init__Person(2, 32);
struct Person* mike = __copy__Person(john);
mike->id = 3;
uint_fast64_t mikeId = mike->id;
uint_fast8_t mikeAge = getAgePerson(mike);
addOneYearPerson(mike);
__deinit__Person(jack);
__deinit__Person(john);
__deinit__Person(mike);
return 0;
}