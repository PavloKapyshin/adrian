#include <stdint.h>
#include <stdlib.h>
struct Person {
uint_fast32_t id;
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
struct Person* __init__Person(uint_fast32_t id, uint_fast8_t age) {
struct Person* self = malloc(sizeof(struct Person));
self->id = id;
self->age = age;
return self;
}
int main(void) {
struct Person* p1 = __init__Person(1, 30);
__deinit__Person(p1);
return 0;
}