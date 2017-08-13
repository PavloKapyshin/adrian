#include <stdint.h>
#include <stdlib.h>
struct adr__Person {
void* adr__data;
};
int main(void) {
struct adr__Person* adr__self = malloc(sizeof(struct adr__Person));
adr__self->adr__data = (void*)(10);
struct adr__Person* adr__p = adr__self;
int_fast8_t adr__m = (int_fast8_t)(adr__p->adr__data);
adr__p->adr__data = (void*)(20);
free(adr__p);
return 0;
}