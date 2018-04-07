#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct ud4fc83Some {
void* ud4fc83data;
uint_fast64_t type_tag;
};
struct ud4fc83None {
uint_fast64_t type_tag;
};
struct ud4fc83None* ud4fc83None__init__() {
struct ud4fc83None* self = malloc(sizeof(struct ud4fc83None));
self->type_tag = 52;
return self;
}
struct ud4fc83None* ud4fc83None__copy__(struct ud4fc83None* self) {
struct ud4fc83None* new = malloc(sizeof(struct ud4fc83None));
new->type_tag = 52;
return new;
}
void ud4fc83None__deinit__(struct ud4fc83None* self) {
free(self);
}
union ud4fc83Maybe {
struct ud4fc83Some* f0;
struct ud4fc83None* f1;
};
int main(void) {
union ud4fc83Maybe* ud4fc83b = malloc(sizeof(union ud4fc83Maybe));
struct IntFast8* t0 = IntFast8__init__(1);
struct ud4fc83Some* i4_self = malloc(sizeof(struct ud4fc83Some));
i4_self->ud4fc83data = IntFast8__copy__(t0);
i4_self->type_tag = 51;
ud4fc83b->f0 = i4_self;
if (((struct ud4fc83Some*)(ud4fc83b->f0))->type_tag == 51) {
IntFast8__deinit__(((struct ud4fc83Some*)(ud4fc83b->f0))->ud4fc83data);
free(ud4fc83b->f0);
}
else if (((struct ud4fc83Some*)(ud4fc83b->f1))->type_tag == 52) {
ud4fc83None__deinit__(ud4fc83b->f1);
}
free(ud4fc83b);
ud4fc83b = malloc(sizeof(union ud4fc83Maybe));
struct IntFast8* t1 = IntFast8__init__(2);
struct ud4fc83Some* i8_self = malloc(sizeof(struct ud4fc83Some));
i8_self->ud4fc83data = IntFast8__copy__(t1);
i8_self->type_tag = 51;
ud4fc83b->f0 = i8_self;
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
if (((struct ud4fc83Some*)(ud4fc83b->f0))->type_tag == 51) {
IntFast8__deinit__(((struct ud4fc83Some*)(ud4fc83b->f0))->ud4fc83data);
free(ud4fc83b->f0);
}
else if (((struct ud4fc83Some*)(ud4fc83b->f1))->type_tag == 52) {
ud4fc83None__deinit__(ud4fc83b->f1);
}
free(ud4fc83b);
return 0;
}
