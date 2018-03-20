#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct T {
void* field;
};
int main(void) {
struct IntFast8* t0 = IntFast8__init__(1);
struct T* i0_self = malloc(sizeof(struct T));
i0_self->field = IntFast8__copy__(t0);
struct T* justAVar = i0_self;
IntFast8__deinit__(justAVar->field);
free(justAVar);
IntFast8__deinit__(t0);
return 0;
}
