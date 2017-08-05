#include <stdint.h>
#include <stdlib.h>
struct adr__UserStruct {

};
struct adr__UserStruct* adr__UserStruct__init__() {
struct adr__UserStruct* adr__self = malloc(sizeof(struct adr__UserStruct));
return adr__self;
}
void adr__UserStruct__deinit__(struct adr__UserStruct* adr__self) {
free(adr__self);
}
struct adr__UserStruct* adr__UserStruct__copy__(struct adr__UserStruct* adr__self) {
struct adr__UserStruct* adr__new = malloc(sizeof(struct adr__UserStruct));
return adr__new;
}
struct adr__UserStruct* adr__UserStruct__deepCopy__(struct adr__UserStruct* adr__self) {
struct adr__UserStruct* adr__new = malloc(sizeof(struct adr__UserStruct));
return adr__new;
}
struct adr__MyLinkedList {
uint_fast64_t adr__length;
void* adr__data;
};
int main(void) {
struct adr__UserStruct* adr__tmp_0 = adr__UserStruct__init__();
struct adr__UserStruct* adr__tmp_1 = adr__UserStruct__copy__(adr__tmp_0);
struct adr__MyLinkedList* adr__self = malloc(sizeof(struct adr__MyLinkedList));
adr__self->adr__length = 1;
adr__self->adr__data = adr__UserStruct__copy__(adr__tmp_1);
struct adr__MyLinkedList* adr__ll = adr__self;
adr__UserStruct__deinit__(adr__ll->adr__data);
free(adr__ll);
adr__UserStruct__deinit__(adr__tmp_0);
adr__UserStruct__deinit__(adr__tmp_1);
return 0;
}