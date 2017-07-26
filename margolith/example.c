#include <stdint.h>
#include <stdlib.h>
struct adr_struct_Person {
uint_fast64_t adr_field_id;
uint_fast8_t adr_field_age;
};
struct adr_struct_Person* adr_func_Person__copy__(struct adr_struct_Person* adr_var_self) {
struct adr_struct_Person* adr_var_new = malloc(sizeof(struct adr_struct_Person));
adr_var_new->adr_field_id = adr_var_self->adr_field_id;
adr_var_new->adr_field_age = adr_var_self->adr_field_age;
return adr_var_new;
}
void adr_func_Person__deinit__(struct adr_struct_Person* adr_var_self) {
free(adr_var_self);
}
struct adr_struct_Person* adr_func_Person__init__(uint_fast64_t adr_var_id, uint_fast8_t adr_var_age) {
struct adr_struct_Person* adr_var_self = malloc(sizeof(struct adr_struct_Person));
adr_var_self->adr_field_id = adr_var_id;
adr_var_self->adr_field_age = adr_var_age;
return adr_var_self;
}
void adr_func_PersonsetAge(struct adr_struct_Person* adr_var_self, uint_fast8_t adr_var_age) {
adr_var_self->adr_field_age = adr_var_age;
}
void adr_func_PersonaddOneYear(struct adr_struct_Person* adr_var_self) {
adr_func_PersonsetAge(adr_var_self, adr_var_self->adr_field_age + 1);
}
uint_fast8_t adr_func_PersongetAge(struct adr_struct_Person* adr_var_self) {
return adr_var_self->adr_field_age;
}
int main(void) {
return 0;
}