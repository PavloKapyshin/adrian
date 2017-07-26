#include <stdint.h>
#include <stdlib.h>
struct adr_struct_Person {
uint_fast32_t adr_field_id;
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
struct adr_struct_Person* adr_func_Person__init__(uint_fast32_t adr_var_id, uint_fast8_t adr_var_age) {
struct adr_struct_Person* adr_var_self = malloc(sizeof(struct adr_struct_Person));
adr_var_self->adr_field_id = adr_var_id;
adr_var_self->adr_field_age = adr_var_age;
return adr_var_self;
}
struct adr_struct_P {
struct adr_struct_Person* adr_field_person;
uint_fast64_t adr_field_id;
};
struct adr_struct_P* adr_func_P__copy__(struct adr_struct_P* adr_var_self) {
struct adr_struct_P* adr_var_new = malloc(sizeof(struct adr_struct_P));
adr_var_new->adr_field_person = adr_var_self->adr_field_person;
adr_var_new->adr_field_id = adr_var_self->adr_field_id;
return adr_var_new;
}
void adr_func_P__deinit__(struct adr_struct_P* adr_var_self) {
adr_func_Person__deinit__(adr_var_self->adr_field_person);
free(adr_var_self);
}
struct adr_struct_P* adr_func_P__init__(struct adr_struct_Person* adr_var_person, uint_fast64_t adr_var_id) {
struct adr_struct_P* adr_var_self = malloc(sizeof(struct adr_struct_P));
adr_var_self->adr_field_person = adr_func_Person__copy__(adr_var_person);
adr_var_self->adr_field_id = adr_var_id;
return adr_var_self;
}
int main(void) {
struct adr_struct_Person* adr_tmp_tmp1 = adr_func_Person__init__(1, 30);
struct adr_struct_P* adr_var_p = adr_func_P__init__(adr_tmp_tmp1, 1);
adr_func_Person__deinit__(adr_tmp_tmp1);
adr_func_P__deinit__(adr_var_p);
return 0;
}