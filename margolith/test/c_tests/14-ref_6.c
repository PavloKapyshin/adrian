#include <stdint.h>
#include <stdlib.h>
struct adr_struct_Person {
uint_fast64_t adr_field_id;
uint_fast8_t adr_field_age;
};
struct adr_struct_Person* adr_func_Person__init__(uint_fast64_t adr_var_id, uint_fast8_t adr_var_age) {
struct adr_struct_Person* adr_var_self = malloc(sizeof(struct adr_struct_Person));
adr_var_self->adr_field_id = adr_var_id;
adr_var_self->adr_field_age = adr_var_age;
return adr_var_self;
}
void adr_func_Person__deinit__(struct adr_struct_Person* adr_var_self) {
free(adr_var_self);
}
struct adr_struct_Person* adr_func_Person__copy__(struct adr_struct_Person* adr_var_self) {
struct adr_struct_Person* adr_var_new = malloc(sizeof(struct adr_struct_Person));
adr_var_new->adr_field_id = adr_var_self->adr_field_id;
adr_var_new->adr_field_age = adr_var_self->adr_field_age;
return adr_var_new;
}
struct adr_struct_Person* adr_func_Person__deepCopy__(struct adr_struct_Person* adr_var_self) {
struct adr_struct_Person* adr_var_new = malloc(sizeof(struct adr_struct_Person));
adr_var_new->adr_field_id = adr_var_self->adr_field_id;
adr_var_new->adr_field_age = adr_var_self->adr_field_age;
return adr_var_new;
}
struct adr_struct_HighPerson {
struct adr_struct_Person* adr_field_person;
};
void adr_func_HighPerson__deinit__(struct adr_struct_HighPerson* adr_var_self) {
free(adr_var_self);
}
struct adr_struct_HighPerson* adr_func_HighPerson__copy__(struct adr_struct_HighPerson* adr_var_self) {
struct adr_struct_HighPerson* adr_var_new = malloc(sizeof(struct adr_struct_HighPerson));
adr_var_new->adr_field_person = adr_var_self->adr_field_person;
return adr_var_new;
}
struct adr_struct_HighPerson* adr_func_HighPerson__deepCopy__(struct adr_struct_HighPerson* adr_var_self) {
struct adr_struct_HighPerson* adr_var_new = malloc(sizeof(struct adr_struct_HighPerson));
struct adr_struct_Person* adr_tmp_tmp1 = adr_func_Person__copy__(adr_var_self->adr_field_person);
adr_var_new->adr_field_person = adr_func_Person__deepCopy__(adr_tmp_tmp1);
adr_func_Person__deinit__(adr_tmp_tmp1);
return adr_var_new;
}
struct adr_struct_HighPerson* adr_func_HighPerson__init__(struct adr_struct_Person* adr_var_person) {
struct adr_struct_HighPerson* adr_var_self = malloc(sizeof(struct adr_struct_HighPerson));
adr_var_self->adr_field_person = adr_var_person;
return adr_var_self;
}
struct adr_struct_HighPerson* adr_func_doSomethingWithJohn(struct adr_struct_Person* adr_var_john) {
return adr_func_HighPerson__init__(adr_var_john);
}
int main(void) {
struct adr_struct_Person* adr_var_john = adr_func_Person__init__(1, 20);
struct adr_struct_Person* adr_var_secondJohn = adr_var_john;
struct adr_struct_HighPerson* adr_var_anotherJohn = adr_func_doSomethingWithJohn(adr_var_secondJohn);
adr_func_HighPerson__deinit__(adr_var_anotherJohn);
adr_func_Person__deinit__(adr_var_john);
return 0;
}