#include <stdint.h>
#include <stdlib.h>
struct adr_struct_Id {
uint_fast64_t adr_field_realId;
};
struct adr_struct_Id* adr_func_Id__copy__(struct adr_struct_Id* adr_var_self) {
struct adr_struct_Id* adr_var_new = malloc(sizeof(struct adr_struct_Id));
adr_var_new->adr_field_realId = adr_var_self->adr_field_realId;
return adr_var_new;
}
void adr_func_Id__deinit__(struct adr_struct_Id* adr_var_self) {
free(adr_var_self);
}
struct adr_struct_Id* adr_func_Id__init__(uint_fast64_t adr_var_realId) {
struct adr_struct_Id* adr_var_self = malloc(sizeof(struct adr_struct_Id));
adr_var_self->adr_field_realId = adr_var_realId;
return adr_var_self;
}
uint_fast64_t adr_func_IdgetId(struct adr_struct_Id* adr_var_self) {
return adr_var_self->adr_field_realId;
}
struct adr_struct_Person {
struct adr_struct_Id* adr_field_id;
uint_fast8_t adr_field_age;
};
struct adr_struct_Person* adr_func_Person__copy__(struct adr_struct_Person* adr_var_self) {
struct adr_struct_Person* adr_var_new = malloc(sizeof(struct adr_struct_Person));
adr_var_new->adr_field_id = adr_func_Id__copy__(adr_var_self->adr_field_id);
adr_var_new->adr_field_age = adr_var_self->adr_field_age;
return adr_var_new;
}
void adr_func_Person__deinit__(struct adr_struct_Person* adr_var_self) {
adr_func_Id__deinit__(adr_var_self->adr_field_id);
free(adr_var_self);
}
struct adr_struct_Person* adr_func_Person__init__(struct adr_struct_Id* adr_var_id, uint_fast8_t adr_var_age) {
struct adr_struct_Person* adr_var_self = malloc(sizeof(struct adr_struct_Person));
adr_var_self->adr_field_id = adr_var_id;
adr_var_self->adr_field_age = adr_var_age;
return adr_var_self;
}
int main(void) {
struct adr_struct_Id* adr_tmp_tmp1 = adr_func_Id__init__(1);
struct adr_struct_Person* adr_var_jack = adr_func_Person__init__(adr_tmp_tmp1, 20);
struct adr_struct_Id* adr_var_johnId = adr_func_Id__init__(2);
struct adr_struct_Id* adr_tmp_tmp2 = adr_func_Id__copy__(adr_var_johnId);
struct adr_struct_Person* adr_var_john = adr_func_Person__init__(adr_tmp_tmp2, 20);
adr_func_Person__deinit__(adr_var_jack);
adr_func_Person__deinit__(adr_var_john);
adr_func_Id__deinit__(adr_var_johnId);
return 0;
}