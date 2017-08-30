#include <stdint.h>
#include <stdlib.h>
void adr_u_seq() {
int_fast8_t* adr_u_t0 = malloc(sizeof(int_fast8_t));
*adr_u_t0 = 1;
free(adr_u_t0);
}
int_fast8_t* adr_u_zero() {
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = 0;
return adr_t0;
}
int_fast8_t* adr_u_addOne(int_fast8_t* adr_u_src) {
int_fast8_t* adr_t1 = malloc(sizeof(int_fast8_t));
*adr_t1 = 1;
int_fast8_t* adr_t2 = malloc(sizeof(int_fast8_t));
*adr_t2 = *adr_u_src + *adr_t1;
free(adr_t1);
return adr_t2;
}
int_fast8_t* adr_u_add(int_fast8_t* adr_u_lvalue, int_fast8_t* adr_u_rvalue) {
int_fast8_t* adr_t3 = malloc(sizeof(int_fast8_t));
*adr_t3 = *adr_u_lvalue + *adr_u_rvalue;
return adr_t3;
}
int_fast8_t* adr_u_addSome(int_fast8_t* adr_u_lvalue, int_fast8_t* adr_u_rvalue) {
int_fast8_t* adr_u_result = malloc(sizeof(int_fast8_t));
*adr_u_result = *adr_u_lvalue + *adr_u_rvalue;
return adr_u_result;
}
int_fast8_t* adr_u_increment(int_fast8_t* adr_u_value, int_fast8_t* adr_u_by) {
int_fast8_t* adr_u_copyOfBy = malloc(sizeof(int_fast8_t));
*adr_u_copyOfBy = *adr_u_by;
int_fast8_t* adr_t4 = malloc(sizeof(int_fast8_t));
*adr_t4 = *adr_u_value + *adr_u_copyOfBy;
free(adr_u_copyOfBy);
return adr_t4;
}
int main(void) {
return 0;
}