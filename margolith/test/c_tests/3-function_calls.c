#include <stdint.h>
#include <stdlib.h>
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
int_fast8_t* adr_u_zeroVar = adr_u_zero();
int_fast8_t* adr_t5 = adr_u_zero();
int_fast8_t* adr_u_oneVar = adr_u_addOne(adr_t5);
int_fast8_t* adr_t6 = adr_u_zero();
int_fast8_t* adr_t7 = adr_u_addOne(adr_t6);
int_fast8_t* adr_u_twoVar = adr_u_addOne(adr_t7);
int_fast8_t* adr_t8 = malloc(sizeof(int_fast8_t));
*adr_t8 = 3;
int_fast8_t* adr_t9 = malloc(sizeof(int_fast8_t));
*adr_t9 = 2;
int_fast8_t* adr_u_fiveVar = adr_u_add(adr_t8, adr_t9);
int_fast8_t* adr_u_sixVar = adr_u_increment(adr_u_fiveVar, adr_u_oneVar);
int_fast8_t* adr_u_sevenVar = adr_u_addSome(adr_u_fiveVar, adr_u_twoVar);
free(adr_u_fiveVar);
free(adr_u_oneVar);
free(adr_u_sevenVar);
free(adr_u_sixVar);
free(adr_t5);
free(adr_t6);
free(adr_t7);
free(adr_t8);
free(adr_t9);
free(adr_u_twoVar);
free(adr_u_zeroVar);
return 0;
}