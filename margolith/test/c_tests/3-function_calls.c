#include <stdint.h>
#include <stdlib.h>
int_fast8_t* adr_u_zero() {
int_fast8_t* adr_t0 = malloc(sizeof(int_fast8_t));
*adr_t0 = 0;
return adr_t0;
}
int_fast8_t* adr_u_addOne(int_fast8_t* adr_u_src) {
int_fast8_t* adr_t1 = malloc(sizeof(int_fast8_t));
*adr_t1 = *adr_u_src;
int_fast8_t* adr_t2 = malloc(sizeof(int_fast8_t));
*adr_t2 = 1;
int_fast8_t* adr_t3 = malloc(sizeof(int_fast8_t));
*adr_t3 = *adr_t1 + *adr_t2;
free(adr_t1);
free(adr_t2);
return adr_t3;
}
int_fast8_t* adr_u_add(int_fast8_t* adr_u_lvalue, int_fast8_t* adr_u_rvalue) {
int_fast8_t* adr_t4 = malloc(sizeof(int_fast8_t));
*adr_t4 = *adr_u_lvalue;
int_fast8_t* adr_t5 = malloc(sizeof(int_fast8_t));
*adr_t5 = *adr_u_rvalue;
int_fast8_t* adr_t6 = malloc(sizeof(int_fast8_t));
*adr_t6 = *adr_t4 + *adr_t5;
free(adr_t4);
free(adr_t5);
return adr_t6;
}
int_fast8_t* adr_u_increment(int_fast8_t* adr_u_value, int_fast8_t* adr_u_by) {
int_fast8_t* adr_u_copyOfBy = malloc(sizeof(int_fast8_t));
*adr_u_copyOfBy = *adr_u_by;
int_fast8_t* adr_t7 = malloc(sizeof(int_fast8_t));
*adr_t7 = *adr_u_value;
int_fast8_t* adr_t8 = malloc(sizeof(int_fast8_t));
*adr_t8 = *adr_u_copyOfBy;
int_fast8_t* adr_t9 = malloc(sizeof(int_fast8_t));
*adr_t9 = *adr_t7 + *adr_t8;
free(adr_u_copyOfBy);
free(adr_t7);
free(adr_t8);
return adr_t9;
}
int main(void) {
int_fast8_t* adr_u_zeroVar = adr_u_zero();
int_fast8_t* adr_t10 = adr_u_zero();
int_fast8_t* adr_u_oneVar = adr_u_addOne(adr_t10);
int_fast8_t* adr_t11 = adr_u_zero();
int_fast8_t* adr_t12 = adr_u_addOne(adr_t11);
int_fast8_t* adr_u_twoVar = adr_u_addOne(adr_t12);
int_fast8_t* adr_t13 = malloc(sizeof(int_fast8_t));
*adr_t13 = 3;
int_fast8_t* adr_t14 = malloc(sizeof(int_fast8_t));
*adr_t14 = 2;
int_fast8_t* adr_u_fiveVar = adr_u_add(adr_t13, adr_t14);
int_fast8_t* adr_t15 = malloc(sizeof(int_fast8_t));
*adr_t15 = *adr_u_fiveVar;
int_fast8_t* adr_t16 = malloc(sizeof(int_fast8_t));
*adr_t16 = *adr_u_oneVar;
int_fast8_t* adr_u_sixVar = adr_u_increment(adr_t15, adr_t16);
free(adr_u_fiveVar);
free(adr_u_oneVar);
free(adr_u_sixVar);
free(adr_t10);
free(adr_t11);
free(adr_t12);
free(adr_t13);
free(adr_t14);
free(adr_t15);
free(adr_t16);
free(adr_u_twoVar);
free(adr_u_zeroVar);
return 0;
}