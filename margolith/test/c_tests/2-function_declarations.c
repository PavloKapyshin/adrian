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
return 0;
}