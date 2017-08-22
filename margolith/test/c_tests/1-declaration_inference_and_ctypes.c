#include <stdint.h>
#include <stdlib.h>
int main(void) {
int_fast8_t* adr_u_m1 = malloc(sizeof(int_fast8_t));
*adr_u_m1 = 10;
int_fast8_t* adr_u_mTypeInference = malloc(sizeof(int_fast8_t));
*adr_u_mTypeInference = 10;
int_fast8_t* adr_u_m3 = malloc(sizeof(int_fast8_t));
*adr_u_m3 = 0;
uint_fast8_t* adr_u_m4 = malloc(sizeof(uint_fast8_t));
*adr_u_m4 = 0;
int_fast16_t* adr_u_m5 = malloc(sizeof(int_fast16_t));
*adr_u_m5 = 0;
uint_fast16_t* adr_u_m6 = malloc(sizeof(uint_fast16_t));
*adr_u_m6 = 0;
int_fast32_t* adr_u_m7 = malloc(sizeof(int_fast32_t));
*adr_u_m7 = 0;
uint_fast32_t* adr_u_m8 = malloc(sizeof(uint_fast32_t));
*adr_u_m8 = 0;
int_fast64_t* adr_u_m9 = malloc(sizeof(int_fast64_t));
*adr_u_m9 = 0;
uint_fast64_t* adr_u_m10 = malloc(sizeof(uint_fast64_t));
*adr_u_m10 = 0;
int_fast8_t* adr_u_mName = malloc(sizeof(int_fast8_t));
*adr_u_mName = *adr_u_m3;
free(adr_u_m1);
free(adr_u_m10);
free(adr_u_m3);
free(adr_u_m4);
free(adr_u_m5);
free(adr_u_m6);
free(adr_u_m7);
free(adr_u_m8);
free(adr_u_m9);
free(adr_u_mName);
free(adr_u_mTypeInference);
return 0;
}