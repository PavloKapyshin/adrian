#include <stdint.h>
#include <stdlib.h>
struct adr_u_Generic {
void* adr_u_data;
};
struct adr_u_T {
void* adr_u_genericData;
uint_fast64_t* adr_u_length;
};
int main(void) {
int_fast8_t* adr_t1 = malloc(sizeof(int_fast8_t));
*adr_t1 = 0;
struct adr_u_Generic* adr_t7 = malloc(sizeof(struct adr_u_Generic));
adr_t7->adr_u_data = malloc(sizeof(int_fast8_t));
*(int_fast8_t*)(adr_t7->adr_u_data) = *adr_t1;
struct adr_u_Generic* adr_u_a = adr_t7;
struct adr_u_Generic* adr_t8 = malloc(sizeof(struct adr_u_Generic));
int_fast8_t* adr_t9 = malloc(sizeof(int_fast8_t));
*adr_t9 = *(int_fast8_t*)(adr_u_a->adr_u_data);
adr_t8->adr_u_data = malloc(sizeof(int_fast8_t));
*(int_fast8_t*)(adr_t8->adr_u_data) = *adr_t9;
free(adr_t9);
struct adr_u_Generic* adr_u_b = adr_t8;
int_fast8_t* adr_t2 = malloc(sizeof(int_fast8_t));
*adr_t2 = 0;
struct adr_u_Generic* adr_t10 = malloc(sizeof(struct adr_u_Generic));
adr_t10->adr_u_data = malloc(sizeof(int_fast8_t));
*(int_fast8_t*)(adr_t10->adr_u_data) = *adr_t2;
struct adr_u_Generic* adr_u_d = adr_t10;
*(int_fast8_t*)(adr_u_d->adr_u_data) = 2;
int_fast8_t* adr_t5 = malloc(sizeof(int_fast8_t));
*adr_t5 = 12;
uint_fast64_t* adr_t6 = malloc(sizeof(uint_fast64_t));
*adr_t6 = 1;
struct adr_u_T* adr_t11 = malloc(sizeof(struct adr_u_T));
adr_t11->adr_u_genericData = malloc(sizeof(int_fast8_t));
*(int_fast8_t*)(adr_t11->adr_u_genericData) = *adr_t5;
adr_t11->adr_u_length = malloc(sizeof(uint_fast64_t));
*adr_t11->adr_u_length = *adr_t6;
struct adr_u_T* adr_u_c = adr_t11;
free(adr_u_a->adr_u_data);
free(adr_u_a);
free(adr_u_b->adr_u_data);
free(adr_u_b);
free(adr_u_c->adr_u_genericData);
free(adr_u_c->adr_u_length);
free(adr_u_c);
free(adr_u_d->adr_u_data);
free(adr_u_d);
free(adr_t1);
free(adr_t2);
free(adr_t5);
free(adr_t6);
return 0;
}