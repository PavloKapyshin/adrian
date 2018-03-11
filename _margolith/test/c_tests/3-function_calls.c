#include <stdint.h>
#include "adrian_c.h"
struct IntFast8* adr_u_zero() {
struct IntFast8* adr_t0 = IntFast8___init__(0);
return adr_t0;
}
struct IntFast8* adr_u_addOne(struct IntFast8* adr_u_src) {
struct IntFast8* adr_t1 = IntFast8___init__(1);
struct IntFast8* adr_t2 = IntFast8___add__(adr_u_src, adr_t1);
IntFast8___deinit__(adr_t1);
return adr_t2;
}
struct IntFast8* adr_u_add(struct IntFast8* adr_u_lvalue, struct IntFast8* adr_u_rvalue) {
struct IntFast8* adr_t3 = IntFast8___add__(adr_u_lvalue, adr_u_rvalue);
return adr_t3;
}
struct IntFast8* adr_u_addSome(struct IntFast8* adr_u_lvalue, struct IntFast8* adr_u_rvalue) {
struct IntFast8* adr_u_result = IntFast8___add__(adr_u_lvalue, adr_u_rvalue);
return adr_u_result;
}
struct IntFast8* adr_u_increment(struct IntFast8* adr_u_value, struct IntFast8* adr_u_by) {
struct IntFast8* adr_u_copyOfBy = IntFast8___copy__(adr_u_by);
struct IntFast8* adr_t4 = IntFast8___add__(adr_u_value, adr_u_copyOfBy);
IntFast8___deinit__(adr_u_copyOfBy);
return adr_t4;
}
int main(void) {
struct IntFast8* adr_u_zeroVar = adr_u_zero();
struct IntFast8* adr_t5 = adr_u_zero();
struct IntFast8* adr_u_oneVar = adr_u_addOne(adr_t5);
struct IntFast8* adr_t6 = adr_u_zero();
struct IntFast8* adr_t7 = adr_u_addOne(adr_t6);
struct IntFast8* adr_u_twoVar = adr_u_addOne(adr_t7);
struct IntFast8* adr_t8 = IntFast8___init__(3);
struct IntFast8* adr_t9 = IntFast8___init__(2);
struct IntFast8* adr_u_fiveVar = adr_u_add(adr_t8, adr_t9);
struct IntFast8* adr_u_sixVar = adr_u_increment(adr_u_fiveVar, adr_u_oneVar);
struct IntFast8* adr_u_sevenVar = adr_u_addSome(adr_u_fiveVar, adr_u_twoVar);
IntFast8___deinit__(adr_u_zeroVar);
IntFast8___deinit__(adr_t5);
IntFast8___deinit__(adr_u_oneVar);
IntFast8___deinit__(adr_t6);
IntFast8___deinit__(adr_t7);
IntFast8___deinit__(adr_u_twoVar);
IntFast8___deinit__(adr_t8);
IntFast8___deinit__(adr_t9);
IntFast8___deinit__(adr_u_fiveVar);
IntFast8___deinit__(adr_u_sixVar);
IntFast8___deinit__(adr_u_sevenVar);
return 0;
}
