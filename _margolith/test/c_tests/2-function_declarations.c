#include <stdint.h>
#include "adrian_c.h"
void adr_u_seq() {
struct IntFast8* adr_u_t0 = IntFast8___init__(1);
IntFast8___deinit__(adr_u_t0);
}
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
return 0;
}
