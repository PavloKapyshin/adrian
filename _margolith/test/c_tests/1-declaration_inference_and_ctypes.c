#include <stdint.h>
#include "adrian_c.h"
int main(void) {
struct IntFast8* adr_u_m1 = IntFast8___init__(10);
struct IntFast8* adr_u_mTypeInference = IntFast8___init__(10);
struct IntFast8* adr_u_m3 = IntFast8___init__(0);
struct UIntFast8* adr_u_m4 = UIntFast8___init__(0);
struct IntFast16* adr_u_m5 = IntFast16___init__(0);
struct UIntFast16* adr_u_m6 = UIntFast16___init__(0);
struct IntFast32* adr_u_m7 = IntFast32___init__(0);
struct UIntFast32* adr_u_m8 = UIntFast32___init__(0);
struct IntFast64* adr_u_m9 = IntFast64___init__(0);
struct UIntFast64* adr_u_m10 = UIntFast64___init__(0);
struct IntFast8* adr_u_mName = IntFast8___copy__(adr_u_m3);
IntFast8___deinit__(adr_u_m1);
IntFast8___deinit__(adr_u_mTypeInference);
IntFast8___deinit__(adr_u_m3);
UIntFast8___deinit__(adr_u_m4);
IntFast16___deinit__(adr_u_m5);
UIntFast16___deinit__(adr_u_m6);
IntFast32___deinit__(adr_u_m7);
UIntFast32___deinit__(adr_u_m8);
IntFast64___deinit__(adr_u_m9);
UIntFast64___deinit__(adr_u_m10);
IntFast8___deinit__(adr_u_mName);
return 0;
}
