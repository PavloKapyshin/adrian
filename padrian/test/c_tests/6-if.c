#include <stdint.h>
#include <adrian_c.h>
struct IntFast8* u34fa7ef() {
struct IntFast8* u34fa7ea = IntFast8__init__(2);
struct IntFast8* t0 = IntFast8__init__(1);
if (t0->literal) {
struct IntFast8* t1 = IntFast8__init__(1);
if (t1->literal) {
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(u34fa7ea);
return IntFast8__init__(1);
}
struct IntFast8* u34fa7eb = IntFast8__init__(1);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(u34fa7ea);
IntFast8__deinit__(u34fa7eb);
return IntFast8__init__(0);
}
else {
struct IntFast8* u34fa7ec = IntFast8__init__(2);
IntFast8__deinit__(u34fa7ec);
}
IntFast8__deinit__(t0);
IntFast8__deinit__(u34fa7ea);
return IntFast8__init__(1);
}
int main(void) {
struct IntFast8* u34fa7ea = u34fa7ef();
IntFast8__deinit__(u34fa7ea);
return 0;
}
