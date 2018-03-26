#include <stdint.h>
#include <adrian_c.h>
struct IntFast8* f() {
struct IntFast8* a = IntFast8__init__(2);
struct IntFast8* t0 = IntFast8__init__(1);
if (t0->literal) {
struct IntFast8* t1 = IntFast8__init__(1);
if (t1->literal) {
IntFast8__deinit__(a);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
return IntFast8__init__(1);
}
struct IntFast8* b = IntFast8__init__(1);
IntFast8__deinit__(a);
IntFast8__deinit__(b);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
return IntFast8__init__(0);
}
else {
struct IntFast8* c = IntFast8__init__(2);
IntFast8__deinit__(c);
}
IntFast8__deinit__(a);
IntFast8__deinit__(t0);
return IntFast8__init__(1);
}
int main(void) {
struct IntFast8* a = f();
IntFast8__deinit__(a);
return 0;
}
