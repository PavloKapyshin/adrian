#include <stdint.h>
#include <adrian_c.h>
int main(void) {
struct IntFast8* u263621a = IntFast8__init__(1);
struct IntFast8* u263621conditional = IntFast8__init__(10);
struct IntFast8* t0 = IntFast8__init__(0);
struct IntFast8* t1 = IntFast8__gt__(u263621conditional, t0);
struct IntFast8* t2 = IntFast8__init__(1);
struct IntFast8* t3 = IntFast8__gt__(u263621conditional, t2);
struct IntFast8* t4 = IntFast8__or__(t1, t3);
while (t4->literal) {
struct IntFast8* t5 = IntFast8__copy__(u263621a);
struct IntFast8* t6 = IntFast8__init__(1);
IntFast8__deinit__(u263621a);
u263621a = IntFast8__add__(t5, t6);
struct IntFast8* t7 = IntFast8__copy__(u263621conditional);
struct IntFast8* t8 = IntFast8__init__(1);
IntFast8__deinit__(u263621conditional);
u263621conditional = IntFast8__sub__(t7, t8);
IntFast8__deinit__(t0);
t0 = IntFast8__init__(0);
IntFast8__deinit__(t1);
t1 = IntFast8__gt__(u263621conditional, t0);
IntFast8__deinit__(t2);
t2 = IntFast8__init__(1);
IntFast8__deinit__(t3);
t3 = IntFast8__gt__(u263621conditional, t2);
IntFast8__deinit__(t4);
t4 = IntFast8__or__(t1, t3);
IntFast8__deinit__(t5);
IntFast8__deinit__(t6);
IntFast8__deinit__(t7);
IntFast8__deinit__(t8);
}
struct IntFast8* u263621b = IntFast8__copy__(u263621a);
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
IntFast8__deinit__(t2);
IntFast8__deinit__(t3);
IntFast8__deinit__(t4);
IntFast8__deinit__(u263621a);
IntFast8__deinit__(u263621b);
IntFast8__deinit__(u263621conditional);
return 0;
}
