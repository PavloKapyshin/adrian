#include <stdint.h>
#include <adrian_c.h>
int main(void) {
struct IntFast8* a = IntFast8__init__(0);
struct UIntFast8* b = UIntFast8__init__(1);
UIntFast8__deinit__(b);
b = UIntFast8__init__(5);
struct UIntFast8* c = UIntFast8__copy__(b);
struct UIntFast8* d = c;
IntFast8__deinit__(a);
UIntFast8__deinit__(b);
UIntFast8__deinit__(c);
return 0;
}
