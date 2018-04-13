#include <stdint.h>
#include <adrian_c.h>
int main(void) {
struct IntFast8* ua0ea16a = IntFast8__init__(0);
struct UIntFast8* ua0ea16b = UIntFast8__init__(1);
UIntFast8__deinit__(ua0ea16b);
ua0ea16b = UIntFast8__init__(5);
struct UIntFast8* ua0ea16c = UIntFast8__copy__(ua0ea16b);
struct UIntFast8* ua0ea16d = ua0ea16c;
IntFast8__deinit__(ua0ea16a);
UIntFast8__deinit__(ua0ea16b);
UIntFast8__deinit__(ua0ea16c);
return 0;
}
