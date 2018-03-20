#include <stdint.h>
#include <adrian_c.h>
struct UIntFast64* power(struct UIntFast64* a) {
return UIntFast64__mul__(a, a);
}
int main(void) {
return 0;
}
