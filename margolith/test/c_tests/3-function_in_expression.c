#include <stdint.h>
int_fast8_t square(int_fast8_t number) {
return number * number;
}
int main(void) {
int_fast8_t a = 10 + 20;
int_fast8_t b = a + 1;
int_fast8_t c = square(b);
int_fast8_t tmp1 = square(c);
int_fast8_t d = square(tmp1) - square(2);
return 0;
}