#include <stdint.h>
void empty() {

}
int_fast8_t one() {
return 1;
}
int_fast8_t plusOne(int_fast8_t number) {
return number + one();
}
int_fast8_t plus(int_fast8_t expr1, int_fast8_t expr2) {
return expr1 + expr2;
}
int main(void) {
int_fast8_t tmp1 = one();
int_fast8_t tmp2 = one();
int_fast8_t tmp3 = one();
int_fast8_t tmp4 = plus(tmp2, tmp3);
int_fast8_t three = plus(tmp1, tmp4);
int_fast8_t four = plusOne(three);
empty();
return 0;
}