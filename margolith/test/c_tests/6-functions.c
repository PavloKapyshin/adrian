#include <stdint.h>
void adr__empty() {

}
int_fast8_t adr__one() {
return 1;
}
int_fast8_t adr__plusOne(int_fast8_t adr__number) {
return adr__number + adr__one();
}
int_fast8_t adr__plus(int_fast8_t adr__expr1, int_fast8_t adr__expr2) {
return adr__expr1 + adr__expr2;
}
int main(void) {
int_fast8_t adr__tmp_0 = adr__one();
int_fast8_t adr__tmp_1 = adr__one();
int_fast8_t adr__tmp_2 = adr__one();
int_fast8_t adr__tmp_3 = adr__plus(adr__tmp_1, adr__tmp_2);
int_fast8_t adr__three = adr__plus(adr__tmp_0, adr__tmp_3);
int_fast8_t adr__four = adr__plusOne(adr__three);
adr__empty();
return 0;
}