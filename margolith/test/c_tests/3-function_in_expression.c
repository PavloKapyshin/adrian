#include <stdint.h>
int_fast8_t adr__square(int_fast8_t adr__number) {
return adr__number * adr__number;
}
int main(void) {
int_fast8_t adr__a = 10 + 20;
int_fast8_t adr__b = adr__a + 1;
int_fast8_t adr__c = adr__square(adr__b);
int_fast8_t adr__tmp_0 = adr__square(adr__c);
int_fast8_t adr__d = adr__square(adr__tmp_0) - adr__square(2);
return 0;
}