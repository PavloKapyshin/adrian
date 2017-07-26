#include <stdint.h>
int_fast8_t adr_func_square(int_fast8_t adr_var_number) {
return adr_var_number * adr_var_number;
}
int main(void) {
int_fast8_t adr_var_a = 10 + 20;
int_fast8_t adr_var_b = adr_var_a + 1;
int_fast8_t adr_var_c = adr_func_square(adr_var_b);
int_fast8_t adr_tmp_tmp1 = adr_func_square(adr_var_c);
int_fast8_t adr_var_d = adr_func_square(adr_tmp_tmp1) - adr_func_square(2);
return 0;
}