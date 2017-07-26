#include <stdint.h>
void adr_func_empty() {

}
int_fast8_t adr_func_one() {
return 1;
}
int_fast8_t adr_func_plusOne(int_fast8_t adr_var_number) {
return adr_var_number + adr_func_one();
}
int_fast8_t adr_func_plus(int_fast8_t adr_var_expr1, int_fast8_t adr_var_expr2) {
return adr_var_expr1 + adr_var_expr2;
}
int main(void) {
int_fast8_t adr_tmp_tmp1 = adr_func_one();
int_fast8_t adr_tmp_tmp2 = adr_func_one();
int_fast8_t adr_tmp_tmp3 = adr_func_one();
int_fast8_t adr_tmp_tmp4 = adr_func_plus(adr_tmp_tmp2, adr_tmp_tmp3);
int_fast8_t adr_var_three = adr_func_plus(adr_tmp_tmp1, adr_tmp_tmp4);
int_fast8_t adr_var_four = adr_func_plusOne(adr_var_three);
adr_func_empty();
return 0;
}