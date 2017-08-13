#include <stdlib.h>
struct adr__Dummy {

};
int main(void) {
struct adr__Dummy* adr__self = malloc(sizeof(struct adr__Dummy));
struct adr__Dummy* adr__d = adr__self;
free(adr__d);
return 0;
}