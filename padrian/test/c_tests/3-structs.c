#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct u55fcbfDate {
struct UIntFast8* u55fcbfday;
struct UIntFast8* u55fcbfmonth;
struct UIntFast64* u55fcbfyear;
uint_fast64_t type_tag;
};
struct u55fcbfDate* u55fcbfDate__init__(struct UIntFast8* u55fcbfday, struct UIntFast8* u55fcbfmonth, struct UIntFast64* u55fcbfyear) {
struct u55fcbfDate* self = malloc(sizeof(struct u55fcbfDate));
self->u55fcbfday = UIntFast8__copy__(u55fcbfday);
self->u55fcbfmonth = UIntFast8__copy__(u55fcbfmonth);
self->u55fcbfyear = UIntFast64__copy__(u55fcbfyear);
self->type_tag = 51;
return self;
}
struct u55fcbfDate* u55fcbfDate__copy__(struct u55fcbfDate* self) {
struct u55fcbfDate* new = malloc(sizeof(struct u55fcbfDate));
new->u55fcbfday = UIntFast8__copy__(self->u55fcbfday);
new->u55fcbfmonth = UIntFast8__copy__(self->u55fcbfmonth);
new->u55fcbfyear = UIntFast64__copy__(self->u55fcbfyear);
self->type_tag = 51;
return new;
}
void u55fcbfDate__deinit__(struct u55fcbfDate* self) {
UIntFast8__deinit__(self->u55fcbfday);
UIntFast8__deinit__(self->u55fcbfmonth);
UIntFast64__deinit__(self->u55fcbfyear);
free(self);
}
void u55fcbfDate_u55fcbffirstDayOfMonth(struct u55fcbfDate* self, struct UIntFast8* u55fcbfmonth) {
UIntFast8__deinit__(self->u55fcbfday);
self->u55fcbfday = UIntFast8__init__(1);
UIntFast8__deinit__(self->u55fcbfmonth);
self->u55fcbfmonth = UIntFast8__copy__(u55fcbfmonth);
}
void u55fcbfDate_u55fcbflastDayOfYear(struct u55fcbfDate* self, struct UIntFast64* u55fcbfyear) {
UIntFast8__deinit__(self->u55fcbfday);
self->u55fcbfday = UIntFast8__init__(31);
UIntFast8__deinit__(self->u55fcbfmonth);
self->u55fcbfmonth = UIntFast8__init__(12);
UIntFast64__deinit__(self->u55fcbfyear);
self->u55fcbfyear = UIntFast64__copy__(u55fcbfyear);
}
void u55fcbfDate_u55fcbffirstDayOfCurrentMonth(struct u55fcbfDate* self) {
u55fcbfDate_u55fcbffirstDayOfMonth(self, self->u55fcbfmonth);
}
int main(void) {
return 0;
}
