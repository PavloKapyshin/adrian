#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct Date {
struct UIntFast8* day;
struct UIntFast8* month;
struct UIntFast64* year;
};
struct Date* Date__init__(struct UIntFast8* day, struct UIntFast8* month, struct UIntFast64* year) {
struct Date* self = malloc(sizeof(struct Date));
self->day = UIntFast8__copy__(day);
self->month = UIntFast8__copy__(month);
self->year = UIntFast64__copy__(year);
return self;
}
struct Date* Date__copy__(struct Date* self) {
struct Date* new = malloc(sizeof(struct Date));
new->day = UIntFast8__copy__(self->day);
new->month = UIntFast8__copy__(self->month);
new->year = UIntFast64__copy__(self->year);
return new;
}
void Date__deinit__(struct Date* self) {
UIntFast8__deinit__(self->day);
UIntFast8__deinit__(self->month);
UIntFast64__deinit__(self->year);
free(self);
}
void Date_firstDayOfMonth(struct Date* self, struct UIntFast8* month) {
UIntFast8__deinit__(self->day);
self->day = UIntFast8__init__(1);
UIntFast8__deinit__(self->month);
self->month = UIntFast8__copy__(month);
}
void Date_lastDayOfYear(struct Date* self, struct UIntFast64* year) {
UIntFast8__deinit__(self->day);
self->day = UIntFast8__init__(31);
UIntFast8__deinit__(self->month);
self->month = UIntFast8__init__(12);
UIntFast64__deinit__(self->year);
self->year = UIntFast64__copy__(year);
}
void Date_firstDayOfCurrentMonth(struct Date* self) {
Date_firstDayOfMonth(self, self->month);
}
int main(void) {
return 0;
}
