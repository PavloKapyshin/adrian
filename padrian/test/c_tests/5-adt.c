#include <stdlib.h>
#include <stdint.h>
#include <adrian_c.h>
struct u52d71aSome {
void* u52d71adata;
uint_fast64_t type_tag;
};
struct u52d71aNone {
uint_fast64_t type_tag;
};
struct u52d71aNone* u52d71aNone__init__() {
struct u52d71aNone* self = malloc(sizeof(struct u52d71aNone));
self->type_tag = 52;
return self;
}
struct u52d71aNone* u52d71aNone__copy__(struct u52d71aNone* self) {
struct u52d71aNone* new = malloc(sizeof(struct u52d71aNone));
new->type_tag = 52;
return new;
}
void u52d71aNone__deinit__(struct u52d71aNone* self) {
free(self);
}
union u52d71aMaybe {
struct u52d71aSome* f0;
struct u52d71aNone* f1;
};
int main(void) {
union u52d71aMaybe* u52d71ab = malloc(sizeof(union u52d71aMaybe));
struct IntFast8* t0 = IntFast8__init__(1);
struct u52d71aSome* i4_self = malloc(sizeof(struct u52d71aSome));
i4_self->u52d71adata = IntFast8__copy__(t0);
i4_self->type_tag = 51;
u52d71ab->f0 = i4_self;
if (((struct u52d71aSome*)(u52d71ab->f0))->type_tag == 51) {
IntFast8__deinit__(((struct u52d71aSome*)(u52d71ab->f0))->u52d71adata);
free(u52d71ab->f0);
}
else if (((struct u52d71aNone*)(u52d71ab->f1))->type_tag == 52) {
u52d71aNone__deinit__(u52d71ab->f1);
}
free(u52d71ab);
u52d71ab = malloc(sizeof(union u52d71aMaybe));
struct IntFast8* t1 = IntFast8__init__(2);
struct u52d71aSome* i8_self = malloc(sizeof(struct u52d71aSome));
i8_self->u52d71adata = IntFast8__copy__(t1);
i8_self->type_tag = 51;
u52d71ab->f0 = i8_self;
union u52d71aMaybe* i9_new = malloc(sizeof(union u52d71aMaybe));
if (((struct u52d71aSome*)(u52d71ab->f0))->type_tag == 51) {
struct u52d71aSome* i10_new = malloc(sizeof(struct u52d71aSome));
i10_new->u52d71adata = IntFast8__copy__(((struct u52d71aSome*)(u52d71ab->f0))->u52d71adata);
i10_new->type_tag = 51;
i9_new->f0 = i10_new;
}
else if (((struct u52d71aNone*)(u52d71ab->f1))->type_tag == 52) {
i9_new->f1 = u52d71aNone__copy__(u52d71ab->f1);
}
union u52d71aMaybe* u52d71ac = i9_new;
IntFast8__deinit__(t0);
IntFast8__deinit__(t1);
if (((struct u52d71aSome*)(u52d71ab->f0))->type_tag == 51) {
IntFast8__deinit__(((struct u52d71aSome*)(u52d71ab->f0))->u52d71adata);
free(u52d71ab->f0);
}
else if (((struct u52d71aNone*)(u52d71ab->f1))->type_tag == 52) {
u52d71aNone__deinit__(u52d71ab->f1);
}
free(u52d71ab);
if (((struct u52d71aSome*)(u52d71ac->f0))->type_tag == 51) {
IntFast8__deinit__(((struct u52d71aSome*)(u52d71ac->f0))->u52d71adata);
free(u52d71ac->f0);
}
else if (((struct u52d71aNone*)(u52d71ac->f1))->type_tag == 52) {
u52d71aNone__deinit__(u52d71ac->f1);
}
free(u52d71ac);
return 0;
}
