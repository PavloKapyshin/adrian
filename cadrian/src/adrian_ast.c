#include <stdlib.h>

#include "adrian_utils.h"
#include "adrian_ast.h"


Empty* Empty_new() {
    Empty* result = safe_malloc(sizeof(Empty));
    return result;
}

Empty* Empty_copy(Empty* self) {
    Empty* result = safe_malloc(sizeof(Empty));
    return result;
}

void Empty_free(Empty* self) {
    free(self);
}


LetDeclaration* LetDeclaration_new(char* name, Type* type, Expr* expr) {
    LetDeclaration* result = safe_malloc(sizeof(LetDeclaration));
    result->name = name;
    result->type = Type_copy(type);
    result->expr = Expr_copy(expr);
    return result;
}

void LetDeclaration_free(LetDeclaration* self) {
    Type_free(self->type);
    Expr_free(self->expr);
    free(self);
}


Literal* Literal_new(LiteralType type, char* text) {
    Literal* result = safe_malloc(sizeof(Literal));
    result->type = type;
    result->text = text;
    return result;
}

Literal* Literal_copy(Literal* self) {
    Literal* result = safe_malloc(sizeof(Literal));
    result->type = self->type;
    result->text = self->text;
    return result;
}

void Literal_free(Literal* self) {
    free(self);
}


Expr* Expr_new() {
    Expr* result = safe_malloc(sizeof(Expr));
    return result;
}


Expr* Expr_copy(Expr* self) {
    Expr* result = safe_malloc(sizeof(Expr));
    if (self->literal != NULL) {
        result->literal = Literal_copy(self->literal);
    }
    else if (self->empty != NULL) {
        result->empty = Empty_copy(self->empty);
    }
    return result;
}


void Expr_free(Expr* self) {
    if (self->literal != NULL) {
        Literal_free(self->literal);
    } else if (self->empty != NULL) {
        Empty_free(self->empty);
    }
    free(self);
}


Type* Type_new() {
    Type* result = safe_malloc(sizeof(Type));
    return result;
}


Type* Type_copy(Type* self) {
    Type* result = safe_malloc(sizeof(Type));
    if (self->empty != NULL) {
        result->empty = Empty_copy(self->empty);
    }
    return result;
}


void Type_free(Type* self) {
    if (self->empty != NULL) {
        Empty_free(self->empty);
    }
    free(self);
}
