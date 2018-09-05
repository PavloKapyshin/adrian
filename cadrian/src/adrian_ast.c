#include <string.h>
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
    result->name = safe_malloc(sizeof(name));
    size_t name_length = strlen(name);
    memmove(result->name, name, name_length + 1);
    result->type = Type_copy(type);
    result->expr = Expr_copy(expr);
    return result;
}

LetDeclaration* LetDeclaration_copy(LetDeclaration* self) {
    LetDeclaration* result = safe_malloc(sizeof(LetDeclaration));
    result->name = safe_malloc(sizeof(self->name));
    size_t name_length = strlen(self->name);
    memmove(result->name, self->name, name_length + 1);
    result->type = Type_copy(self->type);
    result->expr = Expr_copy(self->expr);
    return result;
}

void LetDeclaration_free(LetDeclaration* self) {
    free(self->name);
    Type_free(self->type);
    Expr_free(self->expr);
    free(self);
}


Literal* Literal_new(LiteralType type, char* text) {
    Literal* result = safe_malloc(sizeof(Literal));
    result->type = type;
    result->text = safe_malloc(sizeof(text));
    size_t text_length = strlen(text);
    memmove(result->text, text, text_length + 1);
    return result;
}

Literal* Literal_copy(Literal* self) {
    Literal* result = safe_malloc(sizeof(Literal));
    result->type = self->type;
    result->text = safe_malloc(sizeof(self->text));
    size_t text_length = strlen(self->text);
    memmove(result->text, self->text, text_length + 1);
    return result;
}

void Literal_free(Literal* self) {
    free(self->text);
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


Node* Node_new() {
    Node* result = safe_malloc(sizeof(Node));
    return result;
}


Node* Node_copy(Node* self) {
    Node* result = safe_malloc(sizeof(Node));
    if (self->let_declaration != NULL) {
        result->let_declaration = LetDeclaration_copy(self->let_declaration);
    }
    return result;
}


void Node_free(Node* self) {
    if (self->let_declaration != NULL) {
        LetDeclaration_free(self->let_declaration);
    }
    free(self);
}
