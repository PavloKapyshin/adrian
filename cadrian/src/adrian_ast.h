#ifndef ADRIAN_AST_INCLUDED
#define ADRIAN_AST_INCLUDED


typedef struct {} Empty;


typedef union {
    char* name;
    Empty* empty;
} Type;


typedef enum {
    NumberLiteralType,
    StringLiteralType
} LiteralType;


typedef struct {
    LiteralType type;
    char* text;
} Literal;


typedef union {
    Literal* literal;
    Empty* empty;
} Expr;


typedef struct {
    char* name;
    Type* type;
    Expr* expr;
} LetDeclaration;


typedef union {
    LetDeclaration* let_declaration;
} Node;


extern Empty* Empty_new();
extern Empty* Empty_copy(Empty* self);
extern void Empty_free(Empty* self);

extern LetDeclaration* LetDeclaration_new(char* name, Type* type, Expr* expr);
extern LetDeclaration* LetDeclaration_copy(LetDeclaration* self);
extern void LetDeclaration_free(LetDeclaration* self);

extern Literal* Literal_new(LiteralType type, char* text);
extern Literal* Literal_copy(Literal* self);
extern void Literal_free(Literal* self);

extern Expr* Expr_new();
extern Expr* Expr_copy(Expr* self);
extern void Expr_free(Expr* self);

extern Type* Type_new();
extern Type* Type_copy(Type* self);
extern void Type_free(Type* self);

extern Node* Node_new();
extern Node* Node_copy(Node* self);
extern void Node_free(Node* self);

#endif
