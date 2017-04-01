# Compiler architecture

```
Adrian source code
↓
Lexing and Parsing
↓
Name checking
↓
Type checking
↓
Value checking
↓
Name mangling
↓
CGen AST
↓
C source
```

## Name checking

Compiler checks naming conventions, name existence and that builtins are not reassigned.

## Type checking

Compiler checks that type of name and type of value are equal and are valid.

## Value checking

Compiler generates simplified (not for human :) representation of values.

## Name mangling

Compiler mangles names.

## CGen

Compiler generates CGen AST.

## C source

Compiler generates C source code.
