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
Syntax sugar
↓
Name checking
↓
Type checking
↓
Name mangling
↓
CGen
↓
C source
```

## Name checking

Compiler checks naming conventions, name existence and that builtins are not reassigned.

## Type checking

Compiler checks that type of name and type of value are equal and are valid.

## Syntax sugar

Compiler translates AST into more simple for compiler.

## Name mangling

Compiler mangles names.

## CGen

Compiler generates CGen AST.

## C source

Compiler generates C source code.
