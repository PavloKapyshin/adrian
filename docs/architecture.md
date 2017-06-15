# Compiler architecture

```
Adrian source code
↓
Parsing
↓
Foreign parser interface
↓
Analyzer
↓
Naming rules checking
↓
Name existence checking
↓
Type checking
↓
Name mangling
↓
CGen
↓
C source
```


## Parsing

Parsing the input Adrian source code and returning parser AST
(probably JSON with the AST inside). Parser for margolith can be written in
any programming language with JSON support and can be plugged in using
foreign parser interface.


## Foreign parser interface

Translating parser AST to object-oriented AST.


## Analyzer

Compiler analyzes names and translates them into more specific.


## Naming rules checking

Compiler checks naming conventions.


## Name existence checking

Compiler checks name existence.


## Type checking

Compiler checks types for equality.


## Name mangling

Compiler mangles names using file hash.

## CGen

Compiler generates CGen AST.

## C source

Compiler generates C source code using adrian.cgen library.
