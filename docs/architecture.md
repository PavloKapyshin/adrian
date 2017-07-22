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
Type inference
↓
Default value
↓
Type checking
↓
OOPDef
↓
OOPCall
↓
Interface checking
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


## Type inference

Compiler inferences types where needed.


## Type checking

Compiler checks types for equality.


## Default value

Adds default value where needed.


## OOPDef

Translates methods to funcs.


## OOPCall

Translates method calls to func calls.


## Interface checking

Checks that struct implements interfaces.


## Name mangling

Compiler mangles names using file hash.

## CGen

Compiler generates CGen AST.


## C source

Compiler generates C source code using `adrian.cgen` library.
