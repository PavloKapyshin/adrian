# Compiler architecture

```
Parsing (Adrian source code)
↓
Foreign parser interface
↓
Analyzer
↓
OOPDef
↓
OOPCall
↓
SimpEx
↓
ARC
↓
CGen
↓
Main Function
↓
adrian.cgen (C source)
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


## OOPDef

Translates methods to funcs.


## OOPCall

Translates method calls to func calls.


## SimpEx

Assigns some subexpressions to temp variables. That helps
to deinit this subexpressions if needed.


## ARC

Deinits variables.


## CGen

Compiler generates CGen AST.


## Main Function

Collects some global statements to main function.


## adrian.cgen

Compiler generates C source code using `adrian.cgen` library.
