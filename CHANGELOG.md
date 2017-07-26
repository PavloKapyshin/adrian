# Changelog

## 0.2 (dev)
* fixed aliasing of structs instead of copying in __copy__ method
* fixed memory leak when struct instances were used as values for struct fields
* added namespacing at C level, so now (for example) Adrian-level malloc function
  will not clash with C-level malloc function
* added `c#IntFast64` and `c#UIntFast64` types

## 0.1
* added `c#sizeof`, `c#malloc`, `c#free` functions
* added `c#IntFast8`, `c#IntFast32`, `c#UIntFast8`, `c#UIntFast32` types
