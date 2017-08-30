# Changelog

## 0.1.1
* added constant declaration with `let` keyword
* added ability to nest method calls, function calls and field getting
* fix memory leak: when assigning new value to non-ctype variable, previous value was not freed
* fix bug with REPL: when input was an empty line REPL used previous input again
* added namespacing at C level, so now (for example) Adrian-level malloc function
  will not clash with C-level malloc function
* added `c#IntFast64` and `c#UIntFast64` types


## 0.1
* added `c#sizeof`, `c#malloc`, `c#free` functions
* added `c#IntFast8`, `c#IntFast32`, `c#UIntFast8`, `c#UIntFast32` types
