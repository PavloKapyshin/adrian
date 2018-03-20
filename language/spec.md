# Reserved names
These names are reserved:
```
var
let
fun
adt
ref
protocol
struct
return
```



# Naming rules
## Variables, Constants and Functions
CamelCase starting with lowercase letter.
Regular expression for matching: `[a-z][a-zA-Z0-9]*`.

## Methods
CamelCase starting with lowercase letter.
Special method's name is surrounded by two underscores (\_\_)
Regular expression for matching: `((__)[a-z][a-zA-Z0-9]*(__))|([a-z][a-zA-Z0-9]*)`.

## Modules
Underscore based naming.
Regular expression for matching: `[a-z][a-z_0-9]*`.

## Structs, ADTs and Protocols
CamelCase starting with uppercase letter.
Regular expression for matching: `[A-Z][a-zA-Z0-9]*`



# Statements
## Constant declaration
```adrian
let n: T = v
let n = v
```
`n` is a variable name, `T` is an optional type name and `v` is a value.

## Variable declaration
```adrian
var n: T = v
var n = v
var n: T
```
`n` is a variable name, `T` is an optional type name and `v` is an optional value.

## Variable assignment
```adrian
n = v
```
`n` is a variable name, `v` is a value.
Type of name and type of value must be equal.
Variable must be declarated before assignment.

## Function declaration
```adrian
fun n(args): T {
	stmts
}

fun n(args) {
	stmts
}
```

`n` is a function name, `T` is an optional return type, `args` is a list of arguments,
`stmts` is a list of statements. Function return's type and type `T` must be equal.

### args
`args` can be empty:
```adrian
fun n() {}
```
or not empty:
```adrian
fun n(arg1: T1, arg2: T2, ...) {}
```
`arg1` has type `T1`, `arg2` has type `T2`. Arguments must be separated by
comma. The passed function arguments must be in the order in which arguments in
the function declaration are written.

### stmts
`stmts` can be empty:
```adrian
fun n() {}
```
Or `stmts` can consist of any statement exclude struct, adt, protocol and function declaration and include return statement.

## Struct declaration
```adrian
struct N {
    stmts
}

struct N(ts) {
    stmts
}
```

### stmts
`stmts` can be empty or `stmts` can consist of method declaration and field declaration.

### ts
```adrian
t1, t2, ...
```
`t1`, `t2` are variables that point to types and can be used in stmts as types.

### Field declaration
```adrian
n: T
```
`n` is a name, `T` is a type.

### Method declaration
Same as function, but `stms` may include use of `self`.
