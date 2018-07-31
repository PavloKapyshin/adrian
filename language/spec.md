## Reserved names
These names are reserved:
```
var
let
fun
adt
ref
while
if
in
is
elif
else
protocol
struct
extension
return
```



## Naming rules
#### Variables, Constants and Functions
CamelCase starting with lowercase letter.
Regular expression for matching: `[a-z][a-zA-Z0-9]*`.

#### Methods
CamelCase starting with lowercase letter.
Special method's name is surrounded by two underscores (\_\_).
Regular expression for matching: `((__)[a-z][a-zA-Z0-9]*(__))|([a-z][a-zA-Z0-9]*)`.

#### Modules
Underscore based naming.
Regular expression for matching: `[a-z][a-z_0-9]*`.

#### Structs, ADTs and Protocols
CamelCase starting with uppercase letter.
Regular expression for matching: `[A-Z][a-zA-Z0-9]*`.



## Statements
#### Constant declaration
```adrian
let n: T = v
let n = v
```
`n` is a variable name, `T` is an optional type name and `v` is a value.

#### Variable declaration
```adrian
var n: T = v
var n = v
var n: T
```
`n` is a variable name, `T` is an optional type name and `v` is an optional value.

#### Variable assignment
```adrian
n = v
```
`n` is a variable name, `v` is a value.
Type of name and type of value must be equal.
Variable must be declarated before assignment.

#### Function declaration
```adrian
fun n(arguments): T {
    statements
}

fun n(arguments) {
    statements
}
```

`n` is a function name, `T` is an optional return type, `arguments` is a list of arguments,
`statements` is a list of statements. Function return's type and type `T` must be equal.

##### arguments
`arguments` can be empty:
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

##### statements
`statements` can be empty:
```adrian
fun n() {}
```
Or `statements` can consist of any statement exclude struct, adt, protocol and function declaration and include return statement.

#### Struct declaration
```adrian
struct N {
    statements
}

struct N(ts) {
    statements
}
```

##### statements
`statements` can be empty or `statements` can consist of method declaration and field declaration.

##### ts
```adrian
t1, t2, ...
```
`t1`, `t2` are variables that point to types and can be used in statements as types.

##### Field declaration
```adrian
n: T
```
`n` is a name, `T` is a type.

##### Method declaration
Same as function, but `statements` may include usage of `self`.


#### Extension declaration
```adrian
extension NAME {
    methods
}

extension NAME(parameters) {
    methods
}
```

Methods may include usage of `self`.


#### Adt declaration
```adrian
adt N {
    T1, T2, ...
}
```
`N` is a name for adt, `T1` and `T2` are types.
