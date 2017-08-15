# Naming rules

Names must be named according to this rules.

Private names must start with an underscore `_`.

## Variables

Regular expression for matching variable names: `[a-z_][a-zA-Z0-9]*`.
Variable name cannot be a reserved name.

Variables are named using camel case.

```adrian
-- Good names:
myVeryLongName
_myPrivateName
short
_var1

-- Bad names:
my_bad_name
_my_bad_private_name
var
```

## Functions

Regular expression for matching function names: `[a-z_][a-zA-Z0-9]*`.
Function name cannot be a reserved name.

Functions are named using camel case.

```adrian
-- Good names:
_privateFunc()
publicFunc()
_func12()

-- Bad names:
_bad_func()
bad_Func()
```

## Modules

Regular expression for matching module names: `[a-z_][a-z_0-9]*`.

Module names are named using underscore case.

```adrian
-- Good names:
_special_module#member
c#member
collections#member

-- Bad names:
_specialModule#member
C#member

-- Very bad names:
co_L_L_ections#member
```

## Types

Regular expression for matching type names: `[A-Z_][a-zA-Z0-9]`

Type names are named using camel case.

```adrian
-- Good names:
_Integer
HtmlTag
Context

-- Bad names:
integer
HTMLTag
__Integer
Int_32
```

# Variable declaration

```adrian
var n: T = v
var n: T
```

Where `n` is a variable name, `T` is a type name and `v` is an optional value.

Type of name and type of value must be equal. When `v` is not provided variable will be initialized using default value.

```adrian
var n = v
```

Type inference is also provided.


# Variable assignment

```adrian
n = v
```

Where `n` is a variable name, `v` is a value.

Type of name and type of value must be equal. Variable must be declarated before assignment.


# Function declaration

```adrian
fun n(args): T {
	stmts
}

fun n(args) {
	stmts
}
```

Where `n` is a function name, `T` is an optional return type, `args` is a list of arguments,
`stmts` is a list of statements. Function return's type and type `T` must be equal.

## args

`args` can be empty:

```adrian
fun n() {}
```

```adrian
fun n(arg1: T1, arg2: T2, ...) {}
```

Where `arg1` has type `T1`, `arg2` have type `T2`. Arguments must be separated by
comma. The passed function arguments must be in the order in which arguments in
the function declaration are written.

## stmts

`stmts` can be empty:

```adrian
fun n() {}
```

Or `stmts` can consists of any statement exclude constant decalaration and include return statement.

# Struct declaration

```adrian
struct MyStruct {
    stmts
}
```
