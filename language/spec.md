# Naming rules

Names must be named according to this rules.

Private name must starts with an underscore `_`.

## Variables

Regular expression for matching variable names: `[a-z_][a-zA-Z0-9]*`.
Variable name cannot be a reserved name.

Variables are named using camel case.

```adrian
-- Good names:
var myVeryLongName: None
var _myPrivateName: None
var short: None
var _var1: None

-- Bad names:
var my_bad_name: None
var _my_bad_private_name: None
var var: None
```

## Functions

Regular expression for matching function names: `[a-z_][a-zA-Z0-9]*`.
Function name cannot be a reserved name.

Functions are named using camel case.

```adrian
-- Good names:
fun _privateFunc(): None {}
fun publicFunc(): None {}
fun _func12(): None {}

-- Bad names:
func _bad_func(): None {}
func bad_Func(): None {}
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
var myVar: _Integer
var myVar: HtmlTag
var myVar: Context

-- Bad names:
var myVar: integer
var myVar: HTMLTag
var myVar: __Integer
var myVar: Int_32
```

# Default values

When value is not provided, the types are used for generating default value.

Default values for standard types:

```
| Type Name | Default value |
|-----------+---------------|
| Integer   | 0             |
| String    | ""            |
| Decimal   | 0             |
| Bool      | False         |
| Vector    | []            |
| Set       | ()            |
| Dict      | {}            |
| None      |               |
```

## Examples

```adrian
-- These variable declarations are equivalent:
var myVar: Integer = 0
var myVar: Integer
```

```adrian
-- These functions are equivalent:
fun myFunc(): Integer {}

fun myFunc(): Integer {
	return 0
}
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
fun n(arg1: T1; arg2, arg3: T2; ...) {}
```

Where `arg1` has type `T1`, `arg2` and `arg3` have type `T2`. Different groups of
arguments must be separated by semicolon. Arguments in groups must be separated by
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
sct MyStruct(types) is (interfaces) {
    stmts
}
```

## types

`types` can be empty:

```adrian
sct MyStruct {}
```

```adrian
sct Node(someType1, someType2, ...) {}
```

Every type in `types` must be named as variable.
Because it isn't a type name, it is an argument like in function.
You can use these types inside of struct body.


## interfaces

TODO
