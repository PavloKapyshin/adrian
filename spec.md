# Adrian Language Specification


## Variable declaration

```adrian
var n: t = v
var n: t
```

Where `n` is a variable name, `t` is a type name and `v` is an optional value.

Type of name and type of value must be equal.

```adrian
var n = v
```

Type inference is also provided.


## Variable assignment

```adrian
n = v
```

Where `n` is a variable name, `v` is a value.

Type of name and type of value must be equal. Variable must be declarated before assignment.


## Function declaration

```adrian
func n(args): t {
	stmts
}

func n(args) {
	stmts
}
```

Where `n` is a function name, `t` is an optional return type, `args` is a list of arguments,
`stmts` is a list of statements. Function return's type and type `t` must be equal.

### args

`args` can be empty:

```adrian
func n() {}
```

```adrian
func n(arg1: t1; arg2, arg3: t2) {}
```

Where `arg1` has type `t1`, `arg2` and `arg3` have type `t2`. Different groups of
arguments must be separated by semicolon. Arguments in groups must be separated by
comma. The passed function arguments must be in the order in which arguments in
the function declaration are written.

### stmts

`stmts` can be empty:

```adrian
func n() {}
```

Or `stmts` can consists of any statement except constant decalaration and include return statement.

## TODO

Write about:
[ ] If, elif and else
[ ] Naming rules
