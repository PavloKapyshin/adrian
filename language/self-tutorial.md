## Reserved names
These names are reserved:
```
var, let, for, in, fun, return, struct, if, elif, else, while,
extension, protocol, is, and, or, not, break
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
#### Let declaration
```adrian
let name: Type = expr
let name = expr
```


#### Variable declaration
```adrian
var name: Type = expr
var name = expr
```


#### Variable assignment
```adrian
nameOrFieldReference = expr
```


#### Function declaration
```adrian
fun name(arg1: ArgType1, ...): ReturnType {
    any_statements_with_return_statement
}
```


#### Return
```adrian
return expr
```


#### Struct declaration
```adrian
struct Name {
    field_or_method_decls
}

struct Name(parameter1, ...) {
    field_or_method_decls
}

struct Name is Protocol1, ... {
    field_or_method_decls
}

struct Name(parameter1, ...) is Protocol1, ... {
    field_or_method_decls
}
```


#### Field declaration
```adrian
name: Type
```


#### Method declaration
Same as function, but may include usage of `self`.


#### Extension declaration
```adrian
extension Name {
    methods
}

extension Name(parameter1, ...) {
    methods
}

extension Name is Protocol1, ... {
    methods
}

extension Name(parameter1, ...) is Protocol1, ... {
    methods
}
```


#### Protocol declaration
```adrian
protocol Name {
    func_prototypes
}

protocol Name(parameter1, ...) {
    func_prototypes
}

protocol Name is Protocol1, ... {
    func_prototypes
}

protocol Name(parameter1, ...) is Protocol1, ... {
    func_prototypes
}
```


#### Func prototype
```adrian
fun name(arg1: ArgType1, ...): ReturnType
```


#### For
```adrian
for name1, ... in expr {
    body
}
```


#### While
```adrian
while expr {
    body
}
```


#### If, elif, else
```adrian
if expr {
    body
} elif expr {
    body
} else {
    body
}
```
`elif`s and `else` are optional.


#### Func call
```adrian
funcReference(expr1, ...)
```

`funcReference` = name | name in module | name in struct
