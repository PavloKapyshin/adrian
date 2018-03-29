## Adrian naming

type -- struct, adt or a protocol
generic type -- generic struct, generic adt or a generic protocol
parameter --
    1.Type passed to a generic type (`var a: S(Parameter1, Parameter2, ...)`)
    2.Name which maps to a type in generic type declaration
    (`struct S(param1, param2, ...)`)
struct -- type declarated with `struct` keyword
adt -- type declarated with `adt` keyword
protocol -- type declarated with `protocol` keyword
supertype -- type which has subtypes
subtype -- type which has supertypes
field -- variable or constant inside of struct
special method -- __method__
method -- function inside of struct, adt or a protocol


### Reserved
property -- computed field
attribute -- metadata
