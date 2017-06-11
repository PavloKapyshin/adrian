# C module


## C types

Numeric types:

```
c#IntFast8
c#IntFast16
c#IntFast32
c#IntFast64

c#UIntFast8
c#UIntFast16
c#UIntFast32
c#UIntFast64

c#SizeT
```

`c#Char`
`c#Pointer`
`c#Array`
`c#Void`

`c#String` is an alias for `c#Pointer(c#Char)`.
`c#Memory` is an alias for `c#Pointer(c#Void)`.


## C functions

`c#malloc(size: c#SizeT): c#Memory`
allocates memory and returns poiter to it.

`c#sizeof(source: c#Any): c#SizeT`
returns size (in bytes) of any c struct.

`c#free(memory: c#Memory): None`
frees memory.
