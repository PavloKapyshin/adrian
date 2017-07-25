# c module

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

c#Size
```

Other types:
```
c#Char
c#Pointer(T)
c#Array(T)
c#Void
```

`c#String` is an alias for `c#Pointer(c#Char)`.
`c#Memory` is an alias for `c#Pointer(c#Void)`.


## C functions

`c#malloc(size: c#Size): c#Memory`
allocates memory and returns poiter to it.

`c#sizeof(source: c#Any): c#Size`
returns size (in bytes) of any c struct.

`c#free(memory: c#Memory): c#Void`
frees memory.



# std_types module

All types:

```
Bool
None

Integer

UnicodeString
ByteString
String

Vector
Set
Dict
```

`String` is equivalent to `linked_list#LinkedList(c#Char)`



# linked_list module

## Types:

```
LinkedList
```
