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

Other types:
```
c#Char
c#Pointer
c#Array
c#Void
```

`c#String` is an alias for `c#Pointer(c#Char)`.
`c#Memory` is an alias for `c#Pointer(c#Void)`.


## C functions

`c#malloc(size: c#SizeT): c#Memory`
allocates memory and returns poiter to it.

`c#sizeof(source: c#Any): c#SizeT`
returns size (in bytes) of any c struct.

`c#free(memory: c#Memory): None`
frees memory.



# STD types module

All types:

```
Bool
None

Integer

UnicodeString
ByteString
String

LinkedList
Vector
Set
Dict

Stack
```


`String` is equivalent to `LinkedList(c#Char)`


## LinkedList

```
inf LinkedList

sct Node(type) is (LinkedList, Lengthable, Printable) {

    fun __init__(value: type; next: Maybe(LinkedList(type))): None
    fun empty(): Bool
    fun head(): type
    fun tail(): Maybe(LinkedList(type))
    fun cons(source: LinkedList(type)): None
    fun append(source: LinkedList(type)): None
}
```