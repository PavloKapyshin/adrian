# c module

## Types

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


## Functions

`c#malloc(size: c#Size): Object`
allocates memory and returns poiter to it.

`c#sizeof(source: Object): c#Size`
returns size (in bytes) of any c struct.

`c#free(memory: Object): Void`
frees memory.
