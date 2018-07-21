## py


Internal library. It's available only in pipa.

```adrian
-- Internal
protocol PyObject

struct List is PyObject, Addable, Iterable(PyObject)
struct Int is PyObject, Addable, Subtractable, Multipliable, Divisible
struct Str is PyObject, Addable, Iterable(Str) {
    fun split(by: Str): List
}
struct Set is PyObject, Addable, Iterable(PyObject)
struct Dict is PyObject, Addable, Iterable(PyObject) {
    fun values(): List
    fun keys(): List
}

fun toInt(source: Str): Int
fun toStr(source: PyObject): Str
fun toSet(source: PyObject and Iterable(PyObject)): Set
fun toList(source: PyObject and Iterable(PyObject)): List
fun print(*sources: PyObject): Void
fun len(source: Iterable(PyObject)): Int
```


## c

#### Types

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
```
