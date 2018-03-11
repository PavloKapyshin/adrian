import collections
import enum


class ContainerT(enum.Enum):
    module = 1
    struct = 2
    adt = 3


class CallableT(enum.Enum):
    struct = 1
    fun = 2
    cfunc = 3
    struct_func = 4


class LLT(enum.Enum):
    body = 1
    call_args = 2
    args = 3


class LiteralT(enum.Enum):
    int_fast8_t = 1
    int_fast16_t = 2
    int_fast32_t = 3
    int_fast64_t = 4
    uint_fast8_t = 5
    uint_fast16_t = 6
    uint_fast32_t = 7
    uint_fast64_t = 8
    integer = 9


class DeclT(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4
    struct_func = 5
    method = 6
    protocol = 7
    adt = 9
    field = 10


AST = object()
class BaseNode:
    pass

class Node(BaseNode):
    _keys = ()  # Override in subclass.

    def __str__(self):
        fields = ", ".join(
            "{}={!r}".format(
                key, getattr(self, key))
            for key in self._keys)
        return "{}({})".format(
            self.__class__.__name__, fields)

    def __hash__(self):
        return hash(
            [self.__class__.__name] +
            [getattr(self, key) for key in self._keys])

    __repr__ = __str__


class _Name(collections.UserString):
    """Name concept.

    Is used to represent any name:
     - variable
     - constant
     - function
     - type
    """

    def __init__(self, data):
        super().__init__(data)

    def __eq__(self, other):
        if isinstance(other, str):
            return self.data == other
        elif isinstance(other, _Name):
            return self.data == other.data
        raise TypeError(
            "comparison cannot be applied to {} and {}.".format(
                type(self), type(other)))

    def __hash__(self):
        return hash(self.data)

class Name(_Name):

    def __init__(self, data):
        super().__init__(data)


class Decl(Node):

    def __init__(self, decltype, name, type_, expr):
        self.decltype = decltype
        self.name = name
        self.type_ = type_
        self.expr = expr
        self._keys = ("decltype", "name", "type_", "expr")

class CallableDecl(Node):

    def __init__(
            self, decltype, parent, name, args, rettype, body):
        self.decltype = decltype
        self.parent = parent
        self.name = name
        self.args = args
        self.rettype = rettype
        self.body = body
        self._keys = (
            "decltype", "parent", "name",
            "args", "rettype", "body")

class DataDecl(Node):

    def __init__(self, decltype, name, body):
        self.decltype = decltype
        self.name = name
        self.body = body
        self._keys = ("decltype", "name", "body")


class DataMember(Node):

    def __init__(self, containertype, parent, member):
        self.containertype = containertype
        self.parent = parent
        self.member = member
        self._keys = ("containertype", "parent", "member")


class Callable(Node):

    def __init__(self, callabletype, parent, name, args):
        self.callabletype = callabletype
        self.parent = parent
        self.name = name
        self.args = args
        self._keys = ("callabletype", "parent", "name", "args")


class Arg(Node):

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")


class Empty(BaseNode):

    def __str__(self):
        return "EMPTY"

    def as_list(self):
        return []

    __repr__ = __str__


class LinkedList:

    def __init__(self, value, rest=None):
        self.value = value
        self.rest = rest or Empty()

    def as_list(self):
        def _gen():
            current = self
            yield current.value
            while not isinstance(current.rest, Empty):
                current = current.rest
                yield current.value

        return list(_gen())


class LinkedListNode(LinkedList, Node):

    def __init__(self, lltype, value, rest=None):
        super().__init__(value, rest)
        self.lltype = lltype
        self._keys = ("lltype", "value", "rest")

class Args(LinkedListNode):

    def __init__(self, name, type_, rest=None):
        super().__init__(LLT.args, (name, type_), rest)


class Expr(Node):

    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
        self._keys = ("left", "op", "right")

class Assignment(Node):
    """        op
               v
    myVariable = 1 + 20
    ^^^^^^^^^^   ^^^^^^
       left       right
    """

    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
        self._keys = ("left", "op", "right")


class Return(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


class Literal(Node):

    def __init__(self, literaltype, literal):
        self.literaltype = literaltype
        self.literal = literal
        self._keys = ("literaltype", "literal")


class Cast(Node):

    def __init__(self, to, expr):
        self.to = to
        self.expr = expr
        self._keys = ("to", "expr")


class CompilerT(BaseNode):

    def __str__(self):
        return cls.string

    def __hash__(self):
        return hash(self.__str__())

    __repr__ = __str__


class Void(CompilerT):
    string = "Void"

class Object(CompilerT):
    string = "Object"

class Ref(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )

class StructScalar(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_", )

CINT_TYPES = (
    LiteralT.int_fast8_t,
    LiteralT.int_fast16_t,
    LiteralT.int_fast32_t,
    LiteralT.int_fast64_t,
    LiteralT.uint_fast8_t,
    LiteralT.uint_fast16_t,
    LiteralT.uint_fast32_t,
    LiteralT.uint_fast64_t
)
CTYPES = CINT_TYPES