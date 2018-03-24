import collections
import enum

@enum.unique
class NodeT(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4
    adt = 5
    protocol = 6
    commont = 7
    arg = 8


@enum.unique
class DataT(enum.Enum):
    module = 1
    struct = 2
    adt = 3
    union = 4


@enum.unique
class CallableT(enum.Enum):
    struct = 1
    fun = 2
    cfunc = 3
    struct_func = 4


@enum.unique
class LLT(enum.Enum):
    body = 1
    call_args = 2
    args = 3
    params = 4


class LiteralT(enum.Enum):
    integer = 1


@enum.unique
class DeclT(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4
    struct_func = 5
    method = 6
    field = 7
    protocol = 8
    protocol_func = 9
    adt = 10


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
        return hash(str(self))

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

    def __init__(self, data, is_user_name=True):
        super().__init__(data)
        self.is_user_name = is_user_name


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

    def __init__(self, decltype, name, params, body):
        self.decltype = decltype
        self.name = name
        self.params = params
        self.body = body
        self._keys = ("decltype", "name", "params", "body")


class DataMember(Node):

    def __init__(self, datatype, parent, member):
        self.datatype = datatype
        self.parent = parent
        self.member = member
        self._keys = ("datatype", "parent", "member")


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

    def __bool__(self):
        return False

    __repr__ = __str__


class LinkedList:

    def __init__(self, value, rest=None):
        self.value = value
        self.rest = rest or None

    def __iter__(self):
        current = self
        rest = current.rest
        if current.value is not None:
            if rest is None:
                yield [current.value]
            else:
                yield [current.value] + rest
        else:
            yield from []


class LinkedListNode(LinkedList, Node):

    def __init__(self, lltype, value, rest=None):
        super().__init__(value, rest)
        self.lltype = lltype
        self._keys = ("lltype", "value", "rest")

class Args(LinkedListNode):

    def __init__(self, name, type_, rest=None):
        super().__init__(LLT.args, (name, type_), rest)


class ComplexExpr(Node):

    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
        self._keys = ("left", "op", "right")


class Expr(ComplexExpr):
    pass

class Assignment(ComplexExpr):
    pass


class Return(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


class Literal(Node):

    def __init__(self, type_, literal):
        self.type_ = type_
        self.literal = literal
        self._keys = ("type_", "literal")


class LiteralType(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_",)


class Cast(Node):

    def __init__(self, expr, to):
        self.expr = expr
        self.to = to
        self._keys = ("expr", "to")


class Void(BaseNode):

    def __str__(self):
        return "Void"

    def __hash__(self):
        return hash(self.__str__())

    __repr__ = __str__


class Object(BaseNode):

    def __str__(self):
        return "Object"

    def __hash__(self):
        return hash(self.__str__())

    __repr__ = __str__


class ParamedType(Node):

    def __init__(self, type_, params):
        self.type_ = type_
        self.base = type_
        self.params = params
        self._keys = ("base", "params")


class Ref(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )

class StructScalar(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_", )
