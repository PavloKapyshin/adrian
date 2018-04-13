import enum

import collections


@enum.unique
class NodeT(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4
    adt = 5
    protocol = 6
    parameter = 7
    arg = 8


@enum.unique
class DataT(enum.Enum):
    module = 1
    struct = 2
    adt = 3


@enum.unique
class CallableT(enum.Enum):
    struct = 1
    fun = 2
    struct_func = 4


@enum.unique
class LLT(enum.Enum):
    body = 1
    call_args = 2
    args = 3
    params = 4
    elseif = 5


class LiteralT(enum.Enum):
    number = 1
    string = 2
    vector = 3


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
        return False

    def __hash__(self):
        return hash(self.data)


class Name(_Name):

    def __init__(self, data, is_user_name=True, is_mangled=False):
        super().__init__(data)
        self.is_user_name = is_user_name
        self.is_mangled = is_mangled


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


class StructDecl(Node):

    def __init__(self, name, params, protocols, body):
        self.name = name
        self.params = params
        self.protocols = protocols
        self.body = body
        self._keys = ("name", "params", "protocols", "body")


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


class Not(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr",)


class Expr(ComplexExpr):
    pass


class Assignment(ComplexExpr):
    pass


class Return(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr",)


class Literal(Node):

    def __init__(self, type_, literal):
        self.type_ = type_
        self.literal = literal
        self._keys = ("type_", "literal")


class LiteralType(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_",)


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


class GenericType(Node):

    def __init__(self, base, params):
        self.base = base
        self.params = params
        self._keys = ("base", "params")


class Ref(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr",)


class StructScalar(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_",)


class While(Node):

    def __init__(self, expr, body):
        self.expr = expr
        self.body = body
        self._keys = ("expr", "body")


class Cond(Node):

    def __init__(self, if_, elifs_, else_):
        self.if_ = if_
        self.elifs_ = elifs_
        self.else_ = else_
        self._keys = ("if_", "elifs_", "else_")


class If(Node):

    def __init__(self, expr, body):
        self.expr = expr
        self.body = body
        self._keys = ("expr", "body")


class Elif(Node):

    def __init__(self, expr, body):
        self.expr = expr
        self.body = body
        self._keys = ("expr", "body")


class Else(Node):

    def __init__(self, body):
        self.body = body
        self._keys = ("body",)

class Is(Node):

    def __init__(self, expr, type_):
        self.expr = expr
        self.type_ = type_
        self._keys = ("expr", "type_")


class Alloc(BaseNode):

    def __init__(self):
        pass

    def __str__(self):
        return "Allocation"

    __repr__ = __str__


class PyObject(Node):
    pass


class PyConstant(PyObject):

    def __init__(self, name):
        self.name = name
        self._keys = ("name",)


class PyType(PyObject):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_", )


class PyFunc(PyObject):

    def __init__(self, name):
        self.name = name
        self._keys = ("name", )


class PyCall(PyObject):

    def __init__(self, name, args):
        self.name = name
        self.args = args
        self._keys = ("name", "args")


class PyFuncCall(PyCall):
    pass


class PyTypeCall(PyCall):
    pass


class AdtMember(Node):

    def __init__(self, base, member):
        self.base = base
        self.member = member
        self._keys = ("base", "member")


class AnnotatedName(Node):

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")

    def __eq__(self, other):
        if isinstance(other, str):
            return self.name == other
        elif isinstance(other, Name):
            return other == self.name
        elif isinstance(other, AnnotatedName):
            return self.name == other.name
        return False
