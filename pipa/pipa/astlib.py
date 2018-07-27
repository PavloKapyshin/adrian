import enum
import collections


@enum.unique
class NodeT(enum.Enum):
    let = 1
    var = 2
    func = 3
    struct = 4


@enum.unique
class LiteralT(enum.Enum):
    number = 1
    string = 2
    vector = 3
    dict_ = 4
    set_ = 5


AST = object()


class BaseNode:
    pass


class Node(BaseNode):
    _keys = ()  # Override in subclass.

    def __str__(self):
        fields = ", ".join(
            "{}={!r}".format(
                key, getattr(self, key)) for key in self._keys)
        return "{}({})".format(
            self.__class__.__name__, fields)

    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        if isinstance(self, type(other)):
            for member in self._keys:
                if getattr(self, member) != getattr(other, member):
                    return False
            return True
        return False

    __repr__ = __str__


class Decl(Node):

    def __init__(self, name, type_, expr):
        self.name = name
        self.type_ = type_
        self.expr = expr
        self._keys = ("name", "type_", "expr")

class LetDecl(Decl):
    pass

class VarDecl(Decl):
    pass


class FieldDecl(Node):

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")


class StructDecl(Node):

    def __init__(self, name, body):
        self.name = name
        self.body = body
        self._keys = ("name", "body")


class _FuncDecl(Node):

    def __init__(self, name, args, rettype, body):
        self.name = name
        self.args = args
        self.rettype = rettype
        self.body = body
        self._keys = ("name", "args", "rettype", "body")

class MethodDecl(_FuncDecl):
    pass

class FuncDecl(_FuncDecl):
    pass


class Assignment(Node):

    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
        self._keys = ("left", "op", "right")


class Return(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


class For(Node):

    def __init__(self, names, container, body):
        self.names = names
        self.container = container
        self.body = body
        self._keys = ("names", "container", "body")


class FuncCall(Node):

    def __init__(self, name, args):
        self.name = name
        self.args = args
        self._keys = ("name", "args")


class ModuleMember(Node):

    def __init__(self, module, member):
        self.module = module
        self.member = member
        self._keys = ("module", "member")


class Expr(Node):

    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
        self._keys = ("left", "op", "right")


class StructPath(Node):

    def __init__(self, path):
        self.path = path
        self._keys = ("path", )


class Literal(Node):

    def __init__(self, type_, literal):
        self.type_ = type_
        self.literal = literal
        self._keys = ("type_", "literal")


class InstanceValue(Node):

    def __init__(self, type_, value):
        self.type_ = type_
        self.value = value
        self._keys = ("type_", "value",)


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

    def __init__(self, data):
        super().__init__(data)


class PyObject(Node):
    pass


class PyType(PyObject):

    def __init__(self, name):
        self.name = name
        self._keys = ("name", )


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


class Empty(BaseNode):

    def __str__(self):
        return "EMPTY"

    def __bool__(self):
        return False

    __repr__ = __str__
