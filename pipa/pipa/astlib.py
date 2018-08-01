import enum
import collections


@enum.unique
class NodeT(enum.Enum):
    let = 1
    var = 2
    func = 3
    struct = 4
    protocol = 5


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


class _StructLikeDecl(Node):

    def __init__(self, name, parameters, implemented_protocols, body):
        self.name = name
        self.parameters = parameters
        self.implemented_protocols = implemented_protocols
        self.body = body
        self._keys = ("name", "parameters", "implemented_protocols", "body")


class StructDecl(_StructLikeDecl):
    pass


class ExtensionDecl(_StructLikeDecl):
    pass


class ProtocolDecl(_StructLikeDecl):
    pass


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


class FuncProtoDecl(Node):

    def __init__(self, name, args, rettype):
        self.name = name
        self.args = args
        self.rettype = rettype
        self._keys = ("name", "args", "rettype")


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


class Cond(Node):

    def __init__(self, if_stmt, elifs, else_stmt):
        self.if_stmt = if_stmt
        self.elifs = elifs
        self.else_stmt = else_stmt
        self._keys = ("if_stmt", "elifs", "else_stmt")


class ConditionalStmt_(Node):

    def __init__(self, expr, body):
        self.expr = expr
        self.body = body
        self._keys = ("expr", "body")

class While(ConditionalStmt_):
    pass

class If(ConditionalStmt_):
    pass

class Elif(ConditionalStmt_):
    pass

class Else(Node):

    def __init__(self, body):
        self.body = body
        self._keys = ("body", )


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


class Subscript(Node):

    def __init__(self, base, index):
        self.base = base
        self.index = index
        self._keys = ("base", "index")


class Is(Node):

    def __init__(self, sub_expr, super_expr):
        self.sub_expr = sub_expr
        self.super_expr = super_expr
        self._keys = ("sub_expr", "super_expr")


class StructPath(Node):

    def __init__(self, path):
        self.path = path
        self._keys = ("path", )


class Literal(Node):

    def __init__(self, type_, literal):
        self.type_ = type_
        self.literal = literal
        self._keys = ("type_", "literal")


class Ref(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


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
