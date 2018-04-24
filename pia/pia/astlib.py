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

    def __init__(self, data):
        super().__init__(data)


class StructValue(Node):

    def __init__(self, type_, value):
        self.type_ = type_
        self.value = value
        self._keys = ("type_", "value")


class Decl(Node):

    def __init__(self, name, type_, expr):
        self.name = name
        self.type_ = type_
        self.expr = expr
        self._keys = ("name", "type_", "expr")

class VarDecl(Decl):
    pass


class LetDecl(Decl):
    pass


class FieldDecl(Node):

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")


class FuncPrototype(Node):

    def __init__(self, name, args, rettype):
        self.name = name
        self.args = args
        self.rettype = rettype
        self._keys = ("name", "args", "rettype")


class CallableDecl(Node):

    def __init__(self, name, args, rettype, body):
        self.name = name
        self.args = args
        self.rettype = rettype
        self.body = body
        self._keys = ("name", "args", "rettype", "body")


class FuncDecl(CallableDecl):
    pass


class StructFuncDecl(CallableDecl):
    pass


class DataDecl(Node):

    def __init__(self, name, parameters, protocols, body):
        self.name = name
        self.parameters = parameters
        self.protocols = protocols
        self.body = body
        self._keys = ("name", "parameters", "protocols", "body")


class StructDecl(DataDecl):
    pass


class AdtDecl(DataDecl):
    pass


class ProtocolDecl(DataDecl):
    pass


class ModuleMember(Node):

    def __init__(self, module, member):
        self.module = module
        self.member = member
        self._keys = ("module", "member")


class StructField(Node):

    def __init__(self, struct, field):
        self.struct = struct
        self.field = field
        self._keys = ("struct", "field")


class Call(Node):
    pass


class StructFuncCall(Call):

    def __init__(self, parent, name, args):
        self.parent = parent
        self.name = name
        self.args = args
        self._keys = ("parent", "name", "args")


class FuncCall(Call):

    def __init__(self, name, args):
        self.name = name
        self.args = args
        self._keys = ("name", "args")


class MethodCall(Call):

    def __init__(self, base, name, args):
        self.base = base
        self.name = name
        self.args = args
        self._keys = ("base", "name", "args")


class Empty(BaseNode):

    def __str__(self):
        return "EMPTY"

    def __bool__(self):
        return False

    __repr__ = __str__


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


class Alloc(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_", )


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


class AnnotatedStructField(Node):

    def __init__(self, struct, field, type_):
        self.struct = struct
        self.field = field
        self.type_ = type_
        self._keys = ("struct", "field", "type_")
