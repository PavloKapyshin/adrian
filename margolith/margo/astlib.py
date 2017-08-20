import copy, collections

from . import cdefs


AST = object()
class BaseNode:
    pass


class Node(BaseNode):
    _keys = ()  # Override in subclass.

    def __str__(self):
        return "{}({})".format(
            self.__class__.__name__,
            ", ".join(
                "{}={!r}".format(key, getattr(self, key))
                for key in self._keys))

    __repr__ = __str__

    def copy(self):
        return copy.deepcopy(self)


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

    def copy(self):
        return copy.deepcopy(self)

class Name(_Name):

    def __init__(self, data, is_tmp=False):
        super().__init__(data)
        self.is_tmp = is_tmp

class CType(_Name):
    pass


class ModuleMember(Node):

    def __init__(self, module, member):
        self.module = module
        self.member = member
        self._keys = ("module", "member")


class StructElem(Node):

    def __init__(self, name, elem):
        self.name = name
        self.elem = elem
        self._keys = ("name", "elem")


class ParamedType(Node):

    def __init__(self, base, params):
        self.base = base
        self.params = params
        self._keys = ("base", "params")


class _Callable(Node):

    def __init__(self, name, args):
        self.name = name
        self.args = args
        self._keys = ("name", "args")


class StructCall(_Callable):
    """       args
             vvvvvv
    MyStruct(1 + 20)
    ^^^^^^^^
      name
    """
    pass


class FuncCall(_Callable):
    """     args
           vvvvvv
    myFunc(1 + 20)
    ^^^^^^
     name
    """


class CFuncCall(_Callable):
    """       args
             vvvvvv
    c#myFunc(1 + 20)
      ^^^^^^
       name
    """


class StructFuncCall(Node):

    def __init__(self, struct, method_name, args):
        self.struct = struct
        self.method_name = method_name
        self.args = args
        self._keys = ("struct", "method_name", "args")


class MethodCall(Node):

    def __init__(self, base, method, args):
        self.base = base
        self.method = method
        self.args = args
        self._keys = ("base", "method", "args")


class Arg(Node):

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")


class Empty(BaseNode):

    def __init__(self):
        pass

    def __str__(self):
        return "EMPTY"

    def __len__(self):
        return 0

    def as_list(self):
        return []

    __repr__ = __str__


class Types(Node):

    def __init__(self, type_, rest=None):
        self.type_ = type_
        self.rest = rest or Empty()
        self._keys = ("type_", "rest")

    def as_list(self):
        def _gen():
            current_type = self
            yield current_type.type_
            while not isinstance(current_type.rest, Empty):
                current_type = current_type.rest
                yield current_type.type_
        return list(_gen())

    def append(self, type_):
        current_type = self
        while not isinstance(current_type.rest, Empty):
            current_type = current_type.rest
        current_type.rest = Types(type_, Empty())

    def extend(self, types):
        current_type = self
        while not isinstance(current_type.rest, Empty):
            current_type = current_type.rest
        current_type.rest = types

    def extend_from_list(self, types):
        current_type = self
        while not isinstance(current_type.rest, Empty):
            current_type = current_type.rest
        for type_ in types:
            current_type.rest = Types(type_, Empty())
            current_type = current_type.rest


class Names(Node):

    def __init__(self, name, rest=None):
        self.name = name
        self.rest = rest or Empty()
        self._keys = ("name", "rest")

    def as_list(self):
        def _gen():
            current_name = self
            yield current_name.name
            while not isinstance(current_name.rest, Empty):
                current_name = current_name.rest
                yield current_name.name
        return list(_gen())

    def append(self, name):
        current_name = self
        while not isinstance(current_name.rest, Empty):
            current_name = current_name.rest
        current_name.rest = Names(name, Empty())

    def extend(self, names):
        current_name = self
        while not isinstance(current_name.rest, Empty):
            current_name = current_name.rest
        current_name.rest = names

    def extend_from_list(self, names):
        current_name = self
        while not isinstance(current_name.rest, Empty):
            current_name = current_name.rest
        for name in names:
            current_name.rest = Names(name, Empty())
            current_name = current_name.rest


class Body(Node):

    def __init__(self, stmt, rest=None):
        self.stmt = stmt
        self.rest = rest or Empty()
        self._keys = ("stmt", "rest")

    def as_list(self):
        def _gen():
            current_stmt = self
            yield current_stmt.stmt
            while not isinstance(current_stmt.rest, Empty):
                current_stmt = current_stmt.rest
                yield current_stmt.stmt
        return list(_gen())

    def append(self, stmt):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt.rest = Body(stmt, Empty())

    def extend(self, body):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt.rest = body

    def extend_from_list(self, body):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        for stmt in body:
            current_stmt.rest = Body(stmt, Empty())
            current_stmt = current_stmt.rest

class Args(Node):

    def __init__(self, name, type_, rest=None):
        self.name = name
        self.type_ = type_
        self.rest = rest or Empty()
        self._keys = ("name", "type_", "rest")

    def as_list(self):
        def _gen():
            current_arg = self
            yield (current_arg.name, current_arg.type_)
            while not isinstance(current_arg.rest, Empty):
                current_arg = current_arg.rest
                yield (current_arg.name, current_arg.type_)
        return list(_gen())

    def append(self, name, type_):
        current_arg = self
        while not isinstance(current_arg.rest, Empty):
            current_arg = current_arg.rest
        current_arg.rest = Args(name, type_, Empty())

    def extend(self, args):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt.rest = args


class CallArgs(Node):

    def __init__(self, arg, rest=None):
        self.arg = arg
        self.rest = rest or Empty()
        self._keys = ("arg", "rest")

    def as_list(self):
        def _gen():
            current_arg = self
            yield current_arg.arg
            while not isinstance(current_arg.rest, Empty):
                current_arg = current_arg.rest
                yield current_arg.arg
        return list(_gen())

    def append(self, arg):
        current_arg = self
        while not isinstance(current_arg.rest, Empty):
            current_arg = current_arg.rest
        current_arg.rest = CallArgs(arg, Empty())

    def extend(self, args):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt.rest = args

    def __len__(self):
        length = 1
        current_arg = self
        while not isinstance(current_arg.rest, Empty):
            current_arg = current_arg.rest
            length += 1
        return length


class Expr(Node):

    def __init__(self, op, lexpr, rexpr):
        self.op = op
        self.lexpr = lexpr
        self.rexpr = rexpr
        self._keys = ("op", "lexpr", "rexpr")


class _VarOrLetDecl(Node):

    def __init__(self, name, type_, expr):
        self.name = name
        self.type_ = type_
        self.expr = expr
        self._keys = ("name", "type_", "expr")


class Decl(_VarOrLetDecl):
    """         type_
               vvvvvvv
    var myVar: Integer = 1 + 20
        ^^^^^            ^^^^^^
        name              expr

    """


class AssignmentAndAlloc(_VarOrLetDecl):
    pass


class Assignment(Node):
    """   op
          v
    myVar = 1 + 20
    ^^^^^   ^^^^^^
     var     expr

    """

    def __init__(self, var, op, expr):
        self.var = var
        self.op = op
        self.expr = expr
        self._keys = ("var", "op", "expr")


class _FuncOrMethodDecl(Node):

    def __init__(self, name, args, rettype, body):
        self.name = name
        self.args = args
        self.rettype = rettype
        self.body = body
        self._keys = ("name", "args", "rettype", "body")


class Func(_FuncOrMethodDecl):
    """  name                                    rettype
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """


class Method(_FuncOrMethodDecl):
    """   name                                     rettype
        vvvvvvvv                                  vvvvvvvvvv
    fun myMethod(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                             args                             body
    """


class Protocol(Node):
    """         name     parameters
             vvvvvvvvvv vvvvvvvvvvvvv
    protocol MyProtocol(SomeType, ...) {
        ... < body
    }
    """

    def __init__(self, name, parameters, body):
        self.name = name
        self.parameters = parameters
        self.body = body
        self._keys = ("name", "parameters", "body")


class Struct(Node):
    """     name    parameters             protocols
           vvvvvv vvvvvvvvvvvvvv      vvvvvvvvvvvvvvvvvvvv
    struct MyType(valueType, ...) is (Legthable, Printable) {
        length: Integer         < body
        data: valueType         < body
    }
    """

    def __init__(self, name, parameters, protocols, body):
        self.name = name
        self.parameters = parameters
        self.protocols = protocols
        self.body = body
        self._keys = ("name", "parameters", "protocols", "body")


class Field(Node):
    """struct MyType {
           length: Integer     < Field
           data: String        < Field
       }
    """

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_
        self._keys = ("name", "type_")


class Return(Node):
    """return 1 + 2
              ^^^^^
              expr
    """

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


class Literal(Node):

    def __init__(self, literal):
        self.literal = literal
        self._keys = ("literal", )


class Ref(Literal):
    """ref."""


class IntLiteral(Literal):
    """Just a number like 42"""


class CLiteral(Literal):

    @classmethod
    def to_type(cls):
        return CType(cls._type)


class CIntFast8(CLiteral):
    """CIntFast8."""
    _type = "IntFast8"


class CIntFast16(CLiteral):
    """CIntFast16."""
    _type = "IntFast16"


class CIntFast32(CLiteral):
    """CIntFast32."""
    _type = "IntFast32"


class CIntFast64(CLiteral):
    """CIntFast64."""
    _type = "IntFast64"


class CUIntFast8(CLiteral):
    """CUIntFast8."""
    _type = "UIntFast8"


class CUIntFast16(CLiteral):
    """CUIntFast16."""
    _type = "UIntFast16"


class CUIntFast32(CLiteral):
    """CUIntFast32."""
    _type = "UIntFast32"


class CUIntFast64(CLiteral):
    """CUIntFast64."""
    _type = "UIntFast64"


class CCast(Node):

    def __init__(self, expr, to):
        self.expr = expr
        self.to = to
        self._keys = ("expr", "to")


class CVoid(BaseNode):
    """CVoid."""

    def __str__(self):
        return "Void"

    __repr__ = __str__


class _TypeModifier(Node):

    def __init__(self, type_):
        self.type_ = type_
        self._keys = ("type_", )


class Deref(Node):

    def __init__(self, expr):
        self.expr = expr
        self._keys = ("expr", )


class Pointer(_TypeModifier):
    pass


class StructScalar(_TypeModifier):
    pass


CINT_TYPES = (
    CIntFast8,
    CIntFast32,
    CIntFast16,
    CIntFast64,
    CUIntFast8,
    CUIntFast16,
    CUIntFast32,
    CUIntFast64
)

CTYPES = CINT_TYPES
CTYPES_NAMES = set(str(type_.to_type()) for type_ in CTYPES)