import collections


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


class StructMember(Node):

    def __init__(self, struct, member):
        self.struct = struct
        self.member = member
        self._keys = ("struct", "member")


# TODO: replace with StructMember.
class StructElem(Node):

    def __init__(self, name, elem):
        self.name = name
        self.elem = elem
        self._keys = ("name", "elem")


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


# TODO: replace with StructFuncCall in second half of the compiler.
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

    def __str__(self):
        return "EMPTY"

    def __len__(self):
        return 0

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

    def __init__(self, value, rest=None):
        super().__init__(value, rest)
        self._keys = ("value", "rest")


class Types(LinkedListNode):
    pass


class Names(LinkedListNode):
    pass


class Body(LinkedListNode):
    pass


class Args(LinkedListNode):

    def __init__(self, name, type_, rest=None):
        super().__init__((name, type_), rest)
        self._keys = ("value", "rest")


class CallArgs(LinkedListNode):
    pass


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


class LetDecl(_VarOrLetDecl):
    """         type_
               vvvvvvv
    let myVar: Integer = 1 + 20
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