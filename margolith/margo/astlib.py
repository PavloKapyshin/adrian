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

    def __init__(self, struct, func_name, args):
        self.struct = struct
        self.func_name = func_name
        self.args = args
        self._keys = ("struct", "func_name", "args")


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


class Body(LinkedListNode):
    pass


class CallArgs(LinkedListNode):
    pass


class Args(LinkedListNode):

    def __init__(self, name, type_, rest=None):
        super().__init__((name, type_), rest)
        self._keys = ("value", "rest")


class Expr(Node):

    def __init__(self, op, left_expr, right_expr):
        self.op = op
        self.left_expr = left_expr
        self.right_expr = right_expr
        self._keys = ("op", "left_expr", "right_expr")


class _VarOrLetDecl(Node):

    def __init__(self, name, type_, expr):
        self.name = name
        self.type_ = type_
        self.expr = expr
        self._keys = ("name", "type_", "expr")


class VarDecl(_VarOrLetDecl):
    """         type_
               vvvvvvv
    var myVar: Integer = 1 + 20
        ^^^^^            ^^^^^^
        name              expr
    """


class LetDecl(_VarOrLetDecl):
    """              type_
                    vvvvvvv
    let myConstant: Integer = 1 + 20
        ^^^^^^^^^^            ^^^^^^
           name                expr
    """


# TODO: replace with something else.
class AssignmentAndAlloc(_VarOrLetDecl):
    pass


class Assignment(Node):
    """        op
               v
    myVariable = 1 + 20
    ^^^^^^^^^^   ^^^^^^
     variable     expr
    """

    def __init__(self, variable, op, expr):
        self.variable = variable
        self.op = op
        self.expr = expr
        self._keys = ("variable", "op", "expr")


class _FuncOrMethodDecl(Node):

    def __init__(self, name, args, rettype, body):
        self.name = name
        self.args = args
        self.rettype = rettype
        self.body = body
        self._keys = ("name", "args", "rettype", "body")


class FuncDecl(_FuncOrMethodDecl):
    """  name                                    rettype
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """


class MethodDecl(_FuncOrMethodDecl):
    """   name                                     rettype
        vvvvvvvv                                  vvvvvvvvvv
    fun myMethod(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                             args                             body
    """


class ProtocolDecl(Node):
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


class StructDecl(Node):
    """     name
           vvvvvv
    struct MyType {
        length: Integer         < body
        data: valueType         < body
    }
    """

    def __init__(self, name, body):
        self.name = name
        self.body = body
        self._keys = ("name", "body")


class FieldDecl(Node):
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
    _type = "IntFast8"


class CIntFast16(CLiteral):
    _type = "IntFast16"


class CIntFast32(CLiteral):
    _type = "IntFast32"


class CIntFast64(CLiteral):
    _type = "IntFast64"


class CUIntFast8(CLiteral):
    _type = "UIntFast8"


class CUIntFast16(CLiteral):
    _type = "UIntFast16"


class CUIntFast32(CLiteral):
    _type = "UIntFast32"


class CUIntFast64(CLiteral):
    _type = "UIntFast64"


class CCast(Node):

    def __init__(self, expr, to):
        self.expr = expr
        self.to = to
        self._keys = ("expr", "to")


class CVoid(BaseNode):

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