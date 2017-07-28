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


class Name(collections.UserString):
    """Name concept in Adrian.

    Is used to represent:
     - variable
     - constant
     - function
     - type

    """

    def __init__(self, data):
        super().__init__(data)

    def copy(self):
        return copy.deepcopy(self)


class VariableName(Name):
    pass


class TypeName(Name):
    pass


class ModuleName(Name):
    pass


class FunctionName(Name):
    pass


class MethodName(Name):
    pass


class CType(Name):
    pass


class ModuleMember(Node):

    def __init__(self, module_name, member):
        self._module_name = module_name
        self._member = member
        self._keys = ("module_name", "member")

    @property
    def module_name(self):
        return self._module_name

    @property
    def member(self):
        return self._member


class StructElem(Node):

    def __init__(self, name, elem):
        self._name = name
        self._elem = elem
        self._keys = ("name", "elem")

    @property
    def name(self):
        return self._name

    @property
    def elem(self):
        return self._elem


class FuncCall(Node):
    """Function call.

            args
           vvvvvv
    myFunc(1 + 20)
    ^^^^^^
     name
    """

    def __init__(self, name, args):
        self._name = name
        self._args = args
        self._keys = ("name", "args")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args


class CFuncCall(Node):
    """C function call.

              args
             vvvvvv
    c#myFunc(1 + 20)
      ^^^^^^
       name
    """

    def __init__(self, name, args):
        self._name = name
        self._args = args
        self._keys = ("name", "args")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args


class MethodCall(Node):

    def __init__(self, struct, method, args):
        self._struct = struct
        self._method = method
        self._args = args
        self._keys = ("struct", "method", "args")

    @property
    def struct(self):
        return self._struct

    @property
    def method(self):
        return self._method

    @property
    def args(self):
        return self._args


class Empty(BaseNode):

    def __init__(self):
        pass

    def __str__(self):
        return "EMPTY"

    def as_list(self):
        return []

    __repr__ = __str__


class Body(Node):

    def __init__(self, stmt, rest=None):
        self._stmt = stmt
        self._rest = rest or Empty()
        self._keys = ("stmt", "rest")

    @property
    def stmt(self):
        return self._stmt

    @property
    def rest(self):
        return self._rest

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
        current_stmt._rest = Body(stmt, Empty())

    def extend(self, body):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt._rest = body

    def extend_from_list(self, body):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        for stmt in body:
            current_stmt._rest = Body(stmt, Empty())
            current_stmt = current_stmt.rest

class Args(Node):

    def __init__(self, name, type_, rest=None):
        self._name = name
        self._type = type_
        self._rest = rest or Empty()
        self._keys = ("name", "type_", "rest")

    @property
    def name(self):
        return self._name

    @property
    def type_(self):
        return self._type

    @property
    def rest(self):
        return self._rest

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
        current_arg._rest = Args(name, type_, Empty())

    def extend(self, args):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt._rest = args


class CallArgs(Node):

    def __init__(self, arg, rest=None):
        self._arg = arg
        self._rest = rest or Empty()
        self._keys = ("arg", "rest")

    @property
    def arg(self):
        return self._arg

    @property
    def rest(self):
        return self._rest

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
        current_arg._rest = CallArgs(arg, Empty())

    def extend(self, args):
        current_stmt = self
        while not isinstance(current_stmt.rest, Empty):
            current_stmt = current_stmt.rest
        current_stmt._rest = args

    def __len__(self):
        length = 1
        current_arg = self
        while not isinstance(current_arg.rest, Empty):
            current_arg = current_arg.rest
            length += 1
        return length


class SExpr(Node):

    def __init__(self, op, expr1, expr2):
        self._op = op
        self._expr1 = expr1
        self._expr2 = expr2
        self._keys = ("op", "expr1", "expr2")

    @property
    def op(self):
        return self._op

    @property
    def expr1(self):
        return self._expr1

    @property
    def expr2(self):
        return self._expr2


class Decl(Node):
    """Declaration and (optionally) initialization of variable.

                type_
               vvvvvvv
    var myVar: Integer = 1 + 20
        ^^^^^            ^^^^^^
        name              expr

    """

    def __init__(self, name, type_, expr):
        self._name = name
        self._type = type_
        self._expr = expr
        self._keys = ("name", "type_", "expr")

    @property
    def name(self):
        return self._name

    @property
    def type_(self):
        return self._type

    @property
    def expr(self):
        return self._expr


class Assignment(Node):
    """Assign value to already existing variable.

          op
          v
    myVar = 1 + 20
    ^^^^^   ^^^^^^
    name     expr

    """

    def __init__(self, name, op, expr):
        self._name = name
        self._op = op
        self._expr = expr
        self._keys = ("name", "op", "expr")

    @property
    def name(self):
        return self._name

    @property
    def op(self):
        return self._op

    @property
    def expr(self):
        return self._expr


class Func(Node):
    """Declaration of function.

         name                                      type_
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """

    def __init__(self, name, args, type_, body):
        self._name = name
        self._args = args
        self._type = type_
        self._body = body
        self._keys = ("name", "args", "type_", "body")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args

    @property
    def type_(self):
        return self._type

    @property
    def body(self):
        return self._body


class Method(Node):
    """Declaration of method.

          name                                      type_
        vvvvvvvv                                  vvvvvvvvvv
    fun myMethod(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                             args                             body
    """

    def __init__(self, name, args, type_, body):
        self._name = name
        self._args = args
        self._type = type_
        self._body = body
        self._keys = ("name", "args", "type_", "body")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args

    @property
    def type_(self):
        return self._type

    @property
    def body(self):
        return self._body


class Struct(Node):
    """Declaration of struct.
         name
        vvvvvv
    sct MyType {
        length: Integer    < body
        data: String       < body
    }
    """


    def __init__(self, name, body):
        self._name = name
        self._body = body
        self._keys = ("name", "body")

    @property
    def name(self):
        return self._name

    @property
    def body(self):
        return self._body


class Field(Node):
    """Declaration of field in struct.

    sct MyType {
        length: Integer     < Field
        data: String        < Field
    }
    """

    def __init__(self, name, type_):
        self._name = name
        self._type = type_
        self._keys = ("name", "type_")

    @property
    def name(self):
        return self._name

    @property
    def type_(self):
        return self._type


class Return(Node):
    """Return statement.

    ret 1 + 2
        ^^^^^
        expr
    """

    def __init__(self, expr):
        self._expr = expr
        self._keys = ("expr", )

    @property
    def expr(self):
        return self._expr


class Literal(Node):

    def __init__(self, literal):
        self._literal = literal
        self._keys = ("literal", )

    @property
    def literal(self):
        return self._literal

    @classmethod
    def to_type(cls):
        return cls._type


class Unref(Literal):
    """unref in Adrian."""
    _type = TypeName("Unref")


class Ref(Literal):
    """ref in Adrian."""
    _type = TypeName("Ref")


class Integer(Literal):
    """Integer in Adrian."""
    _type = TypeName("Integer")



class String(Literal):
    """String in Adrian."""
    _type = TypeName("String")


class CLiteral(Literal):

    @classmethod
    def to_type(cls):
        return CType(cls._type)


class CChar(CLiteral):
    """CChar in Adrian."""
    _type = "Char"


class CIntFast8(CLiteral):
    """CIntFast8 in Adrian."""
    _type = "IntFast8"


class CIntFast32(CLiteral):
    """CIntFast32 in Adrian."""
    _type = "IntFast32"


class CIntFast64(CLiteral):
    """CIntFast64 in Adrian."""
    _type = "IntFast64"


class CUIntFast8(CLiteral):
    """CUIntFast8 in Adrian."""
    _type = "UIntFast8"


class CUIntFast32(CLiteral):
    """CUIntFast32 in Adrian."""
    _type = "UIntFast32"


class CUIntFast64(CLiteral):
    """CUIntFast64 in Adrian."""
    _type = "UIntFast64"


class CString(CLiteral):
    """CString in Adrian."""
    _type = "String"


class CVoid(BaseNode):
    """CVoid in Adrian."""

    def __str__(self):
        return "Void"

    __repr__ = __str__


class StructScalar(Node):

    def __init__(self, name):
        self._name = name
        self._keys = ("name", )

    @property
    def name(self):
        return self._name


CTYPES = (
    CIntFast8,
    CIntFast32,
    CIntFast64,
    CUIntFast8,
    CUIntFast32,
    CUIntFast64,
    CChar,
    CString
)
CTYPES_NAMES = set(str(type_.to_type()) for type_ in CTYPES)