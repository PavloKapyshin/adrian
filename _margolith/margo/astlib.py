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

    def __init__(self, data, is_tmp=None):
        super().__init__(data)
        self.is_tmp = is_tmp

    def copy(self):
        return copy.deepcopy(self)


class CType(collections.UserString):
    """CType concept in Adrian.

    Is used to represent type from c module
    """

    def __init__(self, data):
        super().__init__(data)

    def copy(self):
        return copy.deepcopy(self)


class ModuleMember(Node):

    def __init__(self, module, member):
        self._module = module
        self._member = member
        self._keys = ("module", "member")

    @property
    def module(self):
        return self._module

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


class ParamedType(Node):

    def __init__(self, base, params):
        self._base = base
        self._params = params
        self._keys = ("base", "params")

    @property
    def base(self):
        return self._base

    @property
    def params(self):
        return self._params


class Instance(Node):
    """Instance.

              args
             vvvvvv
    MyStruct(1 + 20)
    ^^^^^^^^
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

    def __init__(self, base, method, args):
        self._base = base
        self._method = method
        self._args = args
        self._keys = ("base", "method", "args")

    @property
    def base(self):
        return self._base

    @property
    def method(self):
        return self._method

    @property
    def args(self):
        return self._args


class Arg(Node):

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


class Empty(BaseNode):

    def __init__(self):
        pass

    def __str__(self):
        return "EMPTY"

    def as_list(self):
        return []

    __repr__ = __str__


class Types(Node):

    def __init__(self, type_, rest=None):
        self._type = type_
        self._rest = rest or Empty()
        self._keys = ("type_", "rest")

    @property
    def type_(self):
        return self._type

    @property
    def rest(self):
        return self._rest

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
        current_type._rest = Types(type_, Empty())

    def extend(self, types):
        current_type = self
        while not isinstance(current_type.rest, Empty):
            current_type = current_type.rest
        current_type._rest = types

    def extend_from_list(self, types):
        current_type = self
        while not isinstance(current_type.rest, Empty):
            current_type = current_type.rest
        for type_ in types:
            current_type._rest = Types(type_, Empty())
            current_type = current_type.rest


class Names(Node):

    def __init__(self, name, rest=None):
        self._name = name
        self._rest = rest or Empty()
        self._keys = ("name", "rest")

    @property
    def name(self):
        return self._name

    @property
    def rest(self):
        return self._rest

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
        current_name._rest = Names(name, Empty())

    def extend(self, names):
        current_name = self
        while not isinstance(current_name.rest, Empty):
            current_name = current_name.rest
        current_name._rest = names

    def extend_from_list(self, names):
        current_name = self
        while not isinstance(current_name.rest, Empty):
            current_name = current_name.rest
        for name in names:
            current_name._rest = Names(name, Empty())
            current_name = current_name.rest


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


class Expr(Node):

    def __init__(self, op, lexpr, rexpr):
        self._op = op
        self._lexpr = lexpr
        self._rexpr = rexpr
        self._keys = ("op", "lexpr", "rexpr")

    @property
    def op(self):
        return self._op

    @property
    def lexpr(self):
        return self._lexpr

    @property
    def rexpr(self):
        return self._rexpr


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
     var     expr

    """

    def __init__(self, var, op, expr):
        self._var = var
        self._op = op
        self._expr = expr
        self._keys = ("var", "op", "expr")

    @property
    def var(self):
        return self._var

    @property
    def op(self):
        return self._op

    @property
    def expr(self):
        return self._expr


class Func(Node):
    """Declaration of function.

         name                                     rettype
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """

    def __init__(self, name, args, rettype, body):
        self._name = name
        self._args = args
        self._rettype = rettype
        self._body = body
        self._keys = ("name", "args", "rettype", "body")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args

    @property
    def rettype(self):
        return self._rettype

    @property
    def body(self):
        return self._body


class Method(Node):
    """Declaration of method.

          name                                     rettype
        vvvvvvvv                                  vvvvvvvvvv
    fun myMethod(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                             args                             body
    """

    def __init__(self, name, args, rettype, body):
        self._name = name
        self._args = args
        self._rettype = rettype
        self._body = body
        self._keys = ("name", "args", "rettype", "body")

    @property
    def name(self):
        return self._name

    @property
    def args(self):
        return self._args

    @property
    def rettype(self):
        return self._rettype

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


    def __init__(self, name, param_types, body):
        self._name = name
        self._param_types = param_types
        self._body = body
        self._keys = ("name", "param_types", "body")

    @property
    def name(self):
        return self._name

    @property
    def param_types(self):
        return self._param_types

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
    _type = Name("Unref")


class Ref(Literal):
    """ref in Adrian."""
    _type = Name("Ref")


class Integer(Literal):
    """Integer in Adrian."""
    _type = Name("Integer")



class String(Literal):
    """String in Adrian."""
    _type = Name("String")


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


class CPtr(CLiteral):
    """CPtr in Adrian."""
    _type = "Ptr"


class CCast(Node):

    def __init__(self, expr, to):
        self._expr = expr
        self._to = to
        self._keys = ("expr", "to")

    @property
    def expr(self):
        return self._expr

    @property
    def to(self):
        return self._to


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


CINT_TYPES = (
    CIntFast8,
    CIntFast32,
    CIntFast64,
    CUIntFast8,
    CUIntFast32,
    CUIntFast64
)

CTYPES = (
    CChar,
    CString
) + CINT_TYPES
CTYPES_NAMES = set(str(type_.to_type()) for type_ in CTYPES)