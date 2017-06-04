import copy


class _Atom:
    _keys = ()  # Override in subclass.

    def __str__(self):
        return "{}({})".format(
            self.__class__.__name__,
            ", ".join(
                "{}={!r}".format(
                    key, getattr(self, key)) for key in self._keys))

    __repr__ = __str__

    def copy(self):
        return copy.deepcopy(self)


class Pair(_Atom):
    _keys = ("line", "stmt")

    def __init__(self, line, stmt):
        self.line = line
        self.stmt = stmt


class _Namespace:

    def __init__(self):
        self._scope = 0
        self._space = {self._scope: {}}

    def add_name(self, name, value):
        self._space[self._scope][name] = value

    def del_name(self, name):
        del self._space[self._scope][name]

    def add_scope(self):
        self._scope += 1
        self._space[self._scope] = {}

    def del_scope(self):
        self._scope -= 1
        del self._space[self._scope]

    def exists(self, name):
        return name in self._space[self._scope]

    def get(self, name):
        if self.exists(name):
            return self._space[self._scope][name]
        return None

    def update(self, name, values):
        for key, val in values.items():
            self._space[self._scope][name][key] = val

    def space(self):
        return self._space

    @property
    def scope(self):
        return self._scope


class Context:

    def __init__(self, exit_on_error, module_paths):
        self.namespace = _Namespace()
        self.typespace = _Namespace()
        self.funcspace = _Namespace()
        self.includes = []
        self.exit_on_error = exit_on_error
        self.module_paths = module_paths

    def copy(self):
        return copy.deepcopy(self)


class _Value(_Atom):
    _keys = ("value", )

    def __init__(self, value):
        self.value = value

    @classmethod
    def to_string(cls):
        return cls._string


class Integer(_Value):
    """Integer in Adrian."""
    _string = "Integer"


class String(_Value):
    """String in Adrian."""
    _string = "String"


class CChar(_Value):
    """CChar in Adrian."""
    _string = "Char"


class CIntFast8(_Value):
    """CIntFast8 in Adrian."""
    _string = "IntFast8"


class CIntFast32(_Value):
    """CIntFast32 in Adrian."""
    _string = "IntFast32"


class CUIntFast8(_Value):
    """CUIntFast8 in Adrian."""
    _string = "UIntFast8"


class CUIntFast32(_Value):
    """CUIntFast32 in Adrian."""
    _string = "UIntFast32"


class Name(_Atom):
    """Name concept in Adrian.

    Is used to represent on parsing level:
     - variable
     - constant
     - function
     - type

    """
    _keys = ("value", )

    def __init__(self, value):
        self.value = value


class Arg(_Atom):
    _keys = ("name", "type_")

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_


class StructScalar(_Atom):
    _keys = ("name", )

    def __init__(self, name):
        self.name = name


class FuncCall(_Atom):
    """Function call.

            args
           vvvvvv
    myFunc(1 + 20)
    ^^^^^^
     name
    """
    _keys = ("name", "args")

    def __init__(self, name, args):
        self.name = name
        self.args = args


class Return(_Atom):
    """Return statement.

    ret 1 + 2
        ^^^^^
        value
    """
    _keys = ("value", )

    def __init__(self, value):
        self.expr = expr


class FuncDecl(_Atom):
    """Declaration of function.

         name                                      type_
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """
    _keys = ("name", "args", "type_", "body")

    def __init__(self, name, args, type_, body):
        self.name = name
        self.args = args
        self.type_ = type_
        self.body = body


class StructDecl(_Atom):
    """Declaration of struct.
          name
         vvvvvv
    data MyType {
        var length: Integer    < body
        var data: String       < body
    }
    """

    _keys = ("name", "body")

    def __init__(self, name, body):
        self.name = name
        self.body = body


class Decl(_Atom):
    """Declaration and (optionally) initialization of variable.

                type_
               vvvvvvv
    var myVar: Integer = 1 + 20
        ^^^^^            ^^^^^^
        name              expr

    """
    _keys = ("name", "type_", "expr")

    def __init__(self, name, type_, expr):
        self.name = name
        self.type_ = type_
        self.expr = expr


class Assignment(_Atom):
    """Assign value to already existing variable.

          op
          v
    myVar = 1 + 20
    ^^^^^   ^^^^^^
    name     expr

    """
    _keys = ("name", "op", "expr")

    def __init__(self, name, op, expr):
        self.name = name
        self.op = op
        self.expr = expr


class MethodCall(_Atom):
    _keys = ("method", "args")

    def __init__(self, method, args):
        self.method = method
        self.args = args


class ModuleMember(_Atom):
    _keys = ("name", "member")

    def __init__(self, name, member):
        self.name = name
        self.member = member


class StructElem(_Atom):
    _keys = ("name", "elem")

    def __init__(self, name, elem):
        self.name = name
        self.elem = elem
