class _Atom:
    _keys = ()  # Override in subclass.

    def __str__(self):
        return "{}({})".format(
            self.__class__.__name__,
            ", ".join(
                "{}={!r}".format(
                    key, getattr(self, key)) for key in self._keys))

    __repr__ = __str__


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


class Name(_Atom):
    """Name concept in Adrian.

    Is used to represent:
     - variable
     - constant
     - type

    """
    _keys = ("value", )

    def __init__(self, value):
        self.value = value


class ReturnStmt(_Atom):
    """Return statement.

    return 1 + 2
           ^^^^^
           value
    """
    _keys = ("value", )

    def __init__(self, value):
        self.value = value


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
    struct MyType {
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
    var myVar: Integer = 1 + 2
        ^^^^^            ^^^^^
        name             value

    """
    _keys = ("name", "type_", "value")

    def __init__(self, name, type_, value):
        self.name = name
        self.type_ = type_
        self.value = value


class Assignment(_Atom):
    """Assign value to already existing variable.

          op
          v
    myVar = 1 + 2
    ^^^^^   ^^^^^
    name    value

    """
    _keys = ("name", "op", "value")

    def __init__(self, name, op, value):
        self.name = name
        self.op = op
        self.value = value


class ModuleMember(_Atom):
    _keys = ("module_name", "member")

    def __init__(self, module_name, member):
        self.module_name = module_name
        self.member = member


class StructElem(_Atom):
    _keys = ("struct_name", "elem")

    def __init__(self, struct_name, elem):
        self.struct_name = struct_name
        self.elem = elem
