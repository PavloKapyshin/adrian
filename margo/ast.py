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

    def __init__(self, exit_on_error):
        self.namespace = _Namespace()
        self.typespace = _Namespace()
        self.funcspace = _Namespace()
        self.exit_on_error = exit_on_error


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


class CString(_Value):
    """C string in compiler."""
    _string = "CString"


class _Name(_Atom):
    """Name concept in Adrian.

    Is used to represent:
     - variable
     - constant
     - type

    """
    _keys = ("value", )

    def __init__(self, value):
        self.value = value


class Name(_Name):
    """Name in Adrian (lexing and parsing layer)."""


class VariableName(_Name):
    """Variable name in Adrian."""


class TypeName(_Name):
    """Type name in Adrian."""


class Assignment(_Atom):
    """Declaration and (optionally) initialization of variable.

                type_     value
                vvvvvvv   vvvvv
    var my_lol: Integer = 1 + 2
        ^^^^^^          ^
        name            op

    """
    _keys = ("name", "type_", "value")

    def __init__(self, name, type_, value):
        self.name = Name(name)
        self.type_ = type_
        self.value = value


class ModuleMember(_Atom):
    _keys = ("module_name", "member")

    def __init__(self, module_name, member):
        self.module_name = module_name
        self.member = member
