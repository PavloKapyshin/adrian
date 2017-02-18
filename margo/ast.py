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

    def get_name(self, name):
        if self.exist(name):
            return self._space[self._scope][name]
        return None

    @property
    def scope(self):
        return self._scope


class Context:

    def __init__(self):
        self.namespace = _Namespace()
        self.typespace = _Namespace()
        self.funcspace = _Namespace()


class _UnknownType:

    def __str__(self):
        return "UnknownType"

    __repr__ = __str__


UnknownType = _UnknownType()


class _Value(_Atom):
    _keys = ("value", )

    def __init__(self, value):
        self.value = value


class Integer(_Value):
    """Integer in Adrian."""


class String(_Value):
    """String in Adrian."""


class CString(_Value):
    """C string in compiler."""


class _Name(_Atom):
    """Name concept in Adrian.

    Is used to represent:
     - variable
     - constant

    """
    _keys = ("value", "type_")

    def __init__(self, value, type_):
        self.value = value
        self.type_ = type_


class VariableName(_Name):
    """Variable name in Adrian."""


class Assignment(_Atom):
    """Declaration and (optionally) initialization of variable.

                type_     value
                vvvvvvv   vvvvv
    var my_lol: Integer = 1 + 2
        ^^^^^^          ^
        name            op

    """
    _keys = ("name", "value")

    def __init__(self, name, type_, value):
        self.name = VariableName(name, type_=type_ or UnknownType)
        self.value = value
