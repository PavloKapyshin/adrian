import copy
import collections

from . import cdefs


class Includes:

    def __init__(self):
        self._includes = {}

    def __contains__(self, name):
        return name in self._includes

    def __getitem__(self, key):
        return self._includes[key]

    def __setitem__(self, key, value):
        self._includes[key] = value

    def get_cgen_ast(self):
        ast_ = []
        for key, value in self._includes.items():
            for key_, value_ in value.items():
                if key_ != "__INCLUDE__":
                    ast_.append(value_)
        return ast_


class Namespace:

    def __init__(self):
        self._scope = 0
        self._space = {self._scope: {}}

    def add(self, name, value):
        self._space[self._scope][name] = value

    def del_(self, name):
        del self._space[self._scope][name]

    def add_scope(self):
        self._scope += 1
        self._space[self._scope] = {}

    def del_scope(self):
        self._scope -= 1
        del self._space[self._scope]

    def exists(self, name):
        scope = self._scope
        while scope >= 0:
            if name in self._space[scope]:
                return True
            scope -= 1
        return False

    def exists_in_scope(self, name, scope):
        return name in self._space[scope]

    def exists_in_current_scope(self, name):
        return self.exists_in_scope(name, self._scope)

    def get_with_scope(self, name):
        scope = self._scope
        while scope >= 0:
            if self.exists_in_scope(name, scope):
                return self._space[scope][name], scope
            scope -= 1
        return None, 0

    def get(self, name):
        return get_with_scope(name)[0]

    def get_from_current_scope(self, name):
        if self.exists_in_current_scope(name):
            return self._space[self._scope][name]
        return None

    def update_in_current_scope(self, name, values):
        for key, val in values.items():
            self._space[self._scope][name][key] = val

    def update(self, name, values):
        _, scope = self.get_with_scope(name)
        for key, val in values.items():
            self._space[scope][name][key] = val

    @property
    def space(self):
        return self._space

    @property
    def scope(self):
        return self._scope


class Context:

    def __init__(self, exit_on_error):
        self.namespace = Namespace()
        self.typespace = Namespace()
        self.funcspace = Namespace()
        self.exit_on_error = exit_on_error
        # TODO
        # self.namespace = Namespace()
        # self.typespace = Namespace()
        # self.funcspace = Namespace()
        # self.includes = Includes()
        # self.exit_on_error = exit_on_error
        # self.module_paths = module_paths
        # self.var_types = {}

    def copy(self):
        return copy.deepcopy(self)


class _Node:
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
    __slots__ = ()

    def __init__(self, data):
        super().__init__(data)


class ModuleMember(_Node):
    __slots__ = ("_name", "_member", "_keys")

    def __init__(self, name, member):
        self._name = name
        self._member = member
        self._keys = ("name", "member")

    @property
    def name(self):
        return self._name

    @property
    def member(self):
        return self._member


class StructElem(_Node):
    __slots__ = ("_name", "_elem", "_keys")

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


class FuncCall(_Node):
    """Function call.

            args
           vvvvvv
    myFunc(1 + 20)
    ^^^^^^
     name
    """
    __slots__ = ("_name", "_args", "_keys")

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


class MethodCall(_Node):
    __slots__ = ("_method", "_args", "_keys")

    def __init__(self, method, args):
        self._method = method
        self._args = args
        self._keys = ("method", "args")

    @property
    def method(self):
        return self._method

    @property
    def args(self):
        return self._args


# Adrian language statements.
class Pair(_Node):
    __slots__ = ("_line", "_stmt", "_keys")

    def __init__(self, line, stmt):
        self._line = line
        self._stmt = stmt
        self._keys = ("line", "stmt")

    @property
    def line(self):
        return self._line

    @property
    def stmt(self):
        return self._stmt


class Decl(_Node):
    """Declaration and (optionally) initialization of variable.

                type_
               vvvvvvv
    var myVar: Integer = 1 + 20
        ^^^^^            ^^^^^^
        name              expr

    """
    __slots__ = ("_name", "_type", "_expr", "_keys")

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


class Assignment(_Node):
    """Assign value to already existing variable.

          op
          v
    myVar = 1 + 20
    ^^^^^   ^^^^^^
    name     expr

    """
    __slots__ = ("_name", "_op", "_expr", "_keys")

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


class FuncDecl(_Node):
    """Declaration of function.

         name                                      type_
        vvvvvv                                  vvvvvvvvvv
    fun myFunc(arg1: Type1; arg2, arg3: Type2): ReturnType {...}
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^
                           args                             body
    """
    __slots__ = ("_name", "_args", "_type", "_body", "_keys")

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


class StructDecl(_Node):
    """Declaration of struct.
         name
        vvvvvv
    sct MyType {
        length: Integer    < body
        data: String       < body
    }
    """

    __slots__ = ("_name", "_body", "_keys")

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


class Return(_Node):
    """Return statement.

    ret 1 + 2
        ^^^^^
        expr
    """
    __slots__ = ("_expr", "_keys")

    def __init__(self, expr):
        self._expr = expr
        self._keys = ("expr", )

    @property
    def expr(self):
        return self._expr


# Some Adrian and C atoms and special AST nodes.
class Literal(_Node):
    __slots__ = ("_type", "_literal", "_keys")

    def __init__(self, literal):
        self._literal = literal
        self._keys = ("literal", )

    @property
    def literal(self):
        return self._literal

    @classmethod
    def to_type(cls):
        return cls._type


class Integer(Literal):
    """Integer in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("Integer")



class String(Literal):
    """String in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("String")


class CLiteral(Literal):

    @classmethod
    def to_type(cls):
        return ModuleMember(
            name=cdefs.CMODULE_NAME,
            member=cls._type)


class CChar(CLiteral):
    """CChar in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("Char")


class CIntFast8(CLiteral):
    """CIntFast8 in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("IntFast8")


class CIntFast32(CLiteral):
    """CIntFast32 in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("IntFast32")


class CUIntFast8(CLiteral):
    """CUIntFast8 in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("UIntFast8")


class CUIntFast32(CLiteral):
    """CUIntFast32 in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("UIntFast32")


class CString(CLiteral):
    """CString in Adrian."""

    def __init__(self, literal):
        super().__init__(literal)
        self._type = Name("String")


class CVoid:
    """CVoid in Adrian."""
    __slots__ = ()

    def __str__(self):
        return "Void"

    __repr__ = __str__


class StructScalar(_Node):
    __slots__ = ("_name", "_keys")

    def __init__(self, name):
        self._name = name
        self._keys = ("name", )

    @property
    def name(self):
        return self._name


class Arg(_Node):
    __slots__ = ("_name", "_type", "_keys")

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