from . import astlib, layers
from .utils import A


class DebugFormatter(layers.Layer):

    def __init__(self, current_ind_level=None):
        self.current_ind_level = current_ind_level or 0
        self.b = layers._b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    def up_ind(self):
        self.current_ind_level += 2
        self.b = layers._b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    def down_ind(self):
        self.current_ind_level -= 2
        self.b = layers._b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    # Misc.
    def stmt_with_body(self, decl_string, body):
        body = self.b(body)
        self.up_ind()
        b = ["".join([" " * self.current_ind_level, s]) for s in body]
        self.down_ind()
        yield "\n".join([
            decl_string
        ] + b + [" "*self.current_ind_level + "}"])

    def e_callable_name(self, callabletype, parent, name):
        if callabletype == astlib.CallableT.cfunc:
            return "c#{}".format(self.n(name))
        elif callabletype == astlib.CallableT.struct_func:
            return "{}.{}".format(
                self.n(parent), self.n(name))
        return self.e(name)

    def e_callable(self, expr):
        return "{}({})".format(
            self.e_callable_name(
                expr.callabletype, expr.parent, expr.name),
            self.a(expr.args))

    # Core
    def e(self, expr):
        if expr in A(astlib.Name):
            return self.n(expr)
        elif expr in A(astlib.Callable):
            return self.e_callable(expr)
        elif expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return "{}.{}".format(self.e(expr.parent), self.n(expr.member))
            return "{}#{}".format(self.e(expr.parent), self.n(expr.member))
        elif expr in A(astlib.StructScalar):
            return "!*{}".format(self.t(expr.type_))
        elif expr in A(astlib.Literal):
            return str(expr.literal)
        else:
            print("WHAT?", expr, type(expr))

    def t(self, type_):
        if type_ in A(astlib.Name):
            return self.n(type_)
        if type_ in A(astlib.ParamedType):
            return "{}({})".format(
                self.n(type_.type_),
                ", ".join([
                    self.t(param) for param in type_.params]))
        return "{}#{}".format(self.n(type_.parent), self.t(type_.member))

    def a(self, args):
        if len(args) == 0:
            return ""
        elif isinstance(args[0], tuple):
            return ", ".join([
                "{}: {}".format(self.n(name), self.t(type_))
                for name, type_ in args])
        return ", ".join([self.e(arg) for arg in args])

    def n(self, name):
        return str(name)

    def p(self, params):
        return ", ".join([str(param) for param in params])

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield "{}: {}".format(self.n(stmt.name), self.t(stmt.type_))
        else:
            yield " ".join([
                ("var" if stmt.decltype == astlib.DeclT.var else "let"),
                "".join([self.n(stmt.name), ":"]),
                self.t(stmt.type_),
                "=",
                self.e(stmt.expr)
            ])

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield "{} {} {}".format(
            self.e(stmt.left), stmt.op, self.e(stmt.right))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield "return {}".format(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield self.e_callable(stmt)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        yield from self.stmt_with_body(" ".join([
                "fun",
                "".join([self.n(stmt.name), "(", self.a(stmt.args), "): ", self.t(stmt.rettype)]),
                "{"
            ]), stmt.body)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        kwd = "struct"
        if stmt.decltype == astlib.DeclT.protocol:
            kwd = "protocol"
        if stmt.decltype == astlib.DeclT.adt:
            kwd = "adt"
        yield from self.stmt_with_body(
            " ".join([
                kwd,
                "".join([self.n(stmt.name), "(", self.p(stmt.params), ")"]),
                "{"
            ]), stmt.body)
