from . import astlib, layers
from .utils import A


class DebugFormatter(layers.Layer):

    def __init__(self, current_ind_level=None):
        self.current_ind_level = current_ind_level or 0
        self.b = layers.b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    def inc_ind(self):
        self.current_ind_level += 2
        self.b = layers.b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    def deinc_ind(self):
        self.current_ind_level -= 2
        self.b = layers.b(
            DebugFormatter, current_ind_level=self.current_ind_level+2)

    def stmt_with_body(self, decl_string, body):
        body = self.b(body)
        self.inc_ind()
        b = []
        for s in body:
            if s in A(astlib.Name, astlib.GenericType, astlib.DataMember):
                b.append("".join([" " * self.current_ind_level, self.t(s)]))
            else:
                b.append("".join([" " * self.current_ind_level, s]))
        self.deinc_ind()
        yield "\n".join([
            decl_string
        ] + b + [" "*self.current_ind_level + "}"])

    def e_callable_name(self, callabletype, parent, name):
        if callabletype == astlib.CallableT.cfunc:
            return "c#{}".format(self.n(name))
        elif callabletype == astlib.CallableT.struct_func:
            return "{}.{}".format(
                self.t(parent), self.e(name))
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
            elif expr.datatype == astlib.DataT.adt:
                return "{}.{}".format(self.e(expr.parent), self.e(expr.member))
            return "{}#{}".format(self.e(expr.parent), self.n(expr.member))
        elif expr in A(astlib.StructScalar):
            return "!*{}".format(self.t(expr.type_))
        elif expr in A(astlib.Literal):
            return str(expr.literal)
        elif expr in A(str):
            return expr
        elif expr in A(astlib.Empty):
            return "EMPTY"

    def t(self, type_):
        if type_ in A(astlib.Name):
            return self.n(type_)
        elif type_ in A(astlib.GenericType):
            return "{}({})".format(
                self.t(type_.base),
                ", ".join([
                    self.t(param) for param in type_.params]))
        elif type_ in A(astlib.DataMember):
            return "{}#{}".format(self.n(type_.parent), self.t(type_.member))
        elif type_ in A(astlib.Void):
            return str(type_)
        elif type_ in A(astlib.LiteralType):
            return type_.type_

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

    def _if(self, stmt):
        yield from self.stmt_with_body(" ".join([
            "if",
            self.e(stmt.expr),
            "{"
        ]), stmt.body)

    def _elif(self, stmt):
        yield from self.stmt_with_body(" ".join([
            "elif",
            self.e(stmt.expr),
            "{"
        ]), stmt.body)

    def _else(self, stmt):
        yield from self.stmt_with_body(" ".join([
            "else", "{"
        ]), stmt.body)

    @layers.register(astlib.Cond)
    def cond(self, stmt: astlib.Cond):
        if_stmt = list(self._if(stmt.if_))
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.extend(self._elif(elif_))
        if stmt.else_ is None:
            else_ = []
        else:
            else_ = list(self._else(stmt.else_))
        yield " ".join(if_ + elifs_ + else_)

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
