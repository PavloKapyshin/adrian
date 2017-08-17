from . import layers, astlib, errors, defs
from .patterns import A
from .context import context


def n(name):
    for_processing = name
    is_tmp = False
    if name in A(astlib.Name):
        for_processing = str(name)
        is_tmp = name.is_tmp
    result = defs.ADR_PREFIX
    if not is_tmp:
        result += "_".join(["", defs.USER_PREFIX])
    if context.file_hash != "":
        result += "_".join(["", context.file_hash])
    result += "_".join(["", for_processing])
    return astlib.Name(result)


def t(type_):
    if type_ in A(astlib.CType):
        return type_

    errors.not_implemented(
        context.exit_on_error,
        "namespacing: type (type {})".format(type_))


def decl_args(args):
    result = []
    for arg in args:
        result.append(astlib.Arg(n(arg.name), t(arg.type_)))
    return result

def call_args(args):
    return list(map(e, args))


def e(expr):
    if expr in A(astlib.Name):
        return n(expr)

    if expr in A(astlib.Expr):
        return astlib.Expr(
            expr.op, e(expr.lexpr), e(expr.rexpr))

    if expr in A(astlib.CINT_TYPES):
        return expr

    if expr in A(astlib.CFuncCall):
        return astlib.CFuncCall(
            expr.name, call_args(expr.args))

    if expr in A(astlib.FuncCall):
        return astlib.FuncCall(
            n(expr.name), call_args(expr.args))

    if expr in A(astlib.StructScalar):
        return astlib.StructScalar(t(expr.type_))

    if expr in A(astlib.Deref):
        return astlib.Deref(e(expr.expr))

    errors.not_implemented(
        context.exit_on_error,
        "namespacing: expr (expr {})".format(expr))


class NameSpacing(layers.Layer):

    def b(self, body):
        reg = NameSpacing().get_registry()
        return list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            n(decl.name), t(decl.type_), e(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            e(assment.var), assment.op,
            e(assment.expr))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield astlib.CFuncCall(
            call.name, call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(e(return_.expr))

    @layers.register(astlib.Func)
    def func(self, func):
        yield astlib.Func(
            n(func.name), decl_args(func.args),
            t(func.rettype), self.b(func.body))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        yield astlib.Struct(
            n(struct.name), struct.parameters, struct.protocols,
            self.b(struct.body))

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            n(field.name), t(field.type_))
