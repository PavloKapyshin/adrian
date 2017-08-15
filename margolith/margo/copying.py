from . import layers, astlib, errors, inference
from .context import context, get
from .patterns import A


def get_assment(name, val):
    return astlib.Assignment(
        astlib.Deref(name), "=", val)


def get_val(value):
    if value in A(astlib.Name):
        return astlib.Deref(value)
    if value in A(astlib.Expr):
        return astlib.Expr(
            value.op, get_val(value.lexpr), get_val(value.rexpr))
    return value


def heapify(expr, name):
    type_ = inference.infer(expr)
    allocation = astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(type_)])])
    assignment = get_assment(name, get_val(expr))
    return allocation, [assignment]

def e(expr, name):
    if expr in A(astlib.CTYPES):
        return heapify(expr, name)

    if expr in A(astlib.Name):
        if get(expr)["type"] in A(astlib.CType):
            return heapify(expr, name)
        return expr, []

    if expr in A(astlib.Expr):
        return heapify(expr, name)

    errors.not_implemented(
        context.exit_on_error,
        "copying:e (expr {})".format(expr))


class Copying(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
            "type": decl.type_
        })
        new_expr, assignments = e(decl.expr, decl.name)
        yield astlib.Decl(decl.name, decl.type_, new_expr)
        yield from assignments

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield get_assment(assment.var, assment.expr)