from . import layers, astlib, errors, inference
from .context import context, get
from .patterns import A


def heapify(expr, name):
    type_ = inference.infer(expr)
    allocation = astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(type_)])])
    value = expr
    if value in A(astlib.Name):
        value = astlib.Deref(value)
    assignment = astlib.Assignment(
        astlib.Deref(name), "=", value)
    return allocation, [assignment]

def e(expr, name):
    if expr in A(astlib.CTYPES):
        return heapify(expr, name)

    if expr in A(astlib.Name):
        if get(expr)["type"] in A(astlib.CType):
            return heapify(expr, name)
        return expr, []

    errors.not_implemented("objects:e (expr {})".format(expr))


class Copying(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
            "type": decl.type_
        })
        new_expr, assignments = e(decl.expr, decl.name)
        yield astlib.Decl(decl.name, decl.type_, new_expr)
        yield from assignments