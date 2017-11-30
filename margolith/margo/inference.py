from . import astlib, errors
from .context import context, get
from .patterns import A


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()

    if expr in A(astlib.Name):
        return get(expr)["type"]

    if expr in A(astlib.FuncCall):
        return get(expr.name)["type"]

    if expr in A(astlib.Expr):
        # +, *, -, / returns value of the same type.
        return infer(expr.right_expr)

    if expr in A(astlib.StructCall):
        return expr.name

    if expr in A(astlib.StructFuncCall):
        methods = get(expr.struct)["methods"]
        method = methods[str(expr.func_name)]
        return method["type"]

    if expr in A(astlib.StructMember):
        type_of_struct = infer(expr.struct)
        fields = get(type_of_struct)["fields"]
        field = fields[str(expr.member)]
        return field["type"]

    if expr in A(astlib.Deref, astlib.Ref):
        return infer(expr.expr)

    errors.not_implemented(
        context.exit_on_error,
        "can't infer type from expression {}".format(expr))