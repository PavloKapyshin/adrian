from . import astlib, errors
from .context import context, get
from .patterns import A


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()

    if expr in A(astlib.Name):
        return get(expr)["type"]

    if expr in A(astlib.Expr):
        # +, -, * and / returns the value of the same type.
        return infer(expr.lexpr)

    errors.not_implemented(
        context.exit_on_error, "can't infer type (infer)")