from . import astlib, errors
from .context import context, get
from .patterns import A


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()

    if expr in A(astlib.Name):
        entity = get(expr)
        if entity:
            return entity["type"]
        else:
            errors.non_existing_name(
                context.exit_on_error, name=str(expr))

    if expr in A(astlib.Expr):
        # +, -, * and / returns the value of the same type.
        return infer(expr.lexpr)

    if expr in A(astlib.FuncCall):
        entity = get(expr.name)
        if entity:
            return entity["type"]
        else:
            errors.non_existing_name(
                context.exit_on_error, name=str(expr.name))

    errors.not_implemented(
        context.exit_on_error,
        "can't infer type (expr {})".format(expr))