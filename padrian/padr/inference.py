from .utils import A
from .context import context
from . import astlib, errors


def infer_type(expr):
    if expr in A(astlib.Callable):
        if expr.callabletype == astlib.CallableT.struct:
            return expr.name
    if expr in A(astlib.Name):
        return context.env[expr]
    if expr in A(astlib.Expr):
        return infer_type(expr.left)
    errors.not_now(errors.INFERENCE)
