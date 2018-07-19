from . import astlib, errors
from .context import context
from .utils import A


def infer_type(expr):
    if expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.Name):
        return context.env[expr]["type"]
    elif expr in A(astlib.Expr):
        return infer_type(expr.left)
    else:
        errors.later()
