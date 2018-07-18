from . import astlib, errors
from .context import context
from .utils import A


def infer_type(expr):
    if expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.Name):
        return context.env[expr]["type"]
    else:
        errors.later()
