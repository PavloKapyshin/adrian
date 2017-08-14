from . import astlib, errors
from .context import context, get
from .patterns import A


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()

    if expr in A(astlib.Name):
        return get(expr)["type"]

    errors.not_implemented(":) (infer)")