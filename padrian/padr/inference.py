from .utils import A
from .context import context
from . import astlib

def infer_type(expr):
    if expr in A(astlib.Callable):
        if expr.callabletype == astlib.CallableT.struct:
            return expr.name
