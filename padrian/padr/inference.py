from .utils import A
from .context import context
from . import astlib, errors, defs


def get_init_args(type_):
    entry = context.env[type_]
    return [infer_expr(t) for _, t, in entry["methods"]["__init__"]["args"]]


def infer_expr(type_):
    if type_ in A(astlib.LiteralType):
        if type_.type_ == astlib.LiteralT.integer:
            return astlib.Literal(astlib.LiteralT.integer, "0")
    if type_ in A(astlib.DataMember):
        if type_.datatype == astlib.DataT.module:
            return astlib.Callable(
                astlib.CallableT.struct, astlib.Empty(),
                type_, get_init_args(type_))
    errors.not_now(errors.INFERENCE)


def infer_type(expr):
    if expr in A(astlib.Callable):
        if expr.callabletype == astlib.CallableT.struct:
            return expr.name
    if expr in A(astlib.Name):
        return context.env[expr]["type"]
    if expr in A(astlib.Expr):
        return infer_type(expr.left)
    errors.not_now(errors.INFERENCE)
