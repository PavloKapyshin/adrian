"""Library for type inferencing."""

from . import astlib, errors


def get_type_from_expr(expr):
    # Only c types are supported.
    if isinstance(expr, astlib.SExpr):
        # TODO: return type = type(op(expr1, expr2)).
        return get_type_from_expr(expr.expr1)
    elif isinstance(
            expr, (
                astlib.CIntFast8, astlib.CIntFast32,
                astlib.CUIntFast8, astlib.CUIntFast32)):
        return expr.to_type()
    errors.not_implemented()
