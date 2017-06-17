"""Generates CGen AST."""

from . import cdefs, layers, astlib, errors
from vendor.adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast32": "int_fast32",
    "UIntFast8": "uint_fast8",
    "UIntFast32": "uint_fast32",
}


class CGen(layers.Layer):

    def _type(self, type_):
        # Only c types are supported.
        if isinstance(type_, astlib.ModuleMember):
            if type_.name == cdefs.CMODULE_NAME:
                return getattr(cgen.CTypes, TO_CTYPE[type_.member])
        errors.not_implemented()

    def _expr(self, expr):
        if isinstance(expr, astlib.SExpr):
            return cgen.Expr(
                op=expr.op, expr1=self._expr(expr.expr1),
                expr2=self._expr(expr.expr2))
        elif isinstance(
                expr, (
                    astlib.CIntFast8, astlib.CIntFast32,
                    astlib.CUIntFast8, astlib.CUIntFast32)):
            return cgen.Val(
                literal=expr.literal,
                type_=getattr(cgen.CTypes, TO_CTYPE[expr.to_type().member]))
        errors.not_implemented()

    @layers.preregister(astlib.Decl)
    def _decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=self._type(decl.type_),
            expr=self._expr(decl.expr))
