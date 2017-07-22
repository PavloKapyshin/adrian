"""Generates CGen AST."""

from . import cdefs, layers, astlib, errors
from vendor.adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast32": "int_fast32",
    "UIntFast8": "uint_fast8",
    "UIntFast32": "uint_fast32",
    "Void": "void",
}


class CGen(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.CType):
            return getattr(cgen.CTypes, TO_CTYPE[str(type_)])
        errors.not_implemented("type is not supported")

    def expr(self, expr):
        if isinstance(expr, (
                astlib.CIntFast8, astlib.CIntFast32,
                astlib.CUIntFast8, astlib.CUIntFast32)):
            return cgen.Val(
                literal=expr.literal,
                type_=getattr(cgen.CTypes, TO_CTYPE[str(expr.to_type())]))
        errors.not_implemented("expr is not supported")

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=self.type_(decl.type_),
            expr=self.expr(decl.expr))
