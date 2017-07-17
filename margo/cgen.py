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

    def _decl(self, decl):
        return cgen.Decl(
            name=str(decl.name), type_=self._type(decl.type_),
            expr=self._expr(decl.expr))

    def _func_body(self, body):
        # TODO: support non-empty-list body
        return ([] if isinstance(body, astlib.Empty) else body.as_list())
        # for stmt in body:
        #     yield self._pair(stmt)

    def _func_args(self, args):
        new_args = []
        for arg in ([] if isinstance(args, astlib.Empty) else args.as_list()):
            new_args.append(cgen.Decl(str(arg[0]), type_=self._type(arg[1])))
        return new_args

    def _func_decl(self, func_decl):
        return cgen.Func(
            name=str(func_decl.name), rettype=self._type(func_decl.type_),
            args=self._func_args(func_decl.args),
            body=self._func_body(func_decl.body))

    reg = {
        astlib.Decl: _decl,
        astlib.FuncDecl: _func_decl
    }

    @layers.preregister(astlib.Pair)
    def _pair(self, pair):
        yield self.reg[type(pair.stmt)](self, pair.stmt)
