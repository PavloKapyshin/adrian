from . import ast
from . import defs
from . import layers
from . import errors

from vendor.adrian import cgen
from vendor.paka import funcreg


class CGen(layers.Layer):

    def _to_cop(self, op):
        d = {
            "+": cgen.COps.plus,
            "-": cgen.COps.minus,
            "*": cgen.COps.star,
            "/": cgen.COps.slash,
        }
        if op in d:
            return d[op]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _to_ctype(self, type_):
        return self._to_ctype.reg[type_](self, type_)

    _to_ctype.reg = funcreg.TypeRegistry()

    @_to_ctype.reg.register(ast.CIntFast8)
    @_to_ctype.reg.register(ast.CUIntFast8)
    @_to_ctype.reg.register(ast.CIntFast32)
    @_to_ctype.reg.register(ast.CUIntFast32)
    def _to_ctype_catom(self, catom):
        d = {
            ast.CIntFast8: cgen.CTypes.int_fast8,
            ast.CUIntFast8: cgen.CTypes.uint_fast8,
            ast.CIntFast32: cgen.CTypes.int_fast32,
            ast.CUIntFast32: cgen.CTypes.uint_fast32,
        }
        if type(catom) in d:
            return d[type(catom)]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    def _expr_catom(self, catom):
        cgen.Val(catom.value, self._to_ctype(catom))

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        return cgen.Expr(
            self._to_cop(lst[0]),
            self._expr(lst[1]),
            self._expr(lst[2]))

    def decl(self, stmt):
        name = stmt.name
        expr = self._expr(stmt.expr)
        return cgen.Decl(name.value, expr)