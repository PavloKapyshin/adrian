from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


class Analyzer(layers.Layer):

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        return name

    @_expr.reg.register(ast.StructElem)
    def _expr_struct(self, struct):
        return struct

    @_expr.reg.register(ast.Integer)
    @_expr.reg.register(ast.String)
    def _expr_atom(self, atom):
        return atom

    @_expr.reg.register(ast.MethodCall)
    def _expr_method(self, method):
        if isinstance(method.method, ast.ModuleMember):
            if method.method.name.value == defs.C_MODULE_NAME:
                return self._to_c_data(method.method.member, method.args)
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        return [
            lst[0],
            self._expr(lst[1]),
            self._expr(lst[2])
        ]

    def _to_c_data(self, type_, args):
        d = {
            "IntFast8": ast.CIntFast8,
            "IntFast32": ast.CIntFast32,
            "UIntFast8": ast.CUIntFast8,
            "UIntFast32": ast.CUIntFast32,
            "Char": ast.CChar,
        }
        if not type_.value in d:
            errors.not_implemented(
                self.context.line, self.context.exit_on_error)
        if len(args) != 1:
            errors.wrong_number_of_args(
                self.context.line, self.context.exit_on_error,
                expected="1", got=str(len(args)))
        return d[type_.value](args[0].value)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        if expr:
            expr = self._expr(expr)
        return ast.Decl(name, type_, expr)