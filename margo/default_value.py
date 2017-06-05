from . import ast
from . import defs
from . import layers
from . import errors

from vendor.paka import funcreg


class DefaultValue(layers.Layer):

    def _get_default_expr(self, type_):
        return self._get_default_expr.reg[type_](self, type_)

    _get_default_expr.reg = funcreg.TypeRegistry()

    @_get_default_expr.reg.register(ast.Name)
    def _get_default_expr_from_name(self, name):
        d = {
            ast.Integer.to_string(): ast.Integer("0"),
            ast.String.to_string(): ast.String("")
        }
        if name.value in d:
            return d[name.value]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_get_default_expr.reg.register(ast.ModuleMember)
    def _get_default_expr_from_module(self, module):
        if module.name.value == defs.C_MODULE_NAME:
            d = {
                "IntFast8": (ast.CIntFast8, "0"),
                "IntFast32": (ast.CIntFast32, "0"),
                "UIntFast8": (ast.CUIntFast8, "0"),
                "UIntFast32": (ast.CUIntFast32, "0")
            }
            if module.member.value in d:
                entry = d[module.member.value]
                return entry[0](entry[1])
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr or self._get_default_expr(type_)
        return ast.Decl(name, type_, expr)