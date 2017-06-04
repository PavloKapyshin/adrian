from . import ast
from . import defs
from . import layers
from . import errors

from vendor.paka import funcreg


class TypeInference(layers.Layer):

    def _types_equal(self, type1, type2):
        return self._types_equal.reg[type1](self, type1, type2)

    _types_equal.reg = funcreg.TypeRegistry()

    @_types_equal.reg.register(ast.Name)
    def _types_equal_name(self, type1, type2):
        return type1.value == type2.value

    @_types_equal.reg.register(ast.ModuleMember)
    def _types_equal_name(self, type1, type2):
        return (type1.name == type2.name and \
                self._types_equal(type1.member, type2.member))

    def _get_type_from_expr(self, expr):
        return self._get_type_from_expr.reg[expr](self, expr)

    _get_type_from_expr.reg = funcreg.TypeRegistry()

    @_get_type_from_expr.reg.register(ast.Name)
    def _get_type_from_name(self, name):
        return self.context.namespace.get(name.value)["type_"]

    @_get_type_from_expr.reg.register(ast.MethodCall)
    def _get_type_from_method(self, method):
        if not isinstance(method.method, ast.StructElem):
            return method.method
        elif method.method.elem.value == "__init__":
            return method.method.name
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_get_type_from_expr.reg.register(ast.Integer)
    @_get_type_from_expr.reg.register(ast.String)
    @_get_type_from_expr.reg.register(ast.CIntFast8)
    @_get_type_from_expr.reg.register(ast.CUIntFast8)
    @_get_type_from_expr.reg.register(ast.CIntFast32)
    @_get_type_from_expr.reg.register(ast.CUIntFast32)
    @_get_type_from_expr.reg.register(ast.CChar)
    def _get_type_from_atom(self, atom):
        return atom.to_type()

    @_get_type_from_expr.reg.register(list)
    def _get_type_from_list(self, lst):
        # TODO: maybe typeOf(4 / 2) == Decimal.
        expr_type1 = self._get_type_from_expr(lst[1])
        expr_type2 = self._get_type_from_expr(lst[2])
        if not self._types_equal(expr_type1, expr_type2):
            errors.types_are_not_equal(
                self.context.line, self.context.exit_on_error,
                expr_type1, expr_type2)
        return expr_type1

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        if not type_:
            type_ = self._get_type_from_expr(expr)
        self.context.namespace.add_name(name.value, {
            "type_": type_
        })
        return ast.Decl(name, type_, expr)