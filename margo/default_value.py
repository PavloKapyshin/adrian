from . import ast, defs, layers, errors

from vendor.paka import funcreg


class DefaultValue(layers.Layer):
    _type_to_value = {
        "IntFast8": (ast.CIntFast8, "0"),
        "UIntFast8": (ast.CUIntFast8, "0"),
        "IntFast32": (ast.CIntFast32, "0"),
        "UIntFast32": (ast.CUIntFast32, "0"),

        "Integer": (ast.Integer, "0"),
        "String": (ast.String, ""),
    }

    def _get_default_value(self, type_):
        return self._get_default_value.registry[type_](self, type_)

    _get_default_value.registry = funcreg.TypeRegistry()

    @_get_default_value.registry.register(ast.TypeName)
    def _get_default_value_from_name(self, name):
        if name in self._type_to_value:
            entry = self._type_to_value[name]
            type_ = entry[0]
            args = entry[1]
            return type_(args)
        errors.not_implemented(self.position, self.exit_on_error)

    @_get_default_value.registry.register(ast.ModuleMember)
    def _get_default_value_from_module(self, module):
        if module.member in self._type_to_value:
            entry = self._type_to_value[module.member]
            type_ = entry[0]
            args = entry[1]
            return type_(args)
        errors.not_implemented(self.position, self.exit_on_error)

    def decl(self, stmt):
        expr = stmt.expr
        if isinstance(stmt.expr, ast.Empty):
            expr = self._get_default_value(stmt.type_)
        return ast.Decl(stmt.name, stmt.type_, expr)