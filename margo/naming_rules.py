from . import ast
from . import defs
from . import layers
from . import errors


class NamingRules(layers.Layer):

    def _matches_variable_name(self, name):
        return defs.VARIABLE_REGEX.fullmatch(name)

    def _matches_type_name(self, name):
        return defs.TYPE_REGEX.fullmatch(name)

    def _matches_module_name(self, name):
        return defs.MODULE_REGEX.fullmatch(name)

    def _decl_name(self, name):
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error, name.value)
        return name

    def _type_name(self, name):
        if not self._matches_type_name(name.value):
            errors.bad_name_for_type(
                self.context.line, self.context.exit_on_error, name.value)
        return name

    def _module_name(self, name):
        if not self._matches_module_name(name):
            errors.bad_name_for_module(
                self.context.line, self.context.exit_on_error, name)
        return name

    def _decl_type(self, type_):
        if isinstance(type_, ast.Name):
            return self._type_name(type_)
        elif isinstance(type_, ast.ModuleMember):
            return ast.ModuleMember(
                self._module_name(type_.module_name),
                self._decl_type(type_.member))
        elif isinstance(type_, ast.StructElem):
            return ast.StructElem(
                self._decl_type(type_.struct),
                self._name(type_.elem))
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _name(self, name):
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error, name.value)
        return name

    def _call_args(self, args):
        result = []
        for arg in args:
            result.append(self._expr(arg))
        return result

    def _expr(self, expr):
        if isinstance(expr, ast.Name):
            return self._name(expr)
        elif isinstance(expr, defs.ATOM_TYPES):
            return expr
        elif isinstance(expr, list):
            return [
                expr[0],
                self._expr(expr[1]),
                self._expr(expr[2])
            ]
        elif isinstance(expr, ast.MethodCall):
            return ast.MethodCall(
                self._decl_type(expr.method), self._call_args(expr.args))
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        self.name = self._decl_name(stmt.name)
        self.type_ = stmt.type_
        if stmt.type_:
            self.type_ = self._decl_type(stmt.type_)
        self.expr = stmt.expr
        if stmt.expr:
            self.expr = self._expr(stmt.expr)
        return ast.Decl(self.name, self.type_, self.expr)