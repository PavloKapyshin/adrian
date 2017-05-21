from . import ast
from . import defs
from . import layers
from . import errors


class TypeChecking(layers.Layer):

    def _types_equal(self, type1, type2):
        if (isinstance(type1, ast.Name) and \
                isinstance(type2, ast.Name)):
            return type1.value == type2.value
        elif (isinstance(type1, ast.ModuleMember) and \
                isinstance(type2, ast.ModuleMember)):
            return (type1.module_name == type2.module_name and \
                    self._types_equal(type1.member, type2.member))
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _get_type_from_method(self, method):
        # TODO: support other methods and structs.
        if not isinstance(method, ast.StructElem):
            return method
        elif method.elem.value == "init":
            return method.struct
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _get_type_from_expr(self, expr):
        if isinstance(expr, ast.Name):
            return self.context.namespace.get(expr.value)["type"]
        elif isinstance(expr, ast.MethodCall):
            if ((not isinstance(expr.method, ast.StructElem)) or \
                    expr.method.elem.value == "init"):
                return self._get_type_from_method(expr.method)
            errors.not_implemented(
                self.context.line, self.context.exit_on_error)
        elif isinstance(expr, defs.ATOM_TYPES):
            return ast.Name(expr.to_string())
        elif isinstance(expr, list):
            # Expression must contain only one type atoms.
            # TODO: maybe typeOf(4 / 2) == Decimal.
            expr1type = self._get_type_from_expr(expr[1])
            expr2type = self._get_type_from_expr(expr[2])
            if self._types_equal(expr1type, expr2type):
                return expr1type
            errors.types_are_not_equal(
                self.context.line, self.context.exit_on_error,
                expr1type, expr2type)
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        if expr:
            type_of_expr = self._get_type_from_expr(expr)
            if not self._types_equal(type_, type_of_expr):
                errors.types_are_not_equal(
                    self.context.line, self.context.exit_on_error,
                    type_, type_of_expr)
        return ast.Decl(name, type_, expr)