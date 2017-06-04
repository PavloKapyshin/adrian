from . import ast
from . import defs
from . import layers
from . import errors


class DefaultValue(layers.Layer):

    def _get_c_module_literals(self, type_):
        if type_.value in (defs.C_INT32, defs.C_INT64):
            return [ast.CString("0")]
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _get_default_expr_from_type(self, type_):
        if type_.value == "Integer":
            return ast.Integer("0")
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _get_default_expr_from_module_member(self, type_):
        if type_.module_name == defs.C_MODULE_NAME:
            return ast.MethodCall(
                ast.StructElem(type_, ast.Name("init")),
                self._get_c_module_literals(type_.member))
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _get_default_expr(self, type_):
        if isinstance(type_, ast.Name):
            return self._get_default_expr_from_type(type_)
        elif isinstance(type_, ast.ModuleMember):
            return self._get_default_expr_from_module_member(type_)
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        if not expr:
            expr = self._get_default_expr(type_)
        return ast.Decl(name, type_, expr)