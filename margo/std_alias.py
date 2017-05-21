from . import ast
from . import defs
from . import layers
from . import errors


class StdAlias(layers.Layer):

    def _type_from_alias(self, type_):
        if (isinstance(type_, ast.Name) and \
                type_.value in defs.STD_TYPE_NAMES):
            return ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_)
        return type_

    def _name_to_copy_method(self, name):
        type_ = self.context.namespace.get(name.value)["type"]
        return ast.MethodCall(
            ast.StructElem(type_, ast.Name("copy")), args=[name])

    def _method_call(self, call):
        type_ = call.method
        if not isinstance(type_, ast.StructElem):
            return ast.MethodCall(
                ast.StructElem(type_, ast.Name("init")), call.args)
        elif type_.elem.value == "init":
            # BUG CAN BE HERE!
            return call
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _std_type_init_args_from_alias(self, args):
        result = []
        for arg in args:
            if not isinstance(arg, ast.Integer):
                errors.not_implemented(self.context.line, self.context.exit_on_error)
            # We usually can calculate length of an Integer when compiling.
            result.append(ast.MethodCall(
                ast.StructElem(ast.ModuleMember(
                    defs.C_MODULE_NAME, ast.Name(defs.C_CSTRING)),
                ast.Name("init")),
                [arg.value]))
        return result

    def _atom_to_init_method(self, atom):
        type_ = ast.Name(atom.to_string())
        args = self._std_type_init_args_from_alias([atom])
        return ast.MethodCall(
            ast.StructElem(
                ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("init")), args)

    def _expr_from_alias(self, expr):
        if isinstance(expr, ast.Name):
            return self._name_to_copy_method(expr)
        elif isinstance(expr, ast.MethodCall):
            return self._method_call(expr)
        elif isinstance(expr, defs.ATOM_TYPES):
            return self._atom_to_init_method(expr)
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = self._type_from_alias(stmt.type_)
        expr = self._expr_from_alias(stmt.expr)
        self.context.namespace.add_name(name.value, {"type": type_})
        return ast.Decl(name, type_, expr)