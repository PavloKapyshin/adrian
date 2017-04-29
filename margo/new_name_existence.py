import pathlib

from . import ast
from . import defs
from . import layers
from . import errors


class NameExistence(layers.Layer):

    def _name_exists(self, name):
        return (name in defs.STD_FUNCS or \
                self.context.namespace.exists(name))

    def _type_exists(self, type_):
        return (type_ in defs.STD_TYPE_NAMES or \
                self.context.typespace.exists(type_))

    def _module_exists(self, module_name):
        if module_name == defs.C_MODULE_NAME:
            return True
        elif module_name in defs.STD_MODULES_NAMES:
            return True
        for path in self.context.module_paths:
            for module in pathlib.Path(path).iterdir():
                if module_name == module.name:
                    return True
        return False


    def _module_name(self, module_name):
        if not self._module_exists(module_name):
            errors.non_existing_module(
                self.context.line, self.context.exit_on_error, module_name)
        return module_name

    def _decl_type(self, type_):
        if isinstance(type_, ast.Name):
            if not self._type_exists(type_.value):
                errors.non_existing_type(
                    self.context.line, self.context.exit_on_error,
                    type_.value)
            return type_
        elif isinstance(type_, ast.ModuleMember):
            # TODO: think about type in module
            return ast.ModuleMember(
                self._module_name(type_.module_name), type_.member)
        elif isinstance(type_, ast.StructElem):
            return ast.StructElem(
                self._decl_type(type_.struct),
                self._name(type_.elem))
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _decl_name(self, name):
        # If std name.
        if name.value in defs.STD_FUNCS:
            errors.cant_reassign_builtin(
                self.context.line, self.context.exit_on_error, name.value)
        return name

    def _name(self, name):
        # If name exists.
        if isinstance(name, ast.Name):
            if self._name_exists(name.value):
                return name
            errors.non_existing_name(
                self.context.line, self.context.exit_on_error, name.value)
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _call_args(self, args):
        result = []
        for arg in args:
            result.append(self._expr(arg))
        return result

    def _expr(self, expr):
        if isinstance(expr, ast.Name):
            return self._name(expr)
        elif isinstance(expr, ast.MethodCall):
            if isinstance(expr.method, ast.StructElem):
                if isinstance(expr.method.elem, ast.Name):
                    return ast.MethodCall(
                        ast.StructElem(
                            self._decl_type(expr.method.struct),
                            expr.method.elem),
                        self._call_args(expr.args))
            return ast.MethodCall(
                self._decl_type(expr.method), self._call_args(expr.args))
        elif isinstance(expr, defs.ATOM_TYPES):
            # No names here.
            return expr
        elif isinstance(expr, list):
            return [
                expr[0],
                self._expr(expr[1]),
                self._expr(expr[2])
            ]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        self.name = self._decl_name(stmt.name)
        self.type_ = stmt.type_
        if stmt.type_:
            self.type_ = self._decl_type(stmt.type_)
        self.expr = stmt.expr
        if stmt.expr:
            self.expr = self._expr(stmt.expr)
        # Register here to avoid linking in expression to itself.
        self.context.namespace.add_name(self.name, {
            "node_type": defs.NodeType.variable
        })
        return ast.Decl(self.name, self.type_, self.expr)