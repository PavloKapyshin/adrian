from . import ast
from . import defs
from . import layers
from . import errors


class OOP(layers.Layer):

    def _make_func_from_method(self, expr):
        return ast.FuncCall(
            self._method(expr.method),
            self._args(expr.args))

    def _method(self, method):
        if isinstance(method, ast.StructElem):
            struct = self._method(method.struct)
            elem = self._method(method.elem)
            if isinstance(struct, ast.ModuleMember):
                # Name concatination.
                struct.member = elem + struct.member
            else:
                # Name concatination.
                struct = elem + struct
            return struct
        elif isinstance(method, ast.Name):
            return method.value
        elif isinstance(method, ast.ModuleMember):
            method.member = self._method(method.member)
            return method
        elif isinstance(method, ast.MethodCall):
            return self._make_func_from_method(method)
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _args(self, args):
        result = []
        for arg in args:
            if isinstance(arg, ast.MethodCall):
                result.append(self._make_func_from_method(arg))
            else:
                result.append(arg)
        return result

    def _expr(self, expr):
        if isinstance(expr, ast.MethodCall):
            return self._make_func_from_method(expr)
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = self._expr(stmt.expr)
        return ast.Decl(name, type_, expr)