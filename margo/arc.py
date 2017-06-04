from . import ast
from . import defs
from . import errors
from . import layers


class ARC(layers.Layer):

    def _freed(self, name):
        return not self.context.namespace.exists(name)

    def _free(self, name):
        return ast.FuncCall(
            ast.ModuleMember(module_name=defs.C_MODULE_NAME, member="free"),
            args=[ast.Name(name)])

    def _arc(self):
        # Free all vars where refs == 0
        # var some = 2
        # var m = some
        result = []
        scope = self.context.namespace.scope
        for key, value in self.context.namespace.space[scope].items():
            if isinstance(value["expr"], ast.Name):
                self.context.namespace.update(value["expr"].value, {
                    "refs": self.context.namespace.get(value["expr"].value)["refs"] - 1
                })
            if value["refs"] == 0:
                result.append(self._free(key))
        return result

    def _expr(self, expr):
        if isinstance(expr, ast.Name):
            self.context.namespace.update(expr.value, {
                "refs": self.context.namespace.get(expr.value)["refs"] + 1
            })
            return expr
        elif isinstance(expr, ast.FuncCall):
            # TODO.
            pass
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = self._expr(stmt.expr)
        # TODO: add type_ to context.
        self.context.namespace.add_name(name.value, {
            "expr": expr,
            "refs": 0
        })
        return ast.Decl(name, type_, expr)

    def main(self, ast_):
        result = super().main(ast_)
        return result + self._arc()
