"""Framework for creating layers for margolith."""
from . import ast
from . import defs


class Layer:

    def __init__(self, context=ast.Context(
            exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
        self.context = context

    def decl(self):
        """Overrides in subclass."""

    def data(self):
        """Overrides in subclass."""

    def func(self):
        """Overrides in subclass."""

    def _make_funcs_dict(self):
        return {
            ast.Decl: self.decl,
            ast.Data: self.data,
            ast.Func: self.func
        }

    def main(self, ast_):
        self.funcs = self._make_funcs_dict()
        result = []
        for pair in ast_:
            self.context.line = pair.line
            res = self.funcs[type(pair.stmt)](pair.stmt)
            if isinstance(res, list):
                for elem in res:
                    result.append(ast.Pair(
                        pair.line, elem))
            else:
                result.append(ast.Pair(
                    pair.line, res))
        return result