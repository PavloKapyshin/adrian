"""Framework for creating layers for margolith."""
from . import ast
from . import defs


class Layer:

    def __init__(self, context=ast.Context(
            exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
        self.context = context

    def decl(self):
        """Overrides in subclass."""

    def struct_decl(self):
        """Overrides in subclass."""

    def func_decl(self):
        """Overrides in subclass."""

    def funccall(self):
        """Overrides in subclass."""

    def _make_funcs_dict(self):
        return {
            ast.Decl: self.decl,
            ast.StructDecl: self.struct_decl,
            ast.FuncDecl: self.func_decl,
            ast.FuncCall: self.funccall,
        }

    def main(self, ast_):
        self.funcs = self._make_funcs_dict()
        result = []
        for pair in ast_:
            if isinstance(pair, ast.Pair):
                self.context.line = pair.line
                res = self.funcs[type(pair.stmt)](pair.stmt)
                if isinstance(res, list):
                    for elem in res:
                        result.append(ast.Pair(
                            pair.line, elem))
                else:
                    result.append(ast.Pair(
                        pair.line, res))
            else:
                res = self.funcs[type(pair)](pair)
                if isinstance(res, list):
                    for elem in res:
                        result.append(ast.Pair(
                            self.context.line, elem))
                else:
                    result.append(ast.Pair(
                        self.context.line, res))
        return result