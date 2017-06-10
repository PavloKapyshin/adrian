"""Framework for creating layers for margolith."""
from . import ast, defs


class Layer:

    def __init__(self, context=None):
        self.namespace = None
        self.funcspace = None
        self.typespace = None
        if context:
            self.namespace = context.namespace
            self.funcspace = context.funcspace
            self.typespace = context.typespace

    def decl(self, stmt):
        """Overrides in subclass."""

    def struct_decl(self, stmt):
        """Overrides in subclass."""

    def func_decl(self, stmt):
        """Overrides in subclass."""

    def funccall(self, stmt):
        """Overrides in subclass."""

    def _make_funcs_dict(self):
        return {
            ast.Decl: self.decl,
            ast.StructDecl: self.struct_decl,
            ast.FuncDecl: self.func_decl,
            ast.FuncCall: self.funccall,
        }

    def main(self, ast_, *, exit_on_error=True):
        self.exit_on_error = exit_on_error
        funcs = self._make_funcs_dict()
        result = []
        for pair in ast_:
            self.position = ast.Position(pair.line, pair.column)
            result.append(
                ast.Pair(pair.line, pair.column,
                    funcs[type(pair.stmt)](pair.stmt)))
        return result
        # self.funcs = self._make_funcs_dict()
        # result = []
        # for pair in ast_:
        #     if isinstance(pair, ast.Pair):
        #         self.position = ast.Position(pair.line, pair.column)
        #         res = self.funcs[type(pair.stmt)](pair.stmt)
        #         if isinstance(res, list):
        #             for elem in res:
        #                 result.append(ast.Pair(
        #                     pair.line, pair.column, elem))
        #         else:
        #             result.append(ast.Pair(
        #                 pair.line, pair.column, res))
        #     else:
        #         res = self.funcs[type(pair)](pair)
        #         if isinstance(res, list):
        #             for elem in res:
        #                 result.append(ast.Pair(
        #                     self.position.line, self.position.column,
        #                     elem))
        #         else:
        #             result.append(ast.Pair(
        #                 self.position.line, self.position.column, res))
        # return result