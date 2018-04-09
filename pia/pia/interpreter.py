from . import astlib, layers, env_api, defs
from .context import context
from .utils import A


class Main(layers.Layer):

    def eval_e(self, expr):
        if expr in A(astlib.Name):
            info = env_api.variable_info(expr)
            current_expr = info["expr"]
            return self.eval_e(current_expr)
        elif expr in A(astlib.PyTypeCall):
            if expr.name == defs.INT:
                return int(expr.args[0].literal)
            elif expr.name == defs.STR:
                return expr.args[0].literal

    def py_print(self, args):
        """Interpret py#print"""
        for arg in args:
            print(self.eval_e(arg))

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        env_api.register(stmt)

    @layers.register(astlib.PyFuncCall)
    def py_func_call(self, stmt):
        if stmt.name == defs.PRINT:
            self.py_print(stmt.args)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
