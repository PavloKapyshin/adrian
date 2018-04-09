from . import astlib, layers, env_api, defs
from .context import context
from .utils import A


def transform_node(node, *, registry):
    node_func = registry.get(type(node))
    if node_func:
        return node_func(node)


class Main(layers.Layer):

    def eval_b(self, body):
        reg = Main().get_registry()
        for stmt in body:
            value = transform_node(stmt, registry=reg)
            if value is not None:
                return value

    def eval_e(self, expr):
        if expr in A(astlib.Name, astlib.DataMember):
            return expr

    def register_args_for_eval(self, decl_args, args):
        for (arg_name, arg_type), arg_expr in zip(decl_args, args):
            context.env[arg_name] = {
                "node_type": astlib.NodeT.arg,
                "type_": arg_type,
                "expr": arg_expr
            }

    def eval_for_python(self, expr):
        if expr in A(astlib.Name):
            info = env_api.variable_info(expr)
            current_expr = info["expr"]
            return self.eval_for_python(current_expr)
        elif expr in A(astlib.PyTypeCall):
            if expr.name == defs.INT:
                return int(expr.args[0].literal)
            elif expr.name == defs.STR:
                return expr.args[0].literal
        elif expr in A(astlib.DataMember):
            return env_api.field_info

    def py_print(self, args):
        """Interpret py#print"""
        for arg in args:
            print(self.eval_for_python(arg))

    def register_expr(self, name, expr):
        if expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.struct:
                env_expr = self.struct_call(expr)
                context.env[name]["expr"] = expr

    def struct_call(self, stmt):
        struct_info = env_api.type_info(stmt.name)
        init_info = struct_info["methods"][defs.INIT_METHOD]
        +context.env
        self.register_args_for_eval(init_info["args"], stmt.args)
        return_val = self.eval_b(init_info["body"])
        -context.env
        return return_val

    def func_call(self, stmt):
        func_info = env_api.fun_info(stmt.name)
        +context.env
        self.register_args_for_eval(func_info["args"], stmt.args)
        return_val = self.eval_b(func_info["body"])
        -context.env
        return return_val

    @layers.register(astlib.Callable)
    def callable(self, stmt):
        if stmt.callabletype == astlib.CallableT.fun:
            return self.func_call(stmt)
        elif stmt.callabletype == astlib.CallableT.struct:
            return self.struct_call(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        env_api.register(stmt)
        self.register_expr(stmt.left, stmt.right)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        env_api.register(stmt)
        self.register_expr(stmt.name, stmt.expr)

    def eval_if(self, stmt):
        conditional = self.eval_for_python(stmt.expr)
        if conditional:
            value = self.eval_b(stmt.body)
            if value is not None:
                return True, value
            return True, None
        return False, None

    def eval_else(self, stmt):
        value = self.eval_b(stmt.body)
        if value is not None:
            return value

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        ifs = [stmt.if_] + stmt.elifs_
        else_case = stmt.else_
        for if_ in ifs:
            evaluated, value = self.eval_if(if_)
            if evaluated:
                return value
        if else_case is not None:
            return self.eval_else(else_case)

    @layers.register(astlib.While)
    def while_(self, stmt):
        while self.eval_for_python(stmt.expr):
            value = self.eval_b(stmt.body)
            if value is not None:
                return value

    @layers.register(astlib.Return)
    def return_(self, stmt):
        return self.eval_e(stmt.expr)

    @layers.register(astlib.PyFuncCall)
    def py_func_call(self, stmt):
        if stmt.name == defs.PRINT:
            self.py_print(stmt.args)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        env_api.register(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
