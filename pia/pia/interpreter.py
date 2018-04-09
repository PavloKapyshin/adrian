from copy import deepcopy

from . import astlib, layers, env_api, defs, utils, errors
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

    def _eval_e_ref(self, expr):
        if expr in A(
                astlib.DataMember, astlib.PyTypeCall,
                astlib.PyConstant, astlib.Name):
            return expr
        elif expr in A(astlib.Ref):
            return self._eval_e_ref(expr.expr)
        elif expr in A(astlib.Callable):
            return self.callable(expr)
        elif expr in A(astlib.Alloc):
            return {}
        elif expr in A(dict):
            return expr

    def eval_e(self, expr):
        if expr in A(
                astlib.DataMember, astlib.PyTypeCall,
                astlib.PyConstant):
            return expr
        elif expr in A(astlib.Name):
            return self.eval_e(env_api.variable_info(expr)["expr"])
        elif expr in A(astlib.Ref):
            return self._eval_e_ref(expr.expr)
        elif expr in A(astlib.Callable):
            return self.callable(expr)
        elif expr in A(astlib.Alloc):
            return {}
        elif expr in A(dict):
            return expr

    def register_args_for_eval(self, decl_args, args):
        for (arg_name, arg_type), arg_expr in zip(decl_args, args):
            context.env[arg_name] = {
                "node_type": astlib.NodeT.arg,
                "type_": arg_type,
                "expr": deepcopy(self.eval_e(arg_expr))
            }

    def python_to_adrian(self, expr):
        if expr in A(int):
            return astlib.PyTypeCall(
                defs.INT,
                [astlib.Literal(astlib.LiteralT.number, str(expr))])

    def eval_for_python_std_method(self, expr):
        if expr.name.endswith(defs.NOT_METHOD):
            return not self.eval_for_python(expr.args[0])
        elif expr.name.endswith(defs.OR_METHOD):
            return (
                self.eval_for_python(expr.args[0]) or
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.AND_METHOD):
            return (
                self.eval_for_python(expr.args[0]) and
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.GTE_METHOD):
            return (
                self.eval_for_python(expr.args[0]) >=
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.LTE_METHOD):
            return (
                self.eval_for_python(expr.args[0]) <=
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.GT_METHOD):
            return (
                self.eval_for_python(expr.args[0]) >
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.LT_METHOD):
            return (
                self.eval_for_python(expr.args[0]) <
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.EQ_METHOD):
            return (
                self.eval_for_python(expr.args[0]) ==
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.NEQ_METHOD):
            return (
                self.eval_for_python(expr.args[0]) !=
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.ADD_METHOD):
            return (
                self.eval_for_python(expr.args[0]) +
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.SUB_METHOD):
            return (
                self.eval_for_python(expr.args[0]) -
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.MUL_METHOD):
            return (
                self.eval_for_python(expr.args[0]) *
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.DIV_METHOD):
            return (
                self.eval_for_python(expr.args[0]) /
                self.eval_for_python(expr.args[1]))
        elif expr.name.endswith(defs.APPEND):
            self.py_list_append(expr)

    def py_list_append(self, append_call):
        dest = append_call.args[0]
        element = append_call.args[1]
        while dest in A(astlib.Ref):
            dest = dest.expr
        if dest in A(astlib.Name):
            expr = context.env[dest]["expr"]
            if expr in A(astlib.PyTypeCall):
                if element in A(astlib.Ref):
                    element = element.expr
                elif element in A(astlib.Name):
                    element = context.env[element]["expr"]
                expr.args[0].literal.append(element)
                context.env[dest]["expr"] = expr
            elif expr in A(astlib.Name):
                dest = expr
                context.env[dest]["expr"].args[0].literal.append(
                    context.env[element]["expr"])

    def eval_for_python(self, expr):
        def _get_info(info):
            parent = info.parent
            members = [info.member]
            while parent in A(astlib.DataMember):
                members.append(parent.member)
                parent = parent.parent
            if parent in A(astlib.Ref):
                parent = parent.expr
            info = context.env[parent]["expr"]
            if info in A(astlib.Name):
                info = self.eval_e(info)
            if info in A(astlib.DataMember):
                info = _get_info(info)
            for member in reversed(members):
                info = info[member]
            return info

        if expr in A(astlib.Name):
            info = env_api.variable_info(expr)
            return self.eval_for_python(info["expr"])
        elif expr in A(astlib.PyTypeCall):
            if expr.name == defs.INT:
                return int(expr.args[0].literal)
            elif expr.name == defs.STR:
                return expr.args[0].literal
            elif expr.name == defs.LIST:
                return [
                    self.eval_for_python(l)
                    for l in expr.args[0].literal]
        elif expr in A(astlib.PyConstant):
            if expr.name == defs.TRUE:
                return True
            elif expr.name == defs.FALSE:
                return False
        elif expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.struct_func:
                if expr.parent in A(astlib.PyType):
                    return self.eval_for_python_std_method(expr)
        elif expr in A(astlib.Ref):
            return self.eval_for_python(expr.expr)
        elif expr in A(astlib.DataMember):
            return self.eval_for_python(_get_info(expr))


    def py_print(self, args):
        """Interpret py#print"""
        for arg in args[:-1]:
            print(self.eval_for_python(arg), end=" ")
        print(self.eval_for_python(args[-1]))

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

    def struct_func(self, stmt):
        if stmt.parent in A(astlib.PyType):
            new_expr = self.eval_for_python(stmt)
            return self.python_to_adrian(new_expr)
        method_info = env_api.method_info(stmt.parent, stmt.name)
        +context.env
        self.register_args_for_eval(method_info["args"], stmt.args)
        return_val = self.eval_b(method_info["body"])
        -context.env
        return return_val

    def register(self, stmt):
        if stmt in A(astlib.Assignment):
            if stmt.left in A(astlib.DataMember):
                def _set_info(info):
                    parent = info.parent
                    members = []
                    while parent in A(astlib.DataMember):
                        members.append(parent.member)
                        parent = parent.parent
                    if parent in A(astlib.Ref):
                        parent = parent.expr
                    expr = context.env[parent]["expr"]
                    info = context.env[parent]["expr"]
                    if expr in A(astlib.Name):
                        expr = self.eval_e(expr)
                        info = self.eval_e(info)
                    for member in reversed(members):
                        expr = expr[member]
                    if expr in A(astlib.DataMember):
                        expr = _set_info(expr)
                    expr[stmt.left.member] = self.eval_e(stmt.right)
                    for member in reversed(members):
                        info[member] = expr
                        expr = info
                    return expr
                expr = _set_info(stmt.left)
                context.env[utils.scroll_to_parent(stmt.left.parent)]["expr"] = expr
            else:
                expr = self.eval_e(stmt.right)
                context.env[stmt.left]["expr"] = expr
        elif stmt in A(astlib.Decl):
            if stmt.decltype == astlib.DeclT.field:
                env_api.register(stmt)
            else:
                context.env[stmt.name] = {
                    "node_type": utils.nodetype_from_decl(stmt.decltype),
                    "type_": stmt.type_,
                    "expr": self.eval_e(stmt.expr)
                }

    @layers.register(astlib.Callable)
    def callable(self, stmt):
        if stmt.callabletype == astlib.CallableT.fun:
            return self.func_call(stmt)
        elif stmt.callabletype == astlib.CallableT.struct:
            return self.struct_call(stmt)
        elif stmt.callabletype == astlib.CallableT.struct_func:
            return self.struct_func(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        self.register(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        self.register(stmt)

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
        elif stmt.name == defs.LENGTH:
            errors.later(errors.Version.v0m5.value)
        else:
            errors.no_such_module_member("py", stmt.name)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        env_api.register(stmt)

    def register_body(self, body):
        for stmt in body:
            env_api.register(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        self.register_body(stmt.body)
