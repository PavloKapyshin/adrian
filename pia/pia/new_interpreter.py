from copy import deepcopy

from . import (
    astlib, layers, env_api, defs, utils, errors, typelib, inference)
from .context import context
from .utils import A


def value_from_ref(ref):
    while ref in A(astlib.Ref):
        ref = ref.expr
    return ref


def value_from_name(name):
    if name in A(astlib.Name):
        return context.env[name]["expr"]
    return name


def transform_node(node, *, registry):
    node_func = registry.get(type(node))
    if node_func is not None:
        return node_func(node)


_dict = {
    defs.OR_METHOD: lambda x, y: x or y,
    defs.AND_METHOD: lambda x, y: x and y,
    defs.GTE_METHOD: lambda x, y: x >= y,
    defs.LTE_METHOD: lambda x, y: x <= y,
    defs.GT_METHOD: lambda x, y: x > y,
    defs.LT_METHOD: lambda x, y: x < y,
    defs.EQ_METHOD: lambda x, y: x == y,
    defs.NEQ_METHOD: lambda x, y: x != y,
    defs.ADD_METHOD: lambda x, y: x + y,
    defs.SUB_METHOD: lambda x, y: x - y,
    defs.MUL_METHOD: lambda x, y: x * y,
    defs.DIV_METHOD: lambda x, y: x / y,
}


def _eval_py_method_2arg(method_name, arg1, arg2):
    method_name = method_name[
        :(defs.MANGLING_PREFIX_LEN+len(defs.U_STRING))]
    return _dict[method_name](arg1, arg2)


class Main(layers.Layer):

    def py_to_adr(self, expr):
        if expr in A(int):
            return astlib.PyTypeCall(
                defs.INT,
                [astlib.Literal(astlib.LiteralT.number, str(expr))])
        elif expr in A(str):
            return astlib.PyTypeCall(
                defs.STR,
                [astlib.Literal(astlib.LiteralT.string, expr)])

    def value_from_data_member(self, struct_member):
        if struct_member in A(astlib.DataMember):
            return self.e_data_member(struct_member)
        return struct_member

    def _get_root_expr(self, parent):
        root_expr = context.env[parent]["expr"]
        if root_expr in A(astlib.Name):
            root_expr = self.e(root_expr)
        return root_expr

    def _for_setting_prefix(self, dest):
        parent = dest.parent
        members = []
        while parent in A(astlib.DataMember):
            members.append(parent.member)
            parent = value_from_ref(parent.parent)
        root_expr = self._get_root_expr(parent)
        return parent, members, root_expr

    def _type_for_setting(self, dest, expr):
        parent, members, root_expr = self._for_setting_prefix(dest)
        for member in reversed(members):
            root_expr = root_expr[member]["expr"]
        if root_expr in A(astlib.DataMember):
            root_expr = self._expr_for_setting(root_expr)
        root_expr[dest.member]["type_"] = inference.infer_type(expr)
        # Applying transformation (building new transformed info).
        info = self._get_root_expr(parent)
        for member in reversed(members):
            info[member]["expr"] = root_expr
            root_expr = info
        return root_expr

    def _expr_for_setting(self, dest, expr):
        parent, members, root_expr = self._for_setting_prefix(dest)
        for member in reversed(members):
            # FIXED here root_expr[member] -> root_expr[member]["expr"]
            root_expr = root_expr[member]["expr"]
        if root_expr in A(astlib.DataMember):
            root_expr = self._expr_for_setting(root_expr)
        root_expr[dest.member]["expr"] = self.e(expr)
        # Applying transformation (building new transformed info).
        info = self._get_root_expr(parent)
        for member in reversed(members):
            info[member]["expr"] = root_expr
            root_expr = info
        return root_expr

    def set_data_member_expr(self, dest, expr):
        root = utils.scroll_to_parent(dest)
        context.env[root]["expr"] = self._expr_for_setting(dest, expr)

    def e_data_member(self, expr):
        parent = expr.parent
        members = [expr.member]
        while parent in A(astlib.DataMember):
            members.append(parent.member)
            parent = value_from_ref(parent.parent)
        root_expr = self._get_root_expr(parent)
        for member in reversed(members):
            # FIXED here root_expr[member] -> root_expr[member]["expr"]
            root_expr = root_expr[member]["expr"]
        return root_expr

    def py_print(self, args):
        """Interpret py#print"""
        for arg in args[:-1]:
            print(self.eval_(arg), end=" ")
        print(self.eval_(args[-1]))

    def py_list_append(self, append_call):
        def _append_validate_dest_and_expr(dest, element):
            dest = value_from_ref(dest)
            expr = self.value_from_data_member(value_from_name(dest))
            element = self.value_from_data_member(
                value_from_name(
                    value_from_ref(element)))
            return dest, expr, element

        dest, expr, element = _append_validate(
            append_call.args[0], append_call.args[1])
        if expr in A(astlib.PyTypeCall):
            expr.args[0].literal.append(element)
            if dest in A(astlib.Name):
                context.env[dest]["expr"] = expr
            else:
                self.set_data_member_expr(dest, expr)
        elif expr in A(astlib.Name):
            context.env[expr]["expr"].args[0].literal.append(
                context.env[element]["expr"])

    def register_args(self, decl_args, args):
        for (name, type_), expr in zip(decl_args, args):
            expr = self.e(expr)
            if name != defs.SELF:
                expr = deepcopy(expr)
            if type_ not in context.env:
                type_ = inference.infer_type(expr)
            general_type = type_
            if utils.is_adt(type_) or utils.is_protocol(type_):
                type_ = inference.infer_type(expr)
            context.env[name] = {
                "node_type": astlib.NodeT.arg,
                "general_type": general_type,
                "type_": type_,
                "mapping": env_api.create_type_mapping(general_type),
                "expr": expr
            }

    def b(self, body):
        reg = Main().get_registry()
        for stmt in body:
            value = transform_node(stmt, registry=reg)
            if value is not None:
                return value

    def _e_ref(self, expr):
        if expr in A(astlib.DataMember, astlib.Name):
            return expr
        return self.e(expr)

    def e(self, expr):
        """Use for interpreting expression."""
        if expr in A(astlib.PyTypeCall, astlib.PyConstant, dict):
            return expr
        elif expr in A(astlib.Alloc):
            return {}
        elif expr in A(astlib.Callable):
            return self.callable(expr)
        elif expr in A(astlib.Ref):
            return self._e_ref(expr.expr)
        elif expr in A(astlib.DataMember):
            return self.e_data_member(expr)
        elif expr in A(astlib.Name):
            return self.e(env_api.variable_info(expr))["expr"]
        elif expr in A(astlib.AdtMember):
            return self.e(expr.member)

    def _eval_py_methods(self, expr):
        if expr.name.endswith(defs.NOT_METHOD):
            return not self.eval_(expr.args[0])
        elif expr.name.endswith(defs.APPEND):
            self.py_list_append(expr)
        return _eval_py_method_2arg(
            expr.name, self.eval_(expr.args[0]), self.eval_(expr.args[1]))

    def _eval_py_type(self, expr):
        if expr.name == defs.INT:
            return int(expr.args[0].literal)
        elif expr.name == defs.STR:
            return expr.args[0].literal
        elif expr.name == defs.LIST:
            return [self.eval_(elem) for elem in expr.args[0].literal]

    def _eval_py_constant(self, expr):
        if expr.name == defs.TRUE:
            return True
        elif expr.name == defs.FALSE:
            return False

    def eval_(self, expr):
        if expr in A(astlib.Name):
            return self.eval_(env_api.variable_info(expr)["expr"])
        elif expr in A(astlib.PyTypeCall):
            return self._eval_py_type(expr)
        elif expr in A(astlib.PyConstant):
            return self._eval_py_constant(expr)
        elif expr in A(astlib.Callable):
            if expr.callabletype == astlib.CallableT.struct_func:
                if expr.parent in A(astlib.PyType):
                    return self._eval_py_methods(expr)
        elif expr in A(astlib.Ref):
            return self.eval_(expr.expr)
        elif expr in A(astlib.DataMember):
            return self.eval_(self.e_data_member(expr))
        elif expr in A(astlib.Is):
            info = env_api.get_info(expr.expr)
            type_ = info["type_"]
            return (typelib.types_are_equal(type_, expr.type_) or
                typelib.is_supertype(expr.type_, of=type_) or
                typelib.is_subtype(expr.type_, of=type_))

    def struct_call(self, stmt):
        struct_info = env_api.type_info(stmt.name)
        init_info = struct_info["methods"][defs.INIT_METHOD]
        +context.env
        self.register_args(init_info["args"], stmt.args)
        return_val = self.b(init_info["body"])
        -context.env
        return return_val

    def func_call(self, stmt):
        func_info = env_api.fun_info(stmt.name)
        +context.env
        self.register_args(func_info["args"], stmt.args)
        return_val = self.b(func_info["body"])
        -context.env
        return return_val

    def struct_func(self, stmt):
        if stmt.parent in A(astlib.PyType):
            new_expr = self.eval_(stmt)
            return self.py_to_adr(new_expr)
        method_info = env_api.method_info(stmt.parent, stmt.name)
        +context.env
        self.register_args(method_info["args"], stmt.args)
        return_val = self.b(method_info["body"])
        -context.env
        return return_val

    def register_assignment(self, stmt):
        if stmt.left in A(astlib.DataMember):
            root = utils.scroll_to_parent(stmt.left.parent)
            context.env[root]["expr"] = self._expr_for_setting(
                stmt.left, stmt.right)
            context.env[root]["type_"] = self._type_for_setting(
                stmt.left, stmt.right)
        else:
            context.env[stmt.left]["expr"] = self.e(stmt.right)
            context.env[stmt.left]["type_"] = inference.infer_type(
                stmt.right)

    def register_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            env_api.register(stmt)
        else:
            general_type = stmt.type_
            type_ = stmt.type_
            # TODO: maybe `var some: a` does not work
            if utils.is_adt(stmt.type_) or utils.is_protocol(stmt.type_):
                type_ = inference.infer_type(stmt.expr)
            context.env[stmt.name] = {
                "node_type": utils.nodetype_from_decl(stmt.decltype),
                "type_": type_,
                "general_type": general_type,
                "mapping": env_api.create_type_mapping(general_type),
                "expr": self.e(stmt.expr)
            }

    @layers.register(astlib.Callable)
    def callable(self, stmt):
        if stmt.callabletype == astlib.CallableT.fun:
            return self.func_call(stmt)
        elif stmt.callabletype == astlib.CallableT.struct:
            return self.struct_call(stmt)
        return self.struct_func(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        self.register_assignment(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        self.register_decl(stmt)

    def if_(self, stmt):
        conditional = self.eval_(stmt.expr)
        if conditional:
            return True, self.b(stmt.body)
        return False, None

    def else_(self, stmt):
        return self.b(stmt.body)

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        ifs = [stmt.if_] + stmt.elifs_
        else_ = stmt.else_
        for if_ in ifs:
            evaluated, value = self.if_(if_)
            if evaluated:
                return value
        if else_ is not None:
            return self.else_(else_)

    @layers.register(astlib.While)
    def while_(self, stmt):
        while self.eval_(stmt.expr):
            value = self.b(stmt.body)
            if value is not None:
                return value

    @layers.register(astlib.Return)
    def return_(self, stmt):
        return self.e(stmt.expr)

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

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        self.register_body(stmt.body)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        self.register_body(stmt.body)
