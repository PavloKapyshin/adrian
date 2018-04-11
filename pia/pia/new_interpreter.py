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

    def value_from_data_member(self, struct_member):
        if struct_member in A(astlib.DataMember):
            return self.e_data_member(struct_member)
        return struct_member

    def _get_root_expr(self, parent):
        root_expr = context.env[parent]["expr"]
        if root_expr in A(astlib.Name):
            root_expr = self.e(root_expr)
        return root_expr

    def _prepare_expr_for_setting(self, dest, expr):
        parent = dest.parent
        members = []
        while parent in A(astlib.DataMember):
            members.append(parent.member)
            parent = value_from_ref(parent.parent)

        root_expr = self._get_root_expr(parent)
        for member in reversed(members):
            # FIXED here root_expr[member] -> root_expr[member]["expr"]
            root_expr = root_expr[member]["expr"]


    def set_data_member_expr(self, dest, expr):
        root = utils.scroll_to_parent(dest)
        context.env[root]["expr"] = self._prepare_expr_for_setting(dest, expr)

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

    def py_to_adr(self, expr):
        if expr in A(int):
            return astlib.PyTypeCall(
                defs.INT,
                [astlib.Literal(astlib.LiteralT.number, str(expr))])
        elif expr in A(str):
            return astlib.PyTypeCall(
                defs.STR,
                [astlib.Literal(astlib.LiteralT.string, expr)])

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
