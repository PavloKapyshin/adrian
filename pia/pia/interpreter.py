from copy import deepcopy

from . import astlib, layers, env_api, inference, errors, typelib, defs, utils
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


def transform_node(node, *, registry):
    node_func = registry.get(type(node))
    if node_func is not None:
        return node_func(node)


def _py_method_2arg(method_name, arg1, arg2):
    return _dict[method_name](arg1, arg2)


def py_to_adr(expr):
    if expr in A(int):
        return astlib.PyTypeCall(
            defs.INT, [astlib.Literal(astlib.LiteralT.number, str(expr))])
    elif expr in A(str):
        return astlib.PyTypeCall(
            defs.STR, [astlib.Literal(astlib.LiteralT.string, expr)])
    elif expr in A(list):
        return astlib.PyTypeCall(
            defs.LIST,
            [astlib.Literal(
                astlib.LiteralT.vector, [py_to_adr(elem) for elem in expr])])


class Main(layers.Layer):

    def b(self, body):
        reg = Main().get_registry()
        for stmt in body:
            value = transform_node(stmt, registry=reg)
            if value is not None:
                return value

    def value_from_struct_field(self, struct_field):
        if struct_field in A(astlib.StructField):
            return self.e_struct_field(struct_field)
        return struct_field

    def py_list_append(self, append_call):
        def _append_validate(dest, element):
            dest = value_from_ref(dest)
            expr = self.value_from_struct_field(value_from_name(dest))
            element = self.value_from_struct_field(
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
                self.update_(
                    utils.scroll_to_parent(dest), dest, expr)
        elif expr in A(astlib.Name):
            context.env[expr]["expr"].args[0].literal.append(
                context.env[element]["expr"])

    def adr_to_py_py_method(self, expr):
        if expr.name == defs.NOT_METHOD:
            return not self.adr_to_py(expr.args[0])
        elif expr.name == defs.APPEND:
            self.py_list_append(expr)
        else:
            return _py_method_2arg(
                expr.name, self.adr_to_py(expr.args[0]),
                self.adr_to_py(expr.args[1]))

    def get_field_expr(self, expr):
        struct = expr.struct
        fields = [expr.field]
        while struct in A(astlib.StructField):
            fields.append(struct.field)
            struct = struct.struct
        if struct not in A(astlib.Name):
            root_expr = self.e(struct)
        else:
            root_expr = context.env[struct]["expr"].value

        if root_expr in A(astlib.Name):
            root_expr = self.e(root_expr)
        if root_expr in A(astlib.StructField):
            root_expr = self.get_field_expr(root_expr)
        for field in reversed(fields):
            root_expr = root_expr[field].value
        return root_expr

    def adr_to_py(self, expr):
        if expr in A(astlib.Name):
            return self.adr_to_py(env_api.variable_info(expr)["expr"])
        elif expr in A(astlib.PyTypeCall):
            if expr.name == defs.INT:
                return int(expr.args[0].literal)
            elif expr.name == defs.STR:
                return expr.args[0].literal
            elif expr.name == defs.LIST:
                return [self.adr_to_py(elem) for elem in expr.args[0].literal]
        elif expr in A(astlib.PyConstant):
            if expr.name == defs.TRUE:
                return True
            return False
        elif expr in A(astlib.Ref):
            return self.adr_to_py(expr.expr)
        elif expr in A(astlib.StructField):
            return self.adr_to_py(self.get_field_expr(expr))
        elif expr in A(astlib.Is):
            info = env_api.get_info(expr.expr)
            spec_type = info["spec_type"]
            return (typelib.types_are_equal(spec_type, expr.type_) or
                typelib.is_supertype(expr.type_, of=spec_type) or
                typelib.is_subtype(expr.type_, of=spec_type))
        elif expr in A(astlib.FuncCall):
            return self.adr_to_py(self.e(expr))
        elif expr in A(astlib.StructFuncCall):
            if expr.parent in A(astlib.PyType):
                return self.adr_to_py_py_method(expr)
            return self.adr_to_py(self.e(expr))

    def e_struct_field(self, expr):
        struct = expr.struct
        members = [expr.field]
        while struct in A(astlib.StructField):
            members.append(struct.field)
            struct = struct.struct
        root_expr = self.get_root_expr(struct).value
        for member in reversed(members):
            root_expr = root_expr[member].value
        return root_expr

    def e_ref(self, expr):
        if expr in A(astlib.StructField, astlib.Name):
            return expr
        return self.e(expr)

    def e(self, expr):
        if expr in A(astlib.StructValue):
            return astlib.StructValue(expr.type_, self.e(expr.value))
        elif expr in A(dict):
            return {key: self.e(val) for key, val in expr.items()}
        elif expr in A(astlib.Alloc):
            return astlib.StructValue(expr.type_, {})
        elif expr in A(astlib.FuncCall):
            return self.func_call(expr)
        elif expr in A(astlib.StructFuncCall):
            return self.struct_func_call(expr)
        elif expr in A(astlib.Ref):
            return self.e_ref(expr.expr)
        elif expr in A(astlib.Name):
            return self.e(env_api.variable_info(expr)["expr"])
        elif expr in A(astlib.StructField):
            return self.e_struct_field(expr)
        elif expr in A(astlib.AdtMember):
            return self.e(expr.member)
        return expr

    def init_member_info(self, expr=None, type_=None):
        return astlib.StructValue(
            type_ or inference.infer_spec_type(expr), expr)

    def get_root_expr(self, parent):
        root_expr = context.env[parent]["expr"]
        if root_expr in A(astlib.Name):
            return self.e(root_expr)
        return root_expr

    def for_setting_prefix(self, dest):
        parent = dest.struct
        members = []
        while parent in A(astlib.StructField):
            members.append(parent.field)
            parent = value_from_ref(parent.struct)
        root_expr = self.get_root_expr(parent)
        return parent, members, root_expr

    def prepare_for_setting(self, dest, expr):
        parent, members, root_expr = self.for_setting_prefix(dest)
        for member in reversed(members):
            root_expr = root_expr[member].value
        if root_expr in A(astlib.StructField):
            root_expr = self.prepare_expr_for_setting(root_expr)
        root_expr.value[dest.field] = self.init_member_info(
            expr=self.e(expr), type_=inference.infer_spec_type(expr))
        info = self.get_root_expr(parent)
        for member in reversed(members):
            info[member].value = root_expr
            root_expr = info
        return root_expr

    def update_(self, root, request, expr):
        context.env[root]["expr"] = self.prepare_for_setting(request, expr)

    def register_args(self, decl_args, args):
        for (name, type_), expr_ in zip(decl_args, args):
            expr = self.e(expr_)
            if name != defs.SELF:
                expr = deepcopy(expr)
            spec_type = inference.infer_spec_type(expr)
            context.env[name] = {
                "node_type": astlib.NodeT.arg,
                "type_": type_,
                "spec_type": spec_type,
                "expr": expr
            }

    def register_assignment(self, stmt):
        if stmt.left in A(astlib.StructField):
            root = utils.scroll_to_parent(stmt.left.struct)
            self.update_(root, stmt.left, self.e(stmt.right))
        else:
            context.env[stmt.left]["expr"] = self.e(stmt.right)
            context.env[stmt.left]["spec_type"] = inference.infer_spec_type(
                stmt.right)

    def register_decl(self, stmt):
        type_ = stmt.type_
        expr = self.e(stmt.expr)
        spec_type = inference.infer_spec_type(expr)
        context.env[stmt.name] = {
            "node_type": (astlib.NodeT.var
                if stmt in A(astlib.VarDecl) else astlib.NodeT.let),
            "type_": type_,
            "spec_type": spec_type,
            "expr": expr
        }

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        func_info = env_api.fun_info(stmt.name)
        +context.env
        self.register_args(func_info["args"], stmt.args)
        return_val = self.b(func_info["body"])
        -context.env
        return return_val

    @layers.register(astlib.StructFuncCall)
    def struct_func_call(self, stmt):
        if stmt.parent in A(astlib.PyType):
            return py_to_adr(self.adr_to_py(stmt))
        if utils.is_protocol(stmt.parent):
            stmt.parent = inference.infer_spec_type(stmt.args[0])
        method_info = env_api.method_info(stmt.parent, stmt.name)
        +context.env
        self.register_args(method_info["args"], stmt.args)
        return_val = self.b(method_info["body"])
        -context.env
        return return_val

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        self.register_assignment(stmt)

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        self.register_decl(stmt)

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        self.register_decl(stmt)

    @layers.register(astlib.FieldDecl)
    def field_decl(self, stmt):
        env_api.register(stmt)

    def if_(self, stmt):
        conditional = self.adr_to_py(stmt.expr)
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
            is_evaluated, value = self.if_(if_)
            if is_evaluated:
                return value
        if else_ is not None:
            return self.else_(else_)

    @layers.register(astlib.While)
    def while_(self, stmt):
        while self.adr_to_py(stmt.expr):
            value = self.b(stmt.body)
            if value is not None:
                return value

    @layers.register(astlib.Return)
    def return_(self, stmt):
        return self.e(stmt.expr)

    @layers.register(astlib.PyFuncCall)
    def py_func_call(self, stmt):
        if stmt.name == defs.PRINT:
            for arg in stmt.args[:-1]:
                print(self.adr_to_py(arg), end=" ")
            print(self.adr_to_py(stmt.args[-1]))
        else:
            errors.later(errors.Version.v0m5.value)

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        env_api.register(stmt)

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        env_api.register(stmt)

    def register_body(self, body):
        for stmt in body:
            env_api.register(stmt)

    def data_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        self.register_body(stmt.body)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        self.data_decl(stmt)

    @layers.register(astlib.AdtDecl)
    def adt_decl(self, stmt):
        self.data_decl(stmt)

    @layers.register(astlib.ProtocolDecl)
    def protocol_decl(self, stmt):
        self.data_decl(stmt)
