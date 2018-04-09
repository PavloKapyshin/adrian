from .utils import A
from . import astlib, errors, defs, env_api, utils


def _struct_func_call_from_struct_call(struct_call):
    return astlib.Callable(
        astlib.CallableT.struct_func, struct_call.name,
        defs.INIT_METHOD, struct_call.args)


def make_mapping(params, decl_args, call_args):
    # TODO: support nested generic types (== if decl_type is GenericType).
    mapping = {}
    for (_, decl_type), call_arg in zip(decl_args, call_args):
        if decl_type in params:
            mapping[decl_type] = infer_type(call_arg)
    return mapping


def _infer_type_from_struct_func_call(struct_func_call):
    parent_info = env_api.type_info(struct_func_call.parent)
    if env_api.is_parameter(parent_info["node_type"]):
        if struct_func_call.name in (defs.INIT_METHOD, defs.COPY_METHOD):
            return struct_func_call.parent
    method_info, parent_info = env_api.method_and_struct_info(
        struct_func_call.parent, struct_func_call.name)
    params = parent_info["params"]
    if not params:
        return method_info["type_"]
    mapping = make_mapping(
        params, method_info["args"], struct_func_call.args)
    if mapping:
        return apply_(mapping, for_=method_info["type_"])
    return method_info["type_"]


def apply_(mapping, for_):
    if for_ in A(astlib.Name):
        found = mapping.get(for_)
        return (found if found else for_)
    elif for_ in A(astlib.GenericType):
        return astlib.GenericType(
            for_.base, [apply_(mapping, param) for param in for_.params])
    return for_


def _infer_type_from_data_member(expr):
    parent_info = env_api.parent_info(expr.parent)
    field_info = env_api.field_info(expr.parent, expr.member)
    mapping = parent_info["mapping"]
    if mapping:
        return apply_(mapping, for_=field_info["type_"])
    return field_info["type_"]


def _infer_type_from_adt_member(expr):
    variable_info = env_api.field_info(expr.parent, expr.member)
    return variable_info["type_"]


def _infer_general_type_from_adt_member(expr):
    variable_info = env_api.variable_info(expr.parent)
    return variable_info["type_"]


def infer_type(expr):
    if expr in A(astlib.Ref):
        return infer_type(expr.expr)
    elif expr in A(astlib.Callable):
        if expr.callabletype == astlib.CallableT.struct:
            return _infer_type_from_struct_func_call(
                _struct_func_call_from_struct_call(expr))
        elif expr.callabletype == astlib.CallableT.struct_func:
            return _infer_type_from_struct_func_call(expr)
        elif expr.callabletype == astlib.CallableT.fun:
            return env_api.fun_info(expr.name)["type_"]
        return astlib.Empty()
    elif expr in A(astlib.Name):
        var_info = env_api.variable_info(expr)
        type_ = var_info["type_"]
        if utils.is_adt(type_):
            return infer_type(var_info["expr"])
        return type_
    elif expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.struct:
            return _infer_type_from_data_member(expr)
        elif expr.datatype == astlib.DataT.adt:
            return _infer_type_from_adt_member(expr)
    elif expr in A(astlib.Empty):
        return astlib.Empty()
    elif expr in A(astlib.Literal):
        return astlib.LiteralType(expr.type_)
    elif expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    errors.cannot_infer_type(expr)


def infer_general_type(expr):
    if expr in A(astlib.Ref):
        return infer_general_type(expr.expr)
    elif expr in A(astlib.Callable):
        if expr.callabletype == astlib.CallableT.struct:
            return _infer_type_from_struct_func_call(
                _struct_func_call_from_struct_call(expr))
        elif expr.callabletype == astlib.CallableT.struct_func:
            return _infer_type_from_struct_func_call(expr)
        elif expr.callabletype == astlib.CallableT.fun:
            return env_api.fun_info(expr.name)["type_"]
        return astlib.Empty()
    elif expr in A(astlib.Name):
        return env_api.variable_info(expr)["type_"]
    elif expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.struct:
            return _infer_type_from_data_member(expr)
        elif expr.datatype == astlib.DataT.adt:
            return _infer_general_type_from_adt_member(expr)
    elif expr in A(astlib.Empty):
        return astlib.Empty()
    elif expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    errors.cannot_infer_type(expr)


def _provide_init_args(type_):
    method_info = env_api.method_info(type_, defs.INIT_METHOD)
    return [infer_expr(arg_type) for _, arg_type in method_info["args"]]


def _infer_expr_from_defined_type(type_):
    return astlib.Callable(
        astlib.CallableT.struct, astlib.Empty(),
        type_, _provide_init_args(type_))


def infer_expr(type_):
    if type_ in A(astlib.LiteralType):
        if type_.type_ == astlib.LiteralT.integer:
            return astlib.Literal(astlib.LiteralT.integer, "0")
    elif type_ in A(astlib.Name):
        return _infer_expr_from_defined_type(type_)
    elif type_ in A(astlib.GenericType):
        return astlib.Empty()
    errors.cannot_infer_expr(type_)
