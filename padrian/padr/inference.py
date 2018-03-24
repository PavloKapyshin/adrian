from .context import A, context
from . import astlib, errors, defs


def _struct_func_call_from_struct_call(struct_call):
    return astlib.Callable(
        astlib.CallableT.struct_func, struct_call.name,
        defs.INIT_METHOD, struct_call.args)


def make_mapping(params, decl_args, call_args):
    # TODO: support nested generic types (== if decl_type is ParamedType).
    mapping = {}
    for (_, decl_type), call_arg in zip(decl_args, call_args):
        if decl_type in params:
            mapping[decl_type] = infer_type(call_arg)
    return mapping


def _infer_type_from_struct_func_call(struct_func_call):
    method_info, parent_info = context.env.get_method_and_parent_infos(
        struct_func_call.parent, struct_func_call.name)
    params = parent_info["params"]
    if not params:
        return method_info["type_"]
    mapping = make_mapping(
        params, method_info["args"], struct_func_call.args)
    if mapping:
        return apply_(mapping, for_=method_info["type_"])
    print("HAy, badd, infer...")
    # return astlib.ParamedType(
    #     method_info["type_"], [mapping[param] for param in params])


def apply_(mapping, for_):
    if for_ in A(astlib.Name):
        found = mapping.get(for_)
        return (found if found else for_)
    elif for_ in A(astlib.ParamedType):
        return astlib.ParamedType(
            for_.base, [apply_(mapping, param) for param in for_.params])
    errors.not_implemented("stmt {} is unknown".format(for_), func=apply_)


def _infer_type_from_data_member(expr):
    parent_info = context.env.get_parent_info(expr.parent)
    field_info = context.env.get_field_info(expr.parent, expr.member)
    mapping = parent_info.get("mapping")
    if mapping:
        return apply_(mapping, for_=field_info["type_"])
    return field_info["type_"]


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
            return context.env.get_function_info(expr.name)["type_"]
        return astlib.Empty()
    elif expr in A(astlib.Name):
        var_info = context.env.get_variable_info(expr)
        type_ = var_info["type_"]
        if context.env.is_adt(context.env.get_type_info(type_)["node_type"]):
            return infer_type(var_info["expr"])
        return type_
    elif expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.struct:
            return _infer_type_from_data_member(expr)
    errors.infer_type(expr)


def _provide_init_args(type_):
    method_info = context.env.get_method_info(type_, defs.INIT_METHOD)
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
    elif type_ in A(astlib.DataMember):
        if type_.datatype == astlib.DataT.module:
            return _infer_expr_from_defined_type(type_)
    errors.infer_expr(type_)
