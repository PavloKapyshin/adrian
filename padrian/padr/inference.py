from . import astlib, errors, defs
from .utils import A
from .context import context


def check_found_info(info, request, node_type_checker):
    if not info:
        errors.unknown_name(request)
    node_type = info["node_type"]
    if not node_type_checker(node_type):
        errors.wrong_node_type(request, node_type)


def _get_info_maker(node_type_checker):
    def helper(request):
        info = context.env[request]
        check_found_info(info, request, node_type_checker)
        return info
    return helper


def _is_of_nodetype_maker(*node_types):
    def helper(node_type):
        return node_type in node_types
    return helper


is_variable = _is_of_nodetype_maker(
    astlib.NodeT.let, astlib.NodeT.var, astlib.NodeT.arg)
is_type = _is_of_nodetype_maker(astlib.NodeT.struct)
is_function = _is_of_nodetype_maker(astlib.NodeT.fun)

get_type_info = _get_info_maker(is_type)
get_variable_info = _get_info_maker(is_variable)
get_function_info = _get_info_maker(is_function)


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
    parent_info = get_type_info(struct_func_call.parent)
    methods = parent_info["methods"]
    if struct_func_call.name not in methods:
        errors.no_such_method(struct_func_call.parent, struct_func_call.name)
    method = methods[struct_func_call.name]
    params = parent_info["params"]
    if not params:
        return method["type_"]
    mapping = make_mapping(params, method["args"], struct_func_call.args)
    return astlib.ParamedType(
        method["type_"], [mapping[param] for param in params])


def get_parent_info(expr):
    if expr in A(astlib.Name):
        return get_variable_info(expr)
    return get_parent_info(expr.parent)


def apply_(mapping, for_):
    if for_ in A(astlib.Name):
        found = mapping.get(for_)
        return (found if found else for_)
    elif for_ in A(astlib.ParamedType):
        return astlib.ParamedType(
            for_.base, [apply_(mapping, param) for param in for_.params])
    errors.not_implemented("stmt {} is unknown".format(for_), func=apply_)


def _infer_type_from_data_member(expr):
    parent_info = get_parent_info(expr.parent)
    parent_type = parent_info["type_"]
    parent_type_info = get_type_info(parent_type)
    parent_fields = parent_type_info["fields"]
    if expr.member not in parent_fields:
        errors.no_such_field(
            parent=expr.parent, parent_type=parent_type,
            field=expr.member)
    member_info = parent_fields[expr.member]
    mapping = parent_info.get("mapping")
    if mapping:
        return apply_(mapping, for_=member_info["type_"])
    return member_info["type_"]


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
            return get_function_info(expr.name)["type_"]
        return astlib.Empty()
    elif expr in A(astlib.Name):
        return get_variable_info(expr)["type_"]
    elif expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.struct:
            return _infer_type_from_data_member(expr)
    errors.infer_type(expr)


def _provide_init_args(type_):
    info = get_type_info(type_)
    methods = info["methods"]
    method_info = methods.get(defs.INIT_METHOD)
    if not method_info:
        errors.no_such_method(type_, defs.INIT_METHOD)
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
