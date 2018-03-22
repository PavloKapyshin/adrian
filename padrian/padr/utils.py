import itertools

from .context import context
from . import astlib


class A:
    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)


def _check_found_info(info, request, node_type_checker):
    if not info:
        errors.unknown_name(request)
    node_type = info["node_type"]
    if not node_type_checker(node_type):
        errors.wrong_node_type(request, node_type)


def _get_info_maker(node_type_checker):
    def helper(request):
        info = context.env[request]
        _check_found_info(info, request, node_type_checker)
        return info
    return helper


def _is_of_nodetype_maker(*node_types):
    def helper(node_type):
        return node_type in node_types
    return helper


# Low-level checkings
is_var = _is_of_nodetype_maker(astlib.NodeT.var)
is_let = _is_of_nodetype_maker(astlib.NodeT.let)
is_fun = _is_of_nodetype_maker(astlib.NodeT.fun)
is_protocol = _is_of_nodetype_maker(astlib.NodeT.protocol)
is_adt = _is_of_nodetype_maker(astlib.NodeT.adt)
is_struct = _is_of_nodetype_maker(astlib.NodeT.struct)

# High-level checkings
is_variable = _is_of_nodetype_maker(
    astlib.NodeT.let, astlib.NodeT.var, astlib.NodeT.arg)
is_type = _is_of_nodetype_maker(
    astlib.NodeT.struct, astlib.NodeT.commont,
    astlib.NodeT.adt, astlib.NodeT.protocol)
is_real_type = _is_of_nodetype_maker(
    astlib.NodeT.struct, astlib.NodeT.adt,
    astlib.NodeT.protocol)
is_function = _is_of_nodetype_maker(astlib.NodeT.fun)

get_type_info = _get_info_maker(is_type)
get_variable_info = _get_info_maker(is_variable)
get_function_info = _get_info_maker(is_function)

def get_parent_info(expr):
    if expr in A(astlib.Name):
        return get_variable_info(expr)
    return get_parent_info(expr.parent)

def get_node_type(request):
    info = context.env[request]
    if not info:
        errors.unknown_name(request)
    return info["node_type"]

def get_method_and_parent_infos(parent, method_name):
    parent_info = get_type_info(parent)
    methods = parent_info["methods"]
    method_info = methods.get(method_name)
    if not method_info:
        errors.no_such_method(parent, method_name)
    return method_info, parent_info

def get_method_info(parent, method_name):
    return get_method_and_parent_infos(parent, method_name)[0]

def get_field_info(parent, field_name):
    if parent in A(astlib.Name):
        parent_info = get_parent_info(parent)
    else:
        parent_info = get_field_info(parent.parent, parent.member)
    parent_type = parent_info["type_"]
    parent_type_info = get_type_info(parent_type)
    fields = parent_type_info["fields"]
    field_info = fields.get(field_name)
    if not field_info:
        errors.no_such_field(parent, parent_type, field_name)
    return field_info


def partition(pred, iterable):
    t1, t2 = itertools.tee(iterable)
    return list(filter(pred, t1)), list(itertools.filterfalse(pred, t2))


_is_field_stmt = lambda stmt: (
    stmt in A(astlib.Decl) and stmt.decltype == astlib.DeclT.field)


def split_body(body):
    return partition(_is_field_stmt, body)


def only_fields(body):
    return list(filter(_is_field_stmt, body))


# TODO:
def declt_to_nodet(declt):
    nodet = astlib.NodeT.adt
    if declt == astlib.DeclT.struct:
        nodet = astlib.NodeT.struct
    elif declt == astlib.DeclT.protocol:
        nodet = astlib.NodeT.protocol
    elif declt == astlib.DeclT.var:
        nodet = astlib.NodeT.var
    elif declt == astlib.DeclT.let:
        nodet = astlib.NodeT.let
    return nodet


def add_dicts(dict1, dict2):
    dict_ = dict1
    for key, value in dict2.items():
        dict_[key] = value
    return dict_


def any_common(l1, l2):
    for elem in l1:
        if elem in l2:
            return True
    return False


def get_mapping(type_):
    if type_ in A(astlib.ParamedType):
        mapping = {}
        struct_info = context.env[type_.type_]
        params = struct_info["params"]
        i = 0
        for param in params:
            if param not in mapping:
                mapping[param] = type_.params[i]
            i += 1
        return mapping
    return {}


def register_var_or_let(name, decltype, type_):
    context.env[name] = {
        "node_type": declt_to_nodet(decltype),
        "type_": type_,
        "mapping": get_mapping(type_)
    }


def register_field(name, type_):
    context.env.update(context.parent, {
        "fields": add_dicts(
            context.env[context.parent]["fields"], {
            name: {
                "type_": type_
            }
        })
    })


def register_data_decl(name, decltype, params):
    context.env[name] = {
        "node_type": declt_to_nodet(decltype),
        "params": params,
        "methods": {},
        "fields": {}
    }


def register_func(name, rettype, args):
    context.env[name] = {
        "node_type": astlib.NodeT.fun,
        "type_": rettype,
        "args": args
    }


def register_func_as_child(parent, name, rettype, args):
    context.env.update(parent, {
        "methods": add_dicts(
            context.env[parent]["methods"], {
            name: {
                "type_": rettype,
                "args": args
            }
        })
    })


def register_args(args):
    for name, type_ in args:
        context.env[name] = {
            "node_type": astlib.NodeT.arg,
            "type_": type_
        }


def register_params(params):
    for param in params:
        context.env[param] = {
            "node_type": astlib.NodeT.commont
        }
