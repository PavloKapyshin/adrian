from .context import context
from . import astlib


class A:
    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)


def _is_node_type(*nodet):
    def wrapper(name):
        return context.env[name]["node_type"] in nodet
    return wrapper

is_struct = _is_node_type(astlib.NodeT.struct)
is_var = _is_node_type(astlib.NodeT.var)
is_let = _is_node_type(astlib.NodeT.let)
is_fun = _is_node_type(astlib.NodeT.fun)
is_protocol = _is_node_type(astlib.NodeT.protocol)
is_adt = _is_node_type(astlib.NodeT.adt)
is_type = _is_node_type(
    astlib.NodeT.struct, astlib.NodeT.commont,
    astlib.NodeT.adt, astlib.NodeT.protocol)
is_real_type = _is_node_type(
    astlib.NodeT.struct,
    astlib.NodeT.adt, astlib.NodeT.protocol)

def split_body(body):
    fields, methods = [], []
    if body in A(astlib.Empty):
        return [], []
    for stmt in body:
        if (stmt in A(astlib.Decl) and
                stmt.decltype == astlib.DeclT.field):
            fields.append(stmt)
        else:
            methods.append(stmt)
    return fields, methods


def only_fields(body):
    return split_body(body)[0]


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
