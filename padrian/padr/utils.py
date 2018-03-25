import itertools

from .inference import infer_type
from .context import context as c
from . import astlib, errors


class A:
    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)


def partition(pred, iterable):
    t1, t2 = itertools.tee(iterable)
    return list(filter(pred, t1)), list(itertools.filterfalse(pred, t2))


_is_field_stmt = lambda stmt: (
    stmt in A(astlib.Decl) and stmt.decltype == astlib.DeclT.field)


def split_body(body):
    return partition(_is_field_stmt, body)


def only_fields(body):
    return list(filter(_is_field_stmt, body))


def get_parent(expr):
    if expr in A(astlib.DataMember):
        return get_parent(expr.parent)
    return expr


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
        struct_info = c.env[type_.type_]
        params = struct_info["params"]
        i = 0
        for param in params:
            if param not in mapping:
                mapping[param] = type_.params[i]
            i += 1
        return mapping
    return {}


def get_low_level_type(type_, expr):
    if expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.adt:
            return infer_type(expr.member)
    errors.not_implemented("stmt {} is not supported".format(expr), func=get_low_level_type)


def get_parent_name(expr):
    if expr in A(astlib.DataMember):
        return get_parent_name(expr.parent)
    return expr


def _update(assignment, **kwds):
    if assignment.left in A(astlib.Name):
        right = kwds.get("right", assignment.right)
        c.env[assignment.left]["expr"] = right
    elif assignment.left in A(astlib.DataMember):
        if assignment.left.datatype == astlib.DataT.adt:
            parent = get_parent_name(assignment.left)
            right = kwds.get("right", assignment.right)
            c.env[parent]["expr"] = right
    else:
        print("Bad::_update", assignment.left)


def register(stmt, **kwds):
    if stmt in A(astlib.Assignment):
        _update(stmt, **kwds)
    elif stmt.decltype in (astlib.DeclT.var, astlib.DeclT.let):
        c.env[stmt.name] = {**{
            "node_type": declt_to_nodet(stmt.decltype),
            "type_": stmt.type_,
            "mapping": get_mapping(stmt.type_),
            "expr": stmt.expr
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.fun,):
        c.env[stmt.name] = {**{
            "node_type": declt_to_nodet(stmt.decltype),
            "type_": stmt.rettype,
            "args": stmt.args,
            "body": stmt.body
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.struct_func,):
        c.env[c.parent]["methods"][stmt.name] = {**{
            "type_": stmt.rettype,
            "args": stmt.args,
            "body": stmt.body
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.field,):
        c.env[c.parent]["fields"][stmt.name] = {**{
            "type_": stmt.type_,
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.struct, astlib.DeclT.adt):
        c.env[stmt.name] = {**{
            "node_type": declt_to_nodet(stmt.decltype),
            "params": stmt.params,
            "fields": {},
            "methods": {}
        }, **kwds}


def register_var_or_let(name, decltype, type_, expr):
    low_level_type = None
    info = c.env[type_]
    if info:
        type_node_type = info["node_type"]
        if type_node_type == astlib.NodeT.adt:
            low_level_type = get_low_level_type(type_, expr)
    c.env[name] = {
        "node_type": declt_to_nodet(decltype),
        "type_": type_,
        "mapping": get_mapping(type_),
        "low_level_type": low_level_type
    }


def register_field(name, type_):
    c.env.update(c.parent, {
        "fields": add_dicts(
            c.env[c.parent]["fields"], {
            name: {
                "type_": type_
            }
        })
    })


def register_data_decl(name, decltype, params):
    c.env[name] = {
        "node_type": declt_to_nodet(decltype),
        "params": params,
        "methods": {},
        "fields": {}
    }


def register_func(name, rettype, args):
    c.env[name] = {
        "node_type": astlib.NodeT.fun,
        "type_": rettype,
        "args": args
    }


def register_func_as_child(parent, name, rettype, args):
    c.env.update(parent, {
        "methods": add_dicts(
            c.env[parent]["methods"], {
            name: {
                "type_": rettype,
                "args": args
            }
        })
    })


def register_args(args):
    for name, type_ in args:
        c.env[name] = {
            "node_type": astlib.NodeT.arg,
            "type_": type_
        }


def register_params(params):
    for param in params:
        c.env[param] = {
            "node_type": astlib.NodeT.commont
        }
