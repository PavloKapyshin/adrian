from .context import context
from .utils import A
from . import errors, astlib


def _check_info(info, request, nodetype_checker):
    if info is None:
        errors.unknown_name(request)
    nodetype = info["node_type"]
    if not nodetype_checker(nodetype):
        errors.wrong_nodetype(request, nodetype)


def _get_info_maker(nodetype_checker):
    def helper(request):
        info = unsafe_info(request)
        _check_info(info, request, nodetype_checker)
        return info
    return helper


def _is_of_nodetype_maker(*nodetypes):
    def helper(nodetype):
        return nodetype in nodetypes
    return helper


# Nodetype checkers.
is_var = _is_of_nodetype_maker(astlib.NodeT.var)
is_let = _is_of_nodetype_maker(astlib.NodeT.let)
is_fun = _is_of_nodetype_maker(astlib.NodeT.fun)
is_struct = _is_of_nodetype_maker(astlib.NodeT.struct)
is_protocol = _is_of_nodetype_maker(astlib.NodeT.protocol)
is_adt = _is_of_nodetype_maker(astlib.NodeT.adt)
is_parameter = _is_of_nodetype_maker(astlib.NodeT.parameter)

is_variable = _is_of_nodetype_maker(
    astlib.NodeT.var, astlib.NodeT.let, astlib.NodeT.arg)
is_type = _is_of_nodetype_maker(
    astlib.NodeT.struct, astlib.NodeT.adt, astlib.NodeT.protocol,
    astlib.NodeT.parameter)
is_real_type = _is_of_nodetype_maker(
    astlib.NodeT.struct, astlib.NodeT.adt, astlib.NodeT.protocol)


# High-level API for addressing with Env.
# info = entry in Env.
# we check that info is good (is not None and is of nodetype you expect)
var_info = _get_info_maker(is_var)
let_info = _get_info_maker(is_let)
fun_info = _get_info_maker(is_fun)
adt_info = _get_info_maker(is_adt)
struct_info = _get_info_maker(is_struct)
protocol_info = _get_info_maker(is_protocol)

variable_info = _get_info_maker(is_variable)
type_info = _get_info_maker(is_type)

def parent_info(node):
    if node in A(astlib.DataMember):
        return parent_info(node.parent)
    elif node in A(astlib.Name):
        return variable_info(node)
    errors.cannot_get_info(node, func=parent_info)


def method_and_struct_info(struct, method_name):
    struct_info_ = struct_info(struct)
    method_info_ = struct_info_["methods"].get(method_name)
    if method_info_ is None:
        errors.no_such_method(struct, method_name)
    return method_info_, struct_info_


def field_info(parent, member):
    if parent in A(astlib.DataMember):
        parent_info_ = field_info(parent.parent, parent.member)
    else:
        parent_info_ = parent_info(parent)
    struct_info_ = struct_info(parent_info_["type_"])
    field_info_ = struct_info_["fields"].get(member)
    if field_info is None:
        errors.no_such_field(parent, parent_info_["type_"], member)
    return field_info_


def method_info(struct, method_name):
    return method_and_struct_info(struct, method_name)[0]


# Unsafe API.
def unsafe_info(request):
    return context.env[request]


# Registration.
def _update_by_assignment(assignment, **kwds):
    left = kwds.get("left", assignment.left)
    right = kwds.get("right", assignment.right)
    if left in A(astlib.Name):
        # Just for checking.
        _ = variable_info(left)
        context.env[left]["expr"] = right
    elif left in A(astlib.DataMember):
        if left.datatype == astlib.DataT.adt:
            context.env[left.parent]["expr"] = right
        elif left.datatype == astlib.DataT.struct:
            context.env[left] = right


def create_type_mapping(type_):
    mapping = {}
    if type_ in A(astlib.GenericType):
        struct_info_ = type_info(type_.base)
        for param, passed_param in zip(struct_info_["params"], type_.params):
            mapping[param] = passed_param
    return mapping


def register_params(params):
    for param in params:
        context.env[param] = {
            "node_type": astlib.NodeT.parameter,
            "params": [],
            "methods": {},
            "fields": {}
        }


def register_args(args):
    for name, type_ in args:
        context.env[name] = {
            "node_type": astlib.NodeT.arg,
            "type_": type_,
            "mapping": create_type_mapping(type_),
            "expr": astlib.Empty()
        }


def register(stmt, **kwds):
    if stmt in A(astlib.Assignment):
        _update_by_assignment(stmt, **kwds)
    elif stmt.decltype in (astlib.DeclT.var, astlib.DeclT.let):
        name = kwds.get("name", stmt.name)
        context.env[name] = {**{
            "node_type": (astlib.NodeT.var
                if stmt.decltype == astlib.DeclT.var
                else astlib.NodeT.let),
            "type_": stmt.type_,
            "mapping": create_type_mapping(stmt.type_),
            "expr": stmt.expr
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.fun,):
        context.env[stmt.name] = {**{
            "node_type": astlib.NodeT.fun,
            "type_": stmt.rettype,
            "args": stmt.args,
            "body": stmt.body
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.struct_func,):
        # Just for checking.
        _ = type_info(context.parent)
        context.env[context.parent]["methods"][stmt.name] = {**{
            "type_": stmt.rettype,
            "args": stmt.args,
            "body": stmt.body
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.field,):
        # Just for checking.
        _ = type_info(context.parent)
        context.env[context.parent]["fields"][stmt.name] = {**{
            "type_": stmt.type_,
        }, **kwds}
    elif stmt.decltype in (astlib.DeclT.struct, astlib.DeclT.adt):
        if stmt.decltype == astlib.DeclT.struct:
            nodetype = astlib.NodeT.struct
        elif stmt.decltype == astlib.DeclT.adt:
            nodetype = astlib.NodeT.adt
        context.env[stmt.name] = {**{
            "node_type": nodetype,
            "params": stmt.params,
            "fields": {},
            "methods": {}
        }, **kwds}
