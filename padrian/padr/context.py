import threading
import contextlib
from collections import OrderedDict

from . import astlib


context = threading.local()

@contextlib.contextmanager
def new_context(*, env, exit_on_error, module_paths, clibs_includes, i_count):
    context.env = env
    context.exit_on_error = exit_on_error
    context.module_paths = module_paths
    context.clibs_includes = clibs_includes or OrderedDict()
    context.i_count = i_count
    context.func = None
    context.parent = None
    yield


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
get_adt_info = _get_info_maker(is_adt)
get_variable_info = _get_info_maker(is_variable)
get_function_info = _get_info_maker(is_function)

def raw_get_type_info(request):
    return context.env[request]

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
