import itertools

from . import astlib, errors
from .context import context


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


def nodetype_from_decl(declt):
    if declt == astlib.DeclT.var:
        return astlib.NodeT.var
    return astlib.NodeT.let


def scroll_to_parent(node):
    if node in A(astlib.DataMember):
        return scroll_to_parent(node.parent)
    return node


def nodetype(request):
    info = context.env[request]
    if info is None:
        errors.unknown_name(request)
    return info["node_type"]


def _is_of_nodetype(*nodetypes):
    def helper(request):
        if request in A(astlib.PyObject):
            return False
        return nodetype(request) in nodetypes
    return helper


is_type = _is_of_nodetype(
    astlib.NodeT.adt, astlib.NodeT.struct, astlib.NodeT.parameter)
is_real_type = _is_of_nodetype(astlib.NodeT.adt, astlib.NodeT.struct)
is_adt = _is_of_nodetype(astlib.NodeT.adt)
is_protocol = _is_of_nodetype(astlib.NodeT.protocol)
is_struct = _is_of_nodetype(astlib.NodeT.struct)
is_fun = _is_of_nodetype(astlib.NodeT.fun)
is_var = _is_of_nodetype(astlib.NodeT.var)
is_let = _is_of_nodetype(astlib.NodeT.let)
