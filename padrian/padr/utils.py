import itertools

from . import astlib


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
