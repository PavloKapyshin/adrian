from .context import context
from . import astlib


class A:

    def __init__(self, *types):
        self.types = types

    def __contains__(self, other):
        return isinstance(other, self.types)


def _is_node_type(nodet):
    def wrapper(name):
        return context.env[name]["node_type"] == nodet
    return wrapper

is_struct = _is_node_type(astlib.NodeT.struct)
is_var = _is_node_type(astlib.NodeT.var)
is_let = _is_node_type(astlib.NodeT.let)
is_fun = _is_node_type(astlib.NodeT.fun)
is_protocol = _is_node_type(astlib.NodeT.protocol)
is_adt = _is_node_type(astlib.NodeT.adt)