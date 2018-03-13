import threading
import contextlib

from . import astlib

context = threading.local()

@contextlib.contextmanager
def new_context(*, env, exit_on_error):
    context.env = env
    context.exit_on_error = exit_on_error
    yield


def add_decl(stmt):
    context.env[stmt.name] = {
        "type": stmt.type_,
        "node_type": (
            astlib.NodeT.var
            if stmt.decltype == astlib.DeclT.var
            else astlib.NodeT.let)
    }


def add_data_decl(stmt):
    node_type = astlib.NodeT.struct
    if stmt.decltype == astlib.DeclT.adt:
        node_type = astlib.NodeT.adt
    elif stmt.decltype == astlib.DeclT.protocol:
        node_type = astlib.NodeT.protocol
    context.env[stmt.name] = {
        "node_type": node_type
    }