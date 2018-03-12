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
