import threading
import contextlib

from . import astlib
#from .utils import node_type_from_decltype

context = threading.local()

@contextlib.contextmanager
def new_context(*, env, exit_on_error):
    context.env = env
    context.exit_on_error = exit_on_error
    context.parent = None
    yield


def add_data_decl(stmt):
    node_type = astlib.NodeT.struct
    if stmt.decltype == astlib.DeclT.adt:
        node_type = astlib.NodeT.adt
    elif stmt.decltype == astlib.DeclT.protocol:
        node_type = astlib.NodeT.protocol

    methods = {}
    fields = {}
    if node_type == astlib.NodeT.struct:
        fields_, methods_ = split_body(stmt.body)
        for field_decl in fields_:
            fields[field_decl.name] = field_decl.type_

        for method_decl in methods_:
            methods[method_decl.name] = {
                "args": [],
                "rettype": method_decl.rettype
            }
    context.env[stmt.name] = {
        "node_type": node_type,
        "params": stmt.params,
        "methods": methods,
        "fields": fields
    }