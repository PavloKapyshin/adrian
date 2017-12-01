from . import astlib, errors
from .context import context
from .patterns import A


def _unknown_stmt_in_struct_body():
    errors.not_implemented("unknown statement in struct body")


def split_struct_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.FieldDecl):
            fields.append(stmt)
        elif stmt in A(astlib.MethodDecl):
            methods.append(stmt)
        else:
            _unknown_stmt_in_struct_body()
    return fields, methods
