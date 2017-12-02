import sys

from . import astlib, errors, defs
from .context import context, get
from .patterns import A


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()
    if expr in A(astlib.Name):
        return get(expr)["type"]
    if expr in A(astlib.FuncCall):
        return get(expr.name)["type"]
    if expr in A(astlib.Expr):
        # +, *, -, / returns value of the same type.
        return infer(expr.right_expr)
    if expr in A(astlib.StructCall):
        struct_entry = get(expr.name)
        init_method = struct_entry["methods"][defs.INIT_METHOD_NAME]
        type_ = init_method["type"]
        if not (type_ in A(astlib.ParameterizedType)):
            return type_
    if expr in A(astlib.StructFuncCall):
        methods = get(expr.struct)["methods"]
        method = methods[str(expr.func_name)]
        return method["type"]
    if expr in A(astlib.StructMember):
        type_of_struct = infer(expr.struct)
        fields = get(type_of_struct)["fields"]
        field = fields[str(expr.member)]
        return field["type"]
    if expr in A(astlib.Deref, astlib.Ref):
        return infer(expr.expr)
    if expr in A(astlib.CFuncCall):
        if expr.name == "malloc":
            sizeof = expr.args[0]
            return sizeof.args[0].type_
    errors.not_implemented(
        "can't infer type from expression {}".format(expr))