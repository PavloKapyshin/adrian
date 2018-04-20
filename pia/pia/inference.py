from . import astlib, utils, env_api, errors
from .utils import A


def _infer_same(expr):
    if expr in A(astlib.Ref):
        return infer_type(expr.expr)
    elif expr in A(astlib.Name):
        return env_api.variable_info(expr)["type_"]
    elif expr in A(astlib.StructField):
        return env_api.field_info(expr.struct, expr.field)["type_"]
    elif expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.StructFuncCall):
        if expr.parent in A(astlib.PyObject):
            return expr.parent
        return env_api.method_info(expr.parent, expr.name)["type_"]
    elif expr in A(astlib.StructValue):
        return expr.type_
    errors.cannot_infer_type(expr)


def infer_spec_type(expr):
    return _infer_same(expr)


def infer_type(expr):
    return _infer_same(expr)
