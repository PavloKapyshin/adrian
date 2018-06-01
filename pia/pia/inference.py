from . import astlib, utils, env_api, errors, defs
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
    elif expr in A(astlib.PyFuncCall):
        if expr.name == defs.LEN:
            return astlib.PyType(defs.INT)
        elif expr.name == defs.READ_FILE:
            return astlib.PyType(defs.STR)
        elif expr.name == defs.TO_INT:
            return astlib.PyType(defs.INT)
        elif expr.name == defs.TO_STR:
            return astlib.PyType(defs.STR)
    elif expr in A(astlib.PyConstant):
        if expr.name == defs.ARGV:
            return astlib.PyType(defs.LIST)
    elif expr in A(astlib.StructFuncCall):
        if expr.parent in A(astlib.PyObject):
            return expr.parent
        return env_api.method_info(expr.parent, expr.name)["type_"]
    elif expr in A(astlib.StructValue):
        return expr.type_
    elif expr in A(astlib.FuncCall):
        return env_api.fun_info(expr.name)["type_"]
    elif expr is None:
        return astlib.PyType(defs.MAYBE)
    errors.cannot_infer_type(expr)


def infer_spec_type(expr):
    if expr in A(astlib.AdtMember):
        return infer_spec_type(expr.member)
    elif expr in A(astlib.Name):
        return env_api.variable_info(expr)["spec_type"]
    elif expr in A(astlib.StructField):
        return env_api.field_info(expr.struct, expr.field)["spec_type"]
    return _infer_same(expr)


def infer_type(expr):
    if expr in A(astlib.AdtMember):
        return expr.base
    return _infer_same(expr)
