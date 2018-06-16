from . import astlib, env_api, errors, defs
from .utils import A


def infer_type_in_decl(expr):
    return _same_infer(expr)


def infer_type_in_usage(expr):
    if expr in A(astlib.AdtMember):
        return infer_type_in_usage(expr.member)
    elif expr in A(astlib.Name):
        return env_api.variable_info(expr)["spec_type"]
    elif expr in A(astlib.StructField):
        return env_api.field_info(expr.struct, expr.field)["spec_type"]
    return _same_infer(expr)


def _same_infer(expr):
    if expr in A(astlib.AdtMember):
        return expr.base
    elif expr in A(astlib.Name):
        return env_api.variable_info(expr)["type_"]
    elif expr in A(astlib.StructField):
        return env_api.field_info(expr.struct, expr.field)["type_"]
    elif expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.PyFuncCall):
        result = _infer_from_py_func_call(expr)
        if result is not None:
            return result
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


def _infer_from_py_func_call(expr):
    if expr.name in (defs.LEN, defs.TO_INT):
        return astlib.PyType(defs.INT)
    elif expr.name in (defs.READ_FILE, defs.TO_STR):
        return astlib.PyType(defs.STR)
