from . import astlib, errors, defs
from .context import context
from .utils import A


def infer_type(expr):
    if expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.PyFuncCall):
        if expr.name == defs.FUNC_TO_INT:
            return astlib.PyType(defs.TYPE_INT)
        elif expr.name == defs.FUNC_TO_STR:
            return astlib.PyType(defs.TYPE_STR)
        elif expr.name == defs.FUNC_TO_SET:
            return astlib.PyType(defs.TYPE_SET)
        elif expr.name == defs.FUNC_TO_LIST:
            return astlib.PyType(defs.TYPE_LIST)
        else:
            errors.later()
    elif expr in A(astlib.Name):
        return context.env[expr]["type"]
    elif expr in A(astlib.Expr):
        return infer_type(expr.left)
    elif expr in A(int):
        return astlib.PyType(defs.TYPE_INT)
    elif expr in A(str):
        return astlib.PyType(defs.TYPE_STR)
    elif expr in A(list):
        return astlib.PyType(defs.TYPE_LIST)
    elif expr in A(set):
        return astlib.PyType(defs.TYPE_SET)
    elif expr in A(dict):
        return astlib.PyType(defs.TYPE_DICT)
    else:
        errors.later()
