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
        elif expr.name == defs.FUNC_READ_FILE:
            return astlib.PyType(defs.TYPE_STR)
        else:
            errors.later()
    elif expr in A(astlib.Name):
        return context.env[expr]["type"]
    elif expr in A(astlib.FuncCall):
        info = context.env[expr.name]
        if info["node_type"] == astlib.NodeT.func:
            return info["rettype"]
        elif info["node_type"] == astlib.NodeT.struct:
            return expr.name
        else:
            errors.later()
    elif expr in A(astlib.StructPath):
        # TODO: refactor
        path = expr.path
        if len(path) > 2 or path[0] not in A(astlib.Name):
            errors.later()
        root_info = context.env[path[0]]
        root_expr = root_info["expr"]
        root_type = root_info["type"]
        if root_type not in A(astlib.Name):
            errors.later()
        type_info = context.env[root_type]
        result = None
        for elem in path[1:]:
            if elem in A(astlib.FuncCall):
                errors.later()
            else:
                result = type_info["fields"][elem]["type"]
        return result
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
