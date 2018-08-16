from . import astlib, errors, defs
from .context import context
from .utils import A


def infer_type(expr):
    if expr in A(astlib.PyTypeCall):
        return astlib.PyType(expr.name)
    elif expr in A(astlib.PyType):
        if expr.name == defs.TYPE_NONE:
            return None
        errors.later()
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
        info = context.env[expr]
        if info["node_type"] in (astlib.NodeT.var, astlib.NodeT.let):
            return context.env[expr]["type"]
        return expr
    elif expr in A(astlib.FuncCall):
        info = context.env[expr.name]
        if info["node_type"] == astlib.NodeT.func:
            return info["rettype"]
        elif info["node_type"] == astlib.NodeT.struct:
            return expr.name
        else:
            errors.later()
    elif expr in A(astlib.InstanceValue):
        return expr.type_
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
    elif expr is None:
        return astlib.Name(defs.TYPE_VOID)
    elif expr in A(astlib.GenericType):
        return expr
    else:
        errors.later()


def types_are_equal(type1, type2):
    if type1 not in A(type(type2)):
        return False
    if type1 in A(astlib.Name):
        return type1 == type2
    elif type1 in A(astlib.PyType):
        return type1.name == type2.name
    else:
        errors.later()


def is_super_type(sub_type, super_type):
    if sub_type in A(astlib.PyType) or super_type in A(astlib.PyType):
        return False
    if sub_type in A(astlib.GenericType):
        sub_type = sub_type.base
    if super_type in A(astlib.GenericType):
        super_type = super_type.base
    assert(sub_type in A(astlib.Name))
    assert(super_type in A(astlib.Name))
    sub_type_info = context.env[sub_type]
    assert(sub_type_info["node_type"] in (
            astlib.NodeT.protocol, astlib.NodeT.struct))
    for impled in sub_type_info["implemented_protocols"]:
        if impled in A(astlib.GenericType):
            impled = impled.base
        if impled == super_type:
            return True
    return False
