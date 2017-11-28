import sys

from . import astlib, defs, inference
from .patterns import A


def is_ctype(type_):
    return type_ in A(astlib.CType)


def get_assignment(name, val):
    return astlib.Assignment(
        astlib.Deref(name), "=", val)


def get_val(value):
    if value in A(astlib.Name, astlib.StructMember):
        return astlib.Deref(value)
    if value in A(astlib.Expr):
        return astlib.Expr(
            value.op, get_val(value.left_expr),
            get_val(value.right_expr))
    return value


def heapify(expr, name):
    type_ = inference.infer(expr)
    if is_ctype(type_):
        allocation = astlib.CFuncCall(
            "malloc", [astlib.CFuncCall(
                "sizeof", [astlib.StructScalar(type_)])])
        assignment = get_assignment(name, get_val(expr))
        return allocation, [assignment]
    return astlib.StructFuncCall(
        type_, defs.COPY_METHOD_NAME, args=[expr]), []