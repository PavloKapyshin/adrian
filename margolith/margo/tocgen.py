import sys

from . import cdefs, layers, astlib, errors, defs
from .context import context
from .patterns import A
from adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast32": "int_fast32",
    "IntFast64": "int_fast64",
    "UIntFast8": "uint_fast8",
    "UIntFast32": "uint_fast32",
    "UIntFast64": "uint_fast64",
}

TO_COP = {
    "+": cgen.COps.plus,
    "-": cgen.COps.minus,
    "*": cgen.COps.star,
    "/": cgen.COps.slash,
}


def cfunc_call(call):
    if call.name == cdefs.SIZEOF_FUNC_NAME:
        yield cgen.SizeOf(*call_args(call.args))
    elif call.name in cdefs.CFUNCS:
        yield getattr(cdefs, str(call.name).upper() + "_FUNC_DESCR")(
            *call_args(call.args))

def t(type_):
    if type_ in A(astlib.CType):
        return cgen.CTypes.ptr(getattr(cgen.CTypes, TO_CTYPE[str(type_)]))
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t (type_ {})".format(type_))


def t_without_ptr(type_):
    if type_ in A(astlib.CType):
        return getattr(cgen.CTypes, TO_CTYPE[str(type_)])
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t_without_ptr (type_ {})".format(type_))


def e(expr):
    if expr in A(astlib.CINT_TYPES):
        return cgen.Val(
            literal=expr.literal,
            type_=getattr(cgen.CTypes, TO_CTYPE[str(expr.to_type())]))

    if expr in A(astlib.Deref):
        return cgen.DeRef(e(expr.expr))

    if expr in A(astlib.Name):
        return cgen.Var(str(expr))

    if expr in A(astlib.CFuncCall):
        return list(cfunc_call(expr))[0]

    if expr in A(astlib.StructScalar):
        return cgen.StructType(t_without_ptr(expr.type_))

    if expr in A(astlib.Expr):
        return cgen.Expr(
            TO_COP[expr.op], e(expr.lexpr), e(expr.rexpr))

    errors.not_implemented(
        context.exit_on_error,
        "tocgen: e (expr {} {})".format(expr, type(expr)))


def call_args(args):
    return map(e, args)


class ToCGen(layers.Layer):

    def b(self, body):
        reg = ToCGen().get_registry()
        return map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body)

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=t(decl.type_), expr=e(decl.expr))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield from cfunc_call(call)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield cgen.Assignment(
            name=e(assignment.var),
            expr=e(assignment.expr))

    @layers.register(astlib.Protocol)
    def protocol(self, protocol):
        yield from []

    @layers.register(astlib.Struct)
    def struct(self, struct):
        yield cgen.Struct(
            name=str(struct.name),
            body=self.b(struct.body))
