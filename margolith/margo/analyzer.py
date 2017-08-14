"""
Translates some FuncCalls to StructCall objects.
Translates linked lists to python's lists.
"""

from . import layers, astlib, errors
from . import cdefs, defs
from .context import context
from .patterns import A


def compute(expr):
    if expr in A(astlib.IntLiteral):
        return expr.literal
    if expr in A(astlib.Expr):
        return eval(" ".join([compute(expr.lexpr), expr.op, compute(expr.rexpr)]))
    errors.not_implemented("._.")


def func_call(call):
    if call.name in A(astlib.ModuleMember):
        if call.name.module != cdefs.CMODULE_NAME:
            errors.not_implemented("only c module is supported")
        args = call.args.as_list()
        # you can't use c functions, only types, for now :D
        # computing at compile-time :D
        arg = str(compute(args[0]))
        yield getattr(
            astlib, "C" + str(call.name.member))(arg)
    else:
        node_cls = astlib.FuncCall
        if defs.TYPE_NAME_REGEX.fullmatch(str(call.name)):
            node_cls = astlib.StructCall
        yield node_cls(call.name, call_args(call.args))


def t(type_):
    if type_ in A(astlib.ModuleMember):
        if type_.module == cdefs.CMODULE_NAME:
            return astlib.CType(str(type_.member))
        errors.not_implemented("only c module is supported")
    errors.not_implemented(":) (analyzer: type_)")


def e(expr):
    if expr in A(astlib.FuncCall):
        return list(func_call(expr))[0]

    if expr in A(astlib.Expr):
        return astlib.Expr(expr.op, e(expr.lexpr), e(expr.rexpr))

    if expr in A(astlib.Name, astlib.Empty):
        return expr

    errors.not_implemented(":) (analyzer: expr)")


def call_args(args):
    return map(expr, args.as_list())


class Analyzer(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            decl.name, t(decl.type_), e(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            e(assment.var), assment.op, e(assment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield from func_call(call)