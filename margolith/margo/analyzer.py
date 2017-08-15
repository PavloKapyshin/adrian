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
        return str(eval(" ".join([compute(expr.lexpr), expr.op, compute(expr.rexpr)])))
    errors.not_implemented("can't compute D:")


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
    errors.not_implemented("analyzer:t (type_ {})".format(type_))


def e(expr):
    if expr in A(astlib.FuncCall):
        return list(func_call(expr))[0]

    if expr in A(astlib.Expr):
        return astlib.Expr(expr.op, e(expr.lexpr), e(expr.rexpr))

    if expr in A(astlib.Name, astlib.Empty):
        return expr

    errors.not_implemented("analyzer:e (expr {})".format(expr))


def call_args(args):
    return map(expr, args.as_list())


class Analyzer(layers.Layer):

    def b(self, body):
        reg = Analyzer().get_registry()
        return map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body.as_list())

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            astlib.Name(decl.name), t(decl.type_), e(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            e(assment.var), assment.op, e(assment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield from func_call(call)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        if struct.interfaces:
            errors.not_implemented("interfaces are not supported")
        if struct.param_types:
            errors.not_implemented("parameterized structs are not supported")
        yield astlib.Struct(
            struct.name, struct.param_types, struct.interfaces,
            self.b(struct.body))

    @layers.register(astlib.Interface)
    def inf(self, inf):
        errors.not_implemented("interface declaration is not supported")