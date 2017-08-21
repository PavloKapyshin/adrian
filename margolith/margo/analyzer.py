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
    errors.not_implemented(
        context.exit_on_error, "can't compute D:")


def method_call(call):
    if call.method in A(astlib.StructElem):
        errors.not_implemented(
            context.exit_on_error,
            ("nested struct elements and method calls are not supported\n" +
                "How to fix: try to extract some of these statements to variables"))
    yield astlib.MethodCall(
        e(call.base), call.method, call_args(call.args))


def func_call(call):
    if call.name in A(astlib.ModuleMember):
        if call.name.module != cdefs.CMODULE_NAME:
            errors.not_implemented(
                context.exit_on_error, "only c module is supported")
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
        errors.not_implemented(
            context.exit_on_error, "only c module is supported")

    if type_ in A(astlib.Name):
        if str(type_) == "Void":
            return astlib.CType("Void")
        return type_

    if type_ in A(astlib.Empty):
        return type_

    errors.not_implemented(
        context.exit_on_error, "analyzer:t (type_ {})".format(type_))


def e(expr):
    if expr in A(astlib.FuncCall):
        return list(func_call(expr))[0]

    if expr in A(astlib.MethodCall):
        return list(method_call(expr))[0]

    if expr in A(astlib.Expr):
        return astlib.Expr(expr.op, e(expr.lexpr), e(expr.rexpr))

    if expr in A(astlib.Name, astlib.Empty, astlib.StructElem):
        return expr

    errors.not_implemented(
        context.exit_on_error, "analyzer:e (expr {})".format(expr))


def decl_args(args):
    return [astlib.Arg(name, t(type_)) for name, type_ in args.as_list()]


def call_args(args):
    return list(map(e, args.as_list()))


class Analyzer(layers.Layer):

    def b(self, body):
        reg = Analyzer().get_registry()
        return list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body.as_list()))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield astlib.Decl(
            astlib.Name(decl.name), t(decl.type_), e(decl.expr))

    @layers.register(astlib.LetDecl)
    def let_decl(self, let_decl):
        yield astlib.LetDecl(
            astlib.Name(let_decl.name), t(let_decl.type_), e(let_decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            e(assment.var), assment.op, e(assment.expr))

    @layers.register(astlib.Func)
    def func(self, func):
        yield astlib.Func(
            astlib.Name(func.name), decl_args(func.args),
            t(func.rettype), self.b(func.body))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(e(return_.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield from func_call(call)

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        yield from method_call(call)

    @layers.register(astlib.Method)
    def method(self, method):
        yield astlib.Method(
            astlib.Name(method.name), decl_args(method.args),
            t(method.rettype), self.b(method.body))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        if struct.protocols:
            errors.not_implemented(
                context.exit_on_error,
                "protocols are not supported")
        if struct.parameters:
            errors.not_implemented(
                context.exit_on_error,
                "parameterized structs are not supported")
        yield astlib.Struct(
            astlib.Name(struct.name), [], [],
            self.b(struct.body))

    @layers.register(astlib.Protocol)
    def protocol(self, protocol):
        if protocol.parameters:
            errors.not_implemented(
                context.exit_on_error,
                "parameterized protocols are not supported")
        errors.not_implemented(
            context.exit_on_error,
            "protocol declaration is not supported")

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            field.name, t(field.type_))
