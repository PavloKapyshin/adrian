from . import layers, astlib, errors
from . import cdefs, defs
from .context import context, get
from .patterns import A


def name_exists(name):
    return get(name) is not None


def t(type_):
    if type_ in A(astlib.Name):
        if not name_exists(type_):
            errors.non_existing_name(
                context.exit_on_error, name=str(type_))


def cfunc_call(call):
    cfunc_name(call.name)
    call_args(call.args)


def e(expr):
    if expr in A(astlib.Name):
        if not name_exists(expr):
            errors.non_existing_name(
                context.exit_on_error, name=str(expr))

    if expr in A(astlib.CFuncCall):
        cfunc_call(expr)


def cfunc_name(name):
    print(name, type(name))


def call_args(args):
    map(e, args)


class NameExistence(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        if name_exists(decl.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(decl.name))
        t(decl.type_)
        e(decl.expr)
        context.env.add(str(decl.name), object())
        yield decl

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        e(assment.var)
        e(assment.expr)
        yield assment

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        cfunc_call(call)
        yield call
