import enum

from . import layers, astlib, errors, defs
from .context import context, get, get_in_current_scope
from .patterns import A


class NType(enum.Enum):
    var = 1
    let = 2
    fun = 3
    struct = 4
    method = 5
    field = 6


def name_exists(name):
    return get(name) is not None


def name_exists_in_current_scope(name):
    return get_in_current_scope(name) is not None


def t(type_):
    if type_ in A(astlib.Name):
        if not name_exists(type_):
            errors.non_existing_name(
                context.exit_on_error, name=str(type_))


def cfunc_call(call):
    cfunc_name(call.name)
    call_args(call.args)


def func_call(call):
    if not name_exists(call.name):
        errors.non_existing_name(
            context.exit_on_error, name=str(call.name))
    call_args(call.args)


def e(expr):
    if expr in A(astlib.Name):
        if not name_exists(expr):
            errors.non_existing_name(
                context.exit_on_error, name=str(expr))

    if expr in A(astlib.CFuncCall):
        cfunc_call(expr)

    if expr in A(astlib.FuncCall):
        func_call(expr)

    if expr in A(astlib.StructElem):
        # We hope that elem really exists in this struct :D
        if not name_exists(expr.name):
            errors.non_existing_name(
                context.exit_on_error, name=str(expr.name))

    if expr in A(astlib.StructCall):
        if not name_exists(expr.name):
            errors.non_existing_name(
                context.exit_on_error, name=str(expr.name))
        call_args(expr.args)


def decl_args(args):
    for arg in args:
        context.env.add(str(arg.name), {
            "node_type": NType.var
        })
        t(arg.type_)


def cfunc_name(name):
    errors.not_implemented(
        context.exit_on_error,
        "user calls of functions from c module are not supported")
    print(name, type(name))


def call_args(args):
    map(e, args)


class NameExistence(layers.Layer):

    def b(self, body):
        reg = NameExistence().get_registry()
        for stmt in body:
            list(layers.transform_node(stmt, registry=reg))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        if name_exists_in_current_scope(decl.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(decl.name))
        t(decl.type_)
        e(decl.expr)
        context.env.add(str(decl.name), {
            "node_type": NType.var
        })
        yield decl

    @layers.register(astlib.LetDecl)
    def let_decl(self, let_decl):
        if name_exists_in_current_scope(let_decl.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(let_decl.name))
        t(let_decl.type_)
        e(let_decl.expr)
        context.env.add(str(let_decl.name), {
            "node_type": NType.let
        })
        yield astlib.Decl(
            let_decl.name, let_decl.type_, let_decl.expr)

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        e(assment.var)
        e(assment.expr)
        if assment.var in A(astlib.Name):
            if get(assment.var)["node_type"] == NType.let:
                errors.cant_reassign(
                    context.exit_on_error, name=str(assment.var))
        yield assment

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        cfunc_call(call)
        yield call

    @layers.register(astlib.Return)
    def return_(self, return_):
        e(return_.expr)
        yield return_

    @layers.register(astlib.Func)
    def func(self, func):
        if name_exists_in_current_scope(func.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(func.name))
        context.env.add(str(func.name), {
            "node_type": NType.fun
        })
        context.env.add_scope()
        decl_args(func.args)
        t(func.rettype)
        self.b(func.body)
        yield func
        context.env.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        if name_exists(struct.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(struct.name))
        context.env.add(str(struct.name), {
            "node_type": NType.struct
        })
        context.env.add_scope()
        self.b(struct.body)
        yield struct
        context.env.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        if name_exists_in_current_scope(method.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(method.name))
        context.env.add(str(method.name), {
            "node_type": NType.method
        })
        context.env.add_scope()
        context.env.add("self", {
            "node_type": NType.var
        })

        decl_args(method.args)
        t(method.rettype)
        self.b(method.body)
        yield method
        context.env.del_scope()


    @layers.register(astlib.Field)
    def field(self, field):
        if name_exists_in_current_scope(field.name):
            errors.cant_reassign(
                context.exit_on_error, name=str(field.name))
        context.env.add(str(field.name), {
            "node_type": NType.var
        })
        t(field.type_)
        yield field
