from .context import context
from .utils import A
from . import astlib, layers, defs, inference, utils


def copy(expr):
    return astlib.Callable(
        astlib.CallableT.struct_func, inference.infer_type(expr),
        defs.COPY_METHOD, [expr])


class Copying(layers.Layer):

    def __init__(self):
        self.b = layers._b(Copying)

    # Inner ast nodes translation.
    def e(self, expr):
        if expr in A(astlib.Name, astlib.DataMember):
            return copy(expr)
        if expr in A(astlib.Callable):
            return astlib.Callable(
                expr.callabletype, expr.parent, expr.name,
                self.a(expr.args))
        return expr

    def ae(self, expr):
        return expr

    def a(self, args):
        if len(args) == 0 or isinstance(args[0], tuple):
            return args
        return [self.ae(arg) for arg in args]

    # Subcore funcs.
    def fun_decl(self, stmt):
        utils.register_func(stmt.name, stmt.rettype, stmt.args)
        +context.env
        utils.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    def struct_func_decl(self, stmt):
        utils.register_func_as_child(
            stmt.parent, stmt.name, stmt.rettype, stmt.args)
        +context.env
        utils.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    # Core funcs.
    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr = self.ae(stmt.expr)
        yield astlib.Return(expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr = self.e(stmt.right)
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        args = self.a(stmt.args)
        yield astlib.Callable(stmt.callabletype, stmt.parent, stmt.name, args)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            utils.register_field(stmt.name, stmt.type_)
            yield stmt
        else:
            expr = self.e(stmt.expr)
            utils.register_var_or_let(stmt.name, stmt.decltype, stmt.type_)
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.fun:
            yield from self.fun_decl(stmt)
        if stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register_data_decl(stmt.name, stmt.decltype, stmt.params)
        +context.env
        context.parent = stmt.name
        utils.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
