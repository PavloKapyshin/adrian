from .context import context
from .utils import A
from . import astlib, layers, defs, inference, env_api


def copy(expr):
    return astlib.Callable(
        astlib.CallableT.struct_func, inference.infer_type(expr),
        defs.COPY_METHOD, [expr])


class Copying(layers.Layer):

    def __init__(self):
        self.b = layers.b(Copying)

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

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_scope()
        yield astlib.While(self.ae(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()

    def _if(self, stmt):
        context.env.add_scope()
        result = astlib.If(self.ae(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _elif(self, stmt):
        context.env.add_scope()
        result = astlib.Elif(self.ae(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_ = self._if(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self._elif(elif_))
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_, elifs_, else_)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr = self.ae(stmt.expr)
        yield astlib.Return(expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr = self.e(stmt.right)
        env_api.register(stmt, right=expr)
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        args = self.a(stmt.args)
        yield astlib.Callable(stmt.callabletype, stmt.parent, stmt.name, args)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            env_api.register(stmt)
            yield stmt
        else:
            expr = self.e(stmt.expr)
            env_api.register(stmt, expr=expr)
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        env_api.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        context.parent = stmt.name
        env_api.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
