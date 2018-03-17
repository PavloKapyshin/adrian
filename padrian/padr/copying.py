from .context import context
from .utils import A
from . import astlib, layers, defs, inference, utils


def copy(expr):
    return astlib.Callable(
        astlib.CallableT.struct_func, inference.infer_type(expr),
        defs.COPY_METHOD, [expr])


def add_decl_args(args):
    for name, type_ in args:
        context.env[name] = {
            "node_type": astlib.NodeT.let,
            "type_": type_
        }


class Copying(layers.Layer):

    def __init__(self):
        self.b = layers._b(Copying)

    # Inner ast nodes translation.
    def e(self, expr):
        if expr in A(astlib.Name, astlib.DataMember):
            return copy(expr)
        return expr

    def a(self, args):
        if len(args) == 0 or isinstance(args[0], tuple):
            return args
        return [self.e(arg) for arg in args]

    # Subcore funcs.
    def fun_decl(self, stmt):
        context.env[stmt.name] = {
            "node_type": astlib.NodeT.fun,
            "type_": stmt.rettype,
            "args": stmt.args
        }
        +context.env
        add_decl_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    def struct_func_decl(self, stmt):
        context.env.update(stmt.parent, {
            "methods": utils.add_dicts(
                context.env[stmt.parent]["methods"], {
                stmt.name: {
                    "type_": stmt.rettype,
                    "args": stmt.args
                }
            })
        })
        +context.env
        add_decl_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    # Core funcs.
    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr = self.e(stmt.expr)
        yield astlib.Return(expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr = self.e(stmt.right)
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        args = self.a(stmt.args)
        yield astlib.Callable(
            stmt.callabletype, stmt.parent, stmt.name, args)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            context.env.update(context.parent, {
                "fields": utils.add_dicts(
                    context.env[context.parent]["fields"], {
                    stmt.name: {
                        "type_": stmt.type_
                    }
                })
            })
            yield stmt
        else:
            expr = self.e(stmt.expr)
            context.env[stmt.name] = {
                "node_type": utils.declt_to_nodet(stmt.decltype),
                "type_": stmt.type_
            }
            yield astlib.Decl(
                stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.fun:
            yield from self.fun_decl(stmt)
        if stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        context.env[stmt.name] = {
            "node_type": utils.declt_to_nodet(stmt.decltype),
            "params": stmt.params,
            "fields": {},
            "methods": {},
        }
        +context.env
        context.parent = stmt.name
        for param in stmt.params:
            context.env[param] = {
                "node_type": astlib.NodeT.commont
            }
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
