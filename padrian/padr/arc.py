from . import astlib, layers, env, inference, utils, defs
from .context import context
from .utils import A


def deinit(name, type_):
    return astlib.Callable(
        astlib.CallableT.struct_func, type_,
        defs.DEINIT_METHOD, [name])


def add_decl_args(args):
        for name, type_ in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": type_
            }


class ARC(layers.Layer):

    def __init__(self, flist=None):
        self.flist = flist or env.Env()
        self.b = layers._b(ARC, flist=self.flist)

    # Misc.
    def update_b(self):
        self.b = layers._b(ARC, flist=self.flist)

    def remove_return_expr_from_flist(self, expr):
        if expr in A(astlib.Name):
            self.delfromflist(expr)
        if expr in A(astlib.DataMember):
            self.remove_return_expr_from_flist(expr.parent)

    def free(self, expr):
        if expr in A(astlib.Name):
            return deinit(expr, inference.infer_type(expr))
        return self.free(astlib.Name(expr))

    def addtoflist(self, stmt):
        if stmt.expr not in A(astlib.Ref):
            self.flist[stmt.name] = {
                "type_": stmt.type_
            }

    def delfromflist(self, name):
        del self.flist[name]

    def free_scope(self):
        for key, info in self.flist.cspace():
            yield self.free(key)

    # Subcore funcs.
    def fun_decl(self, stmt):
        context.env[stmt.name] = {
            "node_type": astlib.NodeT.fun,
            "type_": stmt.type_,
            "args": stmt.args
        }
        +context.env
        +self.flist
        add_decl_args(stmt.args)
        self.update_b()
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

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
        +self.flist
        add_decl_args(stmt.args)
        self.update_b()
        body = self.b(stmt.body)
        if stmt.rettype in A(astlib.Void):
            body += list(self.free_scope())
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, body)
        -context.env
        -self.flist

    # Core funcs.
    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        # TODO:
        #   * remove possible memory leaks by
        #     freeing stmt.left if inited
        yield stmt

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        self.remove_return_expr_from_flist(stmt.expr)
        yield from self.free_scope()
        yield stmt

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield stmt

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield stmt
        else:
            context.env[stmt.name] = {
                "node_type": utils.declt_to_nodet(stmt.decltype),
                "type_": stmt.type_
            }
            self.addtoflist(stmt)
            yield stmt

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
            "methods": {}
        }
        +context.env
        +self.flist
        context.parent = stmt.name
        for param in stmt.params:
            context.env[param] = {
                "node_type": astlib.NodeT.commont
            }
        self.update_b()
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
        -self.flist

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()
