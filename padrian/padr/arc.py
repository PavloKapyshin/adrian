from . import layers, astlib, env, defs, inference
from .context import context, add_decl, add_data_decl
from .utils import A


def deinit(name, type_):
    return astlib.Callable(
        astlib.CallableT.struct_func, type_,
        defs.DEINIT_METHOD, [name])


class ARC(layers.Layer):

    def __init__(self, flist=None):
        self.flist = flist or env.Env()

    def free_scope(self):
        for key, entry in self.flist.cspace():
            yield self.free(key)

    def free(self, expr):
        if expr in A(astlib.Name):
            return deinit(expr, inference.infer_type(expr))
        if expr in A(str):
            return self.free(astlib.Name(expr))

    def add_flist(self, stmt):
        self.flist[stmt.name] = {
            "type": stmt.type_
        }

    def del_flist(self, name):
        del self.flist[name]

    def b(self, body):
        reg = ARC(flist=flist).get_registry()
        return list(map(lambda stmt: list(
                layers.transform_node(stmt, registry=reg)),
            body))

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield stmt
        else:
            add_decl(stmt)
            self.add_flist(stmt)
            yield stmt

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        add_data_decl(stmt)
        +context.env
        +self.flist
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, self.b(stmt.body))
        -context.env
        -self.flist

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.free_scope()
