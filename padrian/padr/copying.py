from . import layers, astlib, inference, defs
from .context import context, add_decl, add_data_decl
from .utils import A


def copy(expr):
    return astlib.Callable(
        astlib.CallableT.struct_func, inference.infer_type(expr),
        defs.COPY_METHOD, [expr])


class Copying(layers.Layer):

    def b(self, body):
        reg = Copying().get_registry()
        return list(map(lambda stmt: list(
                layers.transform_node(stmt, registry=reg)),
            body))

    def e(self, expr):
        if expr in A(astlib.Name):
            return copy(expr)
        return expr

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield stmt
        else:
            expr = self.e(stmt.expr)
            result = astlib.Decl(
                stmt.decltype, stmt.name, stmt.type_, expr)
            add_decl(result)
            yield result

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        add_data_decl(stmt)
        +context.env
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, self.b(stmt.body))
        -context.env
