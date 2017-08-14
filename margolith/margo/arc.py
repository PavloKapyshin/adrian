from . import layers, astlib, errors, defs, env
from .context import context
from .patterns import A


def is_ctype(expr):
    return True


class ARC(layers.Layer):

    def __init__(self):
        self.to_free = env.Env()

    def free(self, name, type_):
        if type_ in A(astlib.CType):
            return astlib.CFuncCall("free", [astlib.Name(name)])

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, type_ in sorted(space[scope].copy().items()):
            yield self.free(key, type_)

    @layers.register(astlib.Decl)
    def decl(self, decl):
        self.to_free.add(str(decl.name), decl.type_)
        yield decl

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        if is_ctype(assment.expr):
            # dont free
            yield assment

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()