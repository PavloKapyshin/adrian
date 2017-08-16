from itertools import chain

from . import layers, astlib, errors, defs, env
from .context import context
from .patterns import A


def is_ctype(expr):
    return True


def return_in_func(body):
    for stmt in body:
        if stmt in A(astlib.Return):
            return True
    return False


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

    def b(self, body):
        reg = ARC().get_registry()
        return list(chain.from_iterable(
            map(lambda stmt: list(layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        self.to_free.add(str(decl.name), decl.type_)
        yield decl

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        if is_ctype(assment.expr):
            # dont free
            yield assment

    @layers.register(astlib.Return)
    def return_(self, return_):
        if return_.expr in A(astlib.Name):
            self.to_free.del_(str(return_.expr))
        yield from self.arc()
        yield return_

    @layers.register(astlib.Func)
    def func(self, func):
        self.to_free.add_scope()
        context.env.add(str(func.name), {
            "type": func.rettype
        })
        body = self.b(func.body)
        if not return_in_func(body):
            body.extend(self.arc())
        yield astlib.Func(
            func.name, func.args, func.rettype, body)
        self.to_free.del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()