import sys
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

    def __init__(self, to_free=None):
        self.to_free = to_free or env.Env()

    def free(self, name, val):
        if val["type"] in A(astlib.CType):
            return astlib.CFuncCall("free", [astlib.Name(name, is_tmp=val["is_tmp"])])
        if val["type"] in A(astlib.Name):
            return astlib.MethodCall(
                base=astlib.Name(name, is_tmp=val["is_tmp"]),
                method=astlib.Name(defs.DEINIT_METHOD_NAME),
                args=[])

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, val in sorted(space[scope].copy().items()):
            yield self.free(key, val)

    def b(self, body):
        reg = ARC(to_free=self.to_free).get_registry()
        return list(chain.from_iterable(
            map(lambda stmt: list(layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        self.to_free.add(str(decl.name), {
            "type": decl.type_,
            "is_tmp": decl.name.is_tmp
        })
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

    @layers.register(astlib.Method)
    def method(self, method):
        self.to_free.add_scope()
        context.env.add(str(method.name), {
            "type": method.rettype
        })
        body = self.b(method.body)
        if not return_in_func(body):
            body.extend(self.arc())
        yield astlib.Method(
            method.name, method.args, method.rettype, body)
        self.to_free.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        self.to_free.add_scope()
        context.env.add(str(struct.name), {
            "type": struct.name
        })
        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            self.b(struct.body))
        self.to_free.del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()