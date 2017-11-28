import itertools
import sys

from . import layers, astlib, errors, env
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


def is_ctype(type_):
    return type_ in A(astlib.CType)


def return_in_func(body):
    for stmt in body:
        if stmt in A(astlib.Return):
            return True
    return False


class ARC(layers.Layer):

    def __init__(self, to_free=None, to_not_free=None):
        self.to_free = to_free or env.Env()
        self.to_not_free = to_not_free or env.Env()

    def analyze_expr(self, decl):
        if decl.expr in A(astlib.Name):
            return str(decl.expr)
        return None

    def init_ref(self, decl):
        if decl in A(astlib.VarDecl, astlib.LetDecl):
            self.to_free.add(str(decl.name), self.analyze_expr(decl))

    def free(self, name):
        return "free --> " + name

    def dont_free(self, name):
        self.to_not_free.add(str(name), None)

    def really_need_to_free(self, name, val):
        if self.to_not_free.exists(name):
            return False
        while val is not None:
            if self.to_not_free.exists(val):
                return False
            name = val
            val = self.to_free.get(name)
        return True

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, val in sorted(space[scope].copy().items()):
            if self.really_need_to_free(key, val):
                yield self.free(key)

    def body(self, body):
        reg = ARC(to_free=self.to_free, to_not_free=self.to_not_free).get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.VarDecl)
    @layers.register(astlib.LetDecl)
    def var_decl(self, decl):
        add_to_env(decl)
        self.init_ref(decl)
        yield decl

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        # Bug: no free before assignment.
        yield stmt

    # @layers.register(astlib.LetDecl)
    # def let_decl(self, decl):
    #     add_to_env(decl)
    #     self.init_ref(decl)
    #     yield decl

    @layers.register(astlib.FuncDecl)
    @layers.register(astlib.StructFuncDecl)
    def func(self, decl):
        self.to_free.add_scope()
        self.to_not_free.add_scope()
        add_to_env(decl)
        add_scope()

        body = self.body(decl.body)
        if not return_in_func(body):
            body.extend(self.arc())
        if decl in A(astlib.FuncDecl):
            yield astlib.FuncDecl(
                decl.name, decl.args, decl.rettype, body)
        else:
            yield astlib.StructFuncDecl(
                decl.struct, decl.func, decl.args, decl.rettype, body)

        del_scope()
        self.to_free.del_scope()
        self.to_not_free.del_scope()

    @layers.register(astlib.Return)
    def return_(self, stmt):
        if stmt.expr in A(astlib.Name):
            self.dont_free(stmt.expr)
            # Bug: can free a variable that refs to stmt.expr variable.
            # Example:
            #    ...
            #    var a = c#IntFast8(0)
            #    var b = ref a
            # -> free(b)
            #    return a
        yield from self.arc()
        yield stmt

    @layers.register(astlib.StructDecl)
    def struct(self, decl):
        add_to_env(decl)
        add_scope()
        yield astlib.StructDecl(
            decl.name, decl.var_types, self.body(decl.body))
        del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()