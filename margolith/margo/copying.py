"""Adds __copy__ method calls where needed."""

import itertools
import sys

from . import astlib, layers, inference, defs
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


class Copying(layers.Layer):

    def copy(self, expr):
        return astlib.StructFuncCall(
            inference.infer(expr), defs.COPY_METHOD_NAME, [expr])

    def e(self, expr):
        if expr in A(astlib.Name, astlib.StructMember):
            return self.copy(expr)
        return expr

    def body(self, body):
        reg = Copying().get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    def _decl(self, decl):
        add_to_env(decl)
        yield type(decl)(decl.name, decl.type_, self.e(decl.expr))

    @layers.register(astlib.VarDecl)
    def decl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.LetDecl)
    def ldecl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(stmt.variable, stmt.op, self.e(stmt.expr))

    @layers.register(astlib.While)
    def while_(self, stmt):
        add_scope()
        yield astlib.While(
            stmt.expr, self.body(stmt.body))
        del_scope()

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield stmt

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield astlib.FuncDecl(
            stmt.name, stmt.args,
            stmt.rettype, self.body(stmt.body))
        del_scope()

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield astlib.StructFuncDecl(
            stmt.struct, stmt.func, stmt.args,
            stmt.rettype, self.body(stmt.body))
        del_scope()

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield astlib.StructDecl(
            stmt.name, stmt.var_types, self.body(stmt.body))
        del_scope()