"""Adds __copy__ method calls and allocates memory."""

import itertools
import sys

from . import astlib, layers, inference, defs
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


class Copying(layers.Layer):

    def deref(self, expr):
        if expr in A(astlib.Expr):
            return astlib.Expr(
                expr.op, self.deref(expr.left_expr),
                self.deref(expr.right_expr))
        if expr in A(astlib.Name, astlib.StructMember):
            return astlib.Deref(expr)
        return expr

    def allocate_on_heap(self, expr):
        type_ = inference.infer(expr)
        allocation_expr = astlib.CFuncCall(
            "malloc", [astlib.CFuncCall(
                "sizeof", [astlib.StructScalar(type_)])])
        assignment_expr = self.deref(expr)
        return allocation_expr, assignment_expr

    def copy(self, expr):
        return astlib.StructFuncCall(
            inference.infer(expr), defs.COPY_METHOD_NAME,
            [expr]), None

    def e(self, expr):
        if expr in A(astlib.CTYPES, astlib.Expr):
            return self.allocate_on_heap(expr)
        if expr in A(astlib.Name, astlib.StructMember):
            type_ = inference.infer(expr)
            if type_ in A(astlib.CType):
                return self.allocate_on_heap(expr)
            return self.copy(expr)
        return expr, None

    def body(self, body):
        reg = Copying().get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    def _decl(self, decl):
        expr, additional_expr = self.e(decl.expr)
        add_to_env(decl)
        yield type(decl)(decl.name, decl.type_, expr)
        if additional_expr:
            yield astlib.Assignment(
                self.deref(decl.name),
                "=", additional_expr)

    @layers.register(astlib.VarDecl)
    def decl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.LetDecl)
    def ldecl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        type_ = inference.infer(stmt.variable)
        if type_ in A(astlib.Name):
            yield stmt
        else:
            yield astlib.Assignment(
                self.deref(stmt.variable),
                "=", self.deref(stmt.expr))

    @layers.register(astlib.AssignmentAndAlloc)
    def assignment_and_alloc(self, stmt):
        expr, additional_expr = self.e(stmt.expr)
        yield astlib.Assignment(stmt.name, "=", expr)
        if additional_expr:
            yield astlib.Assignment(
                self.deref(stmt.name), "=", additional_expr)

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