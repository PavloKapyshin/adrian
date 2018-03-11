"""
Translates expressions that are composed of several smaller ones to
three address code. Because every variable must be freed. For example, we
created a variable like this:
    `var lol: c#IntFast32 = c#IntFast32(0) + c#IntFast32(1)`,
there are no variables that point to `c#IntFast32(0)` or `c#IntFast32(1)`.
Also these transformations will help us to improve output code.
"""

import itertools

from . import astlib, layers, defs, inference
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


class TAC(layers.Layer):

    def new_tmp(self, expr):
        tmp_name = astlib.Name(
            "".join([defs.T_STRING, str(context.tmp_count)]), is_tmp=True)
        type_ = inference.infer(expr)
        context.tmp_count += 1
        node = astlib.LetDecl(tmp_name, type_, expr)
        add_to_env(node)
        return tmp_name, [node]

    def call_args(self, args_):
        args, decls = [], []
        for arg in args_:
            arg_, decls_ = self.inner_expr(arg)
            decls.extend(decls_)
            args.append(arg_)
        return args, decls

    def inner_expr(self, expr):
        if expr in A(
                astlib.FuncCall, astlib.StructCall,
                astlib.StructFuncCall):
            expr_, decls_ = self.e(expr)
            tmp, decls = self.new_tmp(expr_)
            return tmp, decls_ + decls
        if expr in A(astlib.Name, astlib.Ref, astlib.IntLiteral):
            return expr, []
        return self.new_tmp(expr)

    def _e(self, expr):
        if expr not in A(astlib.IntLiteral):
            return self.new_tmp(expr)
        return self.e(expr)

    def e(self, expr):
        if expr in A(astlib.FuncCall, astlib.StructCall):
            args, decls = self.call_args(expr.args)
            return type(expr)(expr.name, args), decls
        if expr in A(astlib.StructFuncCall):
            args, decls = self.call_args(expr.args)
            return astlib.StructFuncCall(
                expr.struct, expr.func_name, args), decls
        if expr in A(astlib.StructMember):
            if not expr.struct in A(astlib.Name):
                expr_, decls = self.inner_expr(expr.struct)
                return astlib.StructMember(expr_, expr.member), decls
        return expr, []

    def body(self, body):
        reg = TAC().get_registry()
        return list(itertools.chain.from_iterable(
            map(
                lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    def _decl(self, decl):
        expr, tmp_decls = self.e(decl.expr)
        add_to_env(decl)
        yield from tmp_decls
        yield type(decl)(decl.name, decl.type_, expr)

    @layers.register(astlib.VarDecl)
    def decl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.LetDecl)
    def ldecl(self, decl):
        yield from self._decl(decl)

    @layers.register(astlib.While)
    def while_(self, stmt):
        expr, decls = self.inner_expr(stmt.expr)
        add_scope()
        yield from decls
        yield astlib.While(
            expr, self.body(stmt.body))
        del_scope()

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr, decls = self.e(stmt.expr)
        yield from decls
        yield astlib.Assignment(
            stmt.variable, stmt.op, expr)

    @layers.register(astlib.Return)
    def return_(self, stmt):
        expr, decls = self.inner_expr(stmt.expr)
        yield from decls
        yield astlib.Return(expr)

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield astlib.FuncDecl(
            stmt.name, stmt.args, stmt.rettype, self.body(stmt.body))
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

    @layers.register(astlib.ADTDecl)
    def adt_decl(self, stmt):
        add_to_env(stmt)
        add_scope()
        yield stmt
        del_scope()
