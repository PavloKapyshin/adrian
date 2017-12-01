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
                astlib.StructFuncCall, astlib.Expr):
            expr_, decls_ = self.e(expr)
            tmp, decls = self.new_tmp(expr_)
            return tmp, decls_ + decls
        if expr in A(astlib.Name, astlib.Ref):
            return expr, []
        return self.new_tmp(expr)

    def _e(self, expr):
        if expr in A(astlib.CTYPES):
            return self.new_tmp(expr)
        return self.e(expr)

    def e(self, expr):
        if expr in A(astlib.Expr):
            left_expr, left_decls = self._e(expr.left_expr)
            right_expr, right_decls = self._e(expr.right_expr)
            return astlib.Expr(
                expr.op, left_expr, right_expr), left_decls + right_decls
        if expr in A(astlib.FuncCall, astlib.StructCall):
            args, decls = self.call_args(expr.args)
            return type(expr)(expr.name, args), decls
        if expr in A(astlib.StructFuncCall):
            args, decls = self.call_args(expr.args)
            return astlib.StructFuncCall(
                expr.struct, expr.func_name, args), decls
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

    @layers.register(astlib.AssignmentAndAlloc)
    def assignment_and_alloc(self, stmt):
        expr, decls = self.e(stmt.expr)
        yield from decls
        yield astlib.AssignmentAndAlloc(
            stmt.name, stmt.type_, expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr, decls = self.e(stmt.expr)
        yield from decls
        yield astlib.Assignment(
            stmt.variable, stmt.op, expr)

    @layers.register(astlib.Return)
    def return_(self, stmt):
        expr, decls = self.e(stmt.expr)
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
