import itertools

from . import astlib, layers, defs, inference
from .context import context, add_decl
from .utils import A


class TAC(layers.Layer):

    def __init__(self, tmp_count=0):
        self.tmp_count = tmp_count

    def new_tmp(self, expr):
        name = astlib.Name(
            "".join([defs.T_STRING, str(self.tmp_count)]))
        type_ = inference.infer_type(expr)
        result = astlib.Decl(astlib.DeclT.let, name, type_, expr)
        self.tmp_count += 1
        add_decl(result)
        return name, [result]

    def b(self, body):
        reg = TAC(tmp_count=self.tmp_count).get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                layers.transform_node(stmt, registry=reg)),
            body)))

    def inner_expr(self, expr):
        if expr in A(astlib.Expr):
            expr_, decls = self.e(expr)
            return expr_, decls
        if expr in A(astlib.Callable):
            if expr.callabletype in (
                    astlib.CallableT.fun, astlib.CallableT.struct,
                    astlib.CallableT.struct_func):
                args, decls = self.args(expr.args)
                tmp, decls_ = self.new_tmp(
                    astlib.Callable(
                        expr.callabletype, expr.parent,
                        expr.name, args))
                return tmp, decls + decls_
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                parent, decls = self.inner_expr(expr.parent)
                return astlib.DataMember(
                    astlib.DataT.struct, parent, expr.member), decls
        if expr in A(astlib.Name, astlib.Literal):
            return expr, []
        return self.new_tmp(expr)


    def e(self, expr):
        if expr in A(astlib.Expr):
            left, left_tmps = self.inner_expr(expr.left)
            right, right_tmps = self.inner_expr(expr.right)
            tmps = left_tmps + right_tmps
            return astlib.Expr(left, expr.op, right), tmps
        if expr in A(astlib.Callable):
            if expr.callabletype in (
                    astlib.CallableT.fun, astlib.CallableT.struct,
                    astlib.CallableT.struct_func):
                args, decls = self.args(expr.args)
                return astlib.Callable(
                    expr.callabletype, expr.parent,
                    expr.name, args), decls
        if (expr in A(astlib.DataMember) and
                expr.datatype == astlib.DataT.struct):
            return self.inner_expr(expr)
        return expr, []

    def args(self, args):
        if len(args) == 0:
            return []
        if isinstance(args[0], tuple):
            return []
        else:
            new_args, decls = [], []
            for arg in args:
                new_arg, decls_ = self.inner_expr(arg)
                decls.extend(decls_)
                new_args.append(new_arg)
            return new_arg, decls

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        expr, tmps = self.e(stmt.expr)
        add_decl(stmt)
        yield from tmps
        yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)
