from .context import context, add_decl, add_data_decl
from . import layers, astlib, errors, defs, inference
from .utils import A, is_struct


class Analyzer(layers.Layer):

    def args(self, args):
        if args in A(astlib.Empty):
            return []
        if args in A(astlib.Args):
            pass
        return [self.e(arg) for arg in args.as_list()]

    def t(self, type_):
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                if not str(type_.parent) == defs.CMODULE:
                    errors.not_now(errors.MODULE)
                return type_
        return type_

    def e_callable(self, expr):
        if is_struct(expr.name):
            return astlib.Callable(
                astlib.CallableT.struct, expr.parent,
                expr.name, self.args(expr.args))
        return astlib.Callable(
            expr.callabletype, expr.parent,
            expr.name, self.args(expr.args))

    def e_struct_member(self, expr):
        if expr.member in A(astlib.Callable):
            func_call = expr.member
            parent = self.e(expr.parent)
            return astlib.Callable(
                astlib.CallableT.struct_func,
                inference.infer_type(parent),
                func_call.name,
                [parent] + self.args(func_call.args))
        if expr.member in A(astlib.Name):
            return astlib.DataMember(
                expr.datatype, self.e(expr.parent),
                expr.member)

    def e(self, expr):
        if expr in A(astlib.Callable):
            return self.e_callable(expr)
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return self.e_struct_member(expr)
        if expr in A(astlib.Expr):
            return astlib.Expr(
                self.e(expr.left),
                expr.op,
                self.e(expr.right))
        return expr

    def b(self, body):
        reg = Analyzer().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body.as_list()))

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield stmt
        else:
            type_ = stmt.type_
            expr = self.e(stmt.expr)
            if not type_:
                type_ = inference.infer_type(expr)
            else:
                type_ = self.t(type_)
            result = astlib.Decl(stmt.decltype, stmt.name, type_, expr)
            add_decl(result)
            yield result

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        add_data_decl(stmt)
        +context.env
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, self.b(stmt.body))
        -context.env
