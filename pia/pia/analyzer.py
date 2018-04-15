"""Performs type inference and context-sensitive analysis."""

from . import astlib, layers, inference, env_api
from .context import context
from .utils import A


def t(type_):
    if type_ in A(astlib.Name):
        if type_ == defs.VOID:
            return astlib.Void()
        elif type_ == defs.BOOL:
            return astlib.PyType(defs.BOOL)
    elif type_ in A(astlib.GenericType):
        return astlib.GenericType(
            t(type_.base), [t(param) for param in type_.params])
    return type_


def e(expr):
    if expr in A(astlib.Not):
        expr_ = e(expr.expr)
        return astlib.StructFuncCall(
            inference.infer_type(expr_), defs.NOT_METHOD, [expr_])
    # TODO
    pass


class Analyzer(layers.Layer):

    def __init__(self):
        self.b = layers.b(Analyzer)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        left, right = e(stmt.left), e(stmt.right)
        env_api.register(stmt, right=right)
        yield astlib.Assignment(left, stmt.op, right)

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.MethodCall)
    def method_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.While)
    def while_(self, stmt):
        +context.env
        yield astlib.While(e(stmt.expr), self.b(stmt.body))
        -context.env

    def if_stmt(self, stmt):
        +context.env
        result = astlib.If(e(stmt.expr), self.b(stmt.body))
        -context.env
        return result

    def elif_stmt(self, stmt):
        +context.env
        result = astlib.Elif(e(stmt.expr), self.b(stmt.body))
        -context.env
        return result

    def else_stmt(self, stmt):
        +context.env
        result = astlib.Else(self.b(stmt.body))
        -context.env
        return result

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        if_stmt = self.if_stmt(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self.elif_stmt(elif_))
        else_stmt = stmt.else_
        if else_stmt is not None:
            else_stmt = self.else_stmt(else_stmt)
        yield astlib.Cond(if_stmt, elifs_, else_stmt)

    def decl(self, stmt):
        type_, expr = t(stmt.type_), e(stmt.expr)
        env_api.register(stmt, type_=type_, expr=expr)
        yield type(stmt)(stmt.name, type_, expr)

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.FieldDecl)
    def field_decl(self, stmt):
        type_ = t(stmt.type_)
        env_api.register(stmt, type_=type_)
        yield astlib.FieldDecl(stmt.name, type_)

    def callable_decl(self, stmt):
        args, rettype = decl_a(stmt.args), t(stmt.rettype)
        env_api.register(stmt, args=args, type_=rettype)
        +context.env
        env_api.register_args(args)
        yield type(stmt)(stmt.name, args, rettype, self.b(stmt.body))
        -context.env

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        yield from self.callable_decl(stmt)

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        yield from self.callable_decl(stmt)

    def data_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        context.parent = stmt.name
        env_api.register_params(stmt.parameters)
        yield type(stmt)(
            stmt.name, stmt.parameters, stmt.protocols, self.b(stmt.body))
        -context.env

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AdtDecl)
    def adt_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.ProtocolDecl)
    def protocol_decl(self, stmt):
        yield from self.data_decl(stmt)
