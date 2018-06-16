"""Performs type inference and context-sensitive analysis."""

from . import astlib, layers, inference, env_api, utils, defs
from .context import context
from .utils import A


def make_py_call(call):
    if call.name in A(astlib.PyType):
        return astlib.PyTypeCall(call.name.type_, a(call.args))
    return astlib.PyFuncCall(call.name.name, a(call.args))


def e_call(expr):
    if expr.name in A(astlib.PyObject):
        return make_py_call(expr)
    call_args = a(expr.args)
    if expr in A(astlib.FuncCall):
        if expr.name == defs.REF:
            return astlib.Ref(call_args[0])
        elif utils.is_type(expr.name):
            return astlib.StructFuncCall(
                expr.name, defs.INIT_METHOD, call_args)
        return astlib.FuncCall(expr.name, call_args)
    return astlib.StructFuncCall(
        inference.infer_type_in_decl(expr.base), expr.name, [expr.base] + call_args)


def e_struct_field(expr):
    if expr.struct in A(astlib.Name):
        if utils.is_adt(expr.struct):
            return astlib.AdtMember(expr.struct, e(expr.field))
    return astlib.StructField(e(expr.struct), expr.field)


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


def ass_e(expr):
    if expr in A(astlib.Subscript):
        base = e(expr.base)
        sub = e(expr.sub)
        type_ = inference.infer_type_in_decl(base)
        if type_ in A(astlib.PyObject):
            return expr
        return astlib.StructFuncCall(type_, defs.SETITEM, [base, sub])
    return e(expr)

def e(expr):
    if expr in A(astlib.Not):
        expr_ = e(expr.expr)
        return astlib.StructFuncCall(
            inference.infer_type_in_decl(expr_), defs.NOT_METHOD, [expr_])
    elif expr in A(astlib.Name):
        if expr in (defs.TRUE, defs.FALSE):
            return astlib.PyConstant(str(expr))
    elif expr in A(astlib.Call):
        return e_call(expr)
    elif expr in A(astlib.StructField):
        return e_struct_field(expr)
    elif expr in A(astlib.Expr):
        left = e(expr.left)
        if expr.op == defs.IS:
            return astlib.Is(left, t(expr.right))
        right = e(expr.right)
        return astlib.StructFuncCall(
            inference.infer_type_in_decl(left), defs.OP_TO_METHOD[expr.op],
            [left, right])
    elif expr in A(astlib.Literal):
        if expr.type_ == astlib.LiteralT.vector:
            return astlib.Literal(expr.type_, a(expr.literal))
        elif expr.type_ == astlib.LiteralT.set_:
            return astlib.Literal(expr.type_, {e(elem) for elem in expr.literal})
        elif expr.type_ == astlib.LiteralT.dict_:
            return astlib.Literal(
                expr.type_, {e(key): e(val) for key, val in expr.literal.items()})
    elif expr in A(astlib.AdtMember):
        return astlib.AdtMember(e(expr.base), e(expr.member))
    elif expr in A(astlib.Subscript):
        base = e(expr.base)
        sub = e(expr.sub)
        type_ = inference.infer_type_in_decl(base)
        if type_ in A(astlib.PyObject):
            return expr
        return astlib.StructFuncCall(type_, defs.GETITEM, [base, sub])
    return expr


def a(args):
    return [e(arg) for arg in args]


def decl_a(args):
    return [(name, t(type_)) for name, type_ in args]


def infer(type_, expr):
    if type_ in A(astlib.Empty):
        return inference.infer_type_in_decl(expr)
    return t(type_)


class Analyzer(layers.Layer):

    def __init__(self):
        self.b = layers.b(Analyzer)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        left, right = ass_e(stmt.left), e(stmt.right)
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
        expr = e(stmt.expr)
        type_ = infer(stmt.type_, expr)
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

    @layers.register(astlib.ExtensionDecl)
    def extension_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.AdtDecl)
    def adt_decl(self, stmt):
        yield from self.data_decl(stmt)

    @layers.register(astlib.ProtocolDecl)
    def protocol_decl(self, stmt):
        yield from self.data_decl(stmt)
