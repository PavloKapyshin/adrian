from . import astlib, layers, defs, utils, env_api
from .context import context
from .utils import A


def depends_on_self(expr):
    if expr in A(astlib.Name):
        return expr == defs.SELF
    elif expr in A(astlib.StructField):
        return depends_on_self(expr.struct)
    return False


def _e(expr):
    if expr in A(astlib.MethodCall):
        if not depends_on_self(expr.base) and utils.is_adt(expr.base):
            return astlib.AdtMember(
                expr.base,
                astlib.FuncCall(expr.name, [e(arg) for arg in expr.args]))
    if expr in A(astlib.Call):
        if (expr.name in A(astlib.ModuleMember) and
                expr.name.module in (defs.PY_MODULE, defs.PRELUDE)):
            expr.args = [_e(arg) for arg in expr.args]
        else:
            expr.args = [e(arg) for arg in expr.args]
        return expr
    return expr


def _literal_to_struct_call(adr_type, py_type_name, args):
    return astlib.ModuleMember(
        astlib.Name(defs.PRELUDE),
        astlib.FuncCall(
            adr_type, [astlib.PyTypeCall(py_type_name, args)]))


def unsugar_literal(literal):
    if literal.type_ == astlib.LiteralT.number:
        return _literal_to_struct_call(
            astlib.Name(defs.NUMBER), astlib.Name(defs.INT), [literal])
    elif literal.type_ == astlib.LiteralT.string:
        return _literal_to_struct_call(
            astlib.Name(defs.STRING), astlib.Name(defs.STR), [literal])
    elif literal.type_ == astlib.LiteralT.vector:
        return _literal_to_struct_call(
            astlib.Name(defs.VECTOR), astlib.Name(defs.LIST), [astlib.Literal(
                literal.type_, [e(lit) for lit in literal.literal])])
    elif literal.type_ == astlib.LiteralT.dict_:
        return _literal_to_struct_call(
            astlib.Name(defs.DICT), astlib.Name(defs.DICT), [astlib.Literal(
                literal.type_,
                {e(key): e(val) for key, val in literal.literal.items()})])
    elif literal.type_ == astlib.LiteralT.set_:
        return _literal_to_struct_call(
            astlib.Name(defs.SET), astlib.Name(defs.SET), [astlib.Literal(
                literal.type_,
                {e(key): e(val) for key, val in literal.literal.items()})])
    return literal


def e(expr):
    # TODO: support print
    if expr in A(astlib.Literal):
        return unsugar_literal(expr)
    elif expr in A(astlib.FuncCall):
        if (expr.name in A(astlib.Name) and
                expr.name in (defs.PRINT, defs.LENGTH)):
            return astlib.FuncCall(
                astlib.ModuleMember(defs.PRELUDE, expr.name),
                list(map(e, expr.args)))
    return _e(expr)


class SyntaxSugar(layers.Layer):

    def __init__(self):
        self.b = layers.b(SyntaxSugar)

    def decl(self, stmt):
        env_api.register(stmt)
        yield type(stmt)(stmt.name, stmt.type_, e(stmt.expr))

    @layers.register(astlib.VarDecl)
    def var_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.LetDecl)
    def let_decl(self, stmt):
        yield from self.decl(stmt)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(stmt.left, stmt.op, e(stmt.right))

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.MethodCall)
    def method_call(self, stmt):
        yield e(stmt)

    @layers.register(astlib.While)
    def while_(self, stmt):
        yield astlib.While(e(stmt.expr), self.b(stmt.body))

    def if_(self, stmt):
        return astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_(self, stmt):
        return astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_(self, stmt):
        return astlib.Else(self.b(stmt.body))

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        if_ = self.if_(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self.elif_(elif_))
        else_ = stmt.else_
        if else_ is not None:
            else_ = self.else_(else_)
        yield astlib.Cond(if_, elifs_, else_)

    def callable_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        env_api.register_args(stmt.args)
        yield type(stmt)(
            stmt.name, stmt.args, stmt.rettype, self.b(stmt.body))
        +context.env

    @layers.register(astlib.FuncDecl)
    def func_decl(self, stmt):
        yield from self.callable_decl(stmt)

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, stmt):
        yield from self.callable_decl(stmt)

    def data_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        yield type(stmt)(
            stmt.name, stmt.parameters, stmt.protocols, self.b(stmt.body))

    @layers.register(astlib.ExtensionDecl)
    def extension_decl(self, stmt):
        # TODO: support extensions for module structs.
        if stmt.name in A(astlib.ModuleMember):
            errors.later(errors.Version.unknown)
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
