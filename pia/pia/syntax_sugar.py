from . import astlib, layers, defs
from .utils import A


def _e(expr):
    if expr in A(astlib.Callable):
        return astlib.Callable(
            expr.callabletype, expr.parent, expr.name,
            [e(arg) for arg in expr.args])
    return expr


def _literal_to_struct_call(adr_type, py_type_name, args):
    return astlib.Callable(
        astlib.CallableT.struct, astlib.Empty(),
        astlib.DataMember(astlib.DataT.module, defs.PRELUDE, adr_type),
        [astlib.PyTypeCall(py_type_name, args)])


def unsugar_literal(literal):
    if literal.type_ == astlib.LiteralT.number:
        return _literal_to_struct_call(defs.NUMBER, defs.INT, [literal])
    elif literal.type_ == astlib.LiteralT.string:
        return _literal_to_struct_call(defs.STRING, defs.STR, [literal])
    elif literal.type_ == astlib.LiteralT.vector:
        return _literal_to_struct_call(
            defs.VECTOR, defs.LIST, [astlib.Literal(
                literal.type_, [e(lit) for lit in literal.literal])])
    return literal


def e(expr):
    if expr in A(astlib.Literal):
        return unsugar_literal(expr)
    return _e(expr)


class SyntaxSugar(layers.Layer):

    def __init__(self):
        self.b = layers.b(SyntaxSugar)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, e(stmt.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(stmt.left, stmt.op, e(stmt.right))

    @layers.register(astlib.Return)
    def return_(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_(self, stmt):
        yield e(stmt)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name, stmt.args, stmt.rettype,
            self.b(stmt.body))

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        yield astlib.StructDecl(
            stmt.name, stmt.parameters, stmt.protocols, self.b(stmt.body))

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.parameters, self.b(stmt.body))
