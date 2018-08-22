from . import astlib, layers, defs
from .context import context
from .utils import A


def _literal_to_struct_call(adr_type, py_type_name, args):
    return astlib.ModuleMember(
        astlib.Name(defs.MODULE_PRELUDE),
        astlib.FuncCall(
            adr_type, [astlib.PyTypeCall(py_type_name, args)]))


def desugar_literal(literal):
    if literal.type_ == astlib.LiteralT.number:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_NUMBER), astlib.Name(defs.TYPE_INT), [literal])
    elif literal.type_ == astlib.LiteralT.string:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_STRING), astlib.Name(defs.TYPE_STR), [literal])
    elif literal.type_ == astlib.LiteralT.vector:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_VECTOR), astlib.Name(defs.TYPE_LIST), [astlib.Literal(
                literal.type_, [e(lit) for lit in literal.literal])])
    elif literal.type_ == astlib.LiteralT.dict_:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_DICT), astlib.Name(defs.TYPE_DICT), [astlib.Literal(
                literal.type_,
                {e(key): e(val) for key, val in literal.literal.items()})])
    elif literal.type_ == astlib.LiteralT.set_:
        return _literal_to_struct_call(
            astlib.Name(defs.TYPE_SET), astlib.Name(defs.TYPE_SET), [astlib.Literal(
                literal.type_,
                {e(key): e(val) for key, val in literal.literal.items()})])
    return literal


def _e(expr):
    if expr in A(astlib.FuncCall):
        if (expr.name in A(astlib.ModuleMember) and
                expr.name.module in (defs.MODULE_PRELUDE, defs.MODULE_PY)):
            return astlib.FuncCall(expr.name, [_e(arg) for arg in expr.args])
        else:
            return astlib.FuncCall(expr.name, [e(arg) for arg in expr.args])
    elif expr in A(astlib.Name):
        if expr in defs.PRELUDE_OBJS:
            return astlib.ModuleMember(defs.MODULE_PRELUDE, expr)
    elif expr in A(astlib.StructPath):
        return astlib.StructPath([_e(elem) for elem in expr.path])
    return expr


def e(expr):
    if expr in A(astlib.Literal):
        return desugar_literal(expr)
    elif expr in A(astlib.FuncCall):
        if expr.name in A(astlib.Name) and expr.name in defs.PRELUDE_OBJS:
            return astlib.FuncCall(
                astlib.ModuleMember(defs.MODULE_PRELUDE, expr.name),
                list(map(e, expr.args)))
    elif expr in A(astlib.Subscript):
        return astlib.Subscript(e(expr.base), e(expr.index))
    elif expr in A(astlib.Expr):
        return astlib.Expr(e(expr.left), expr.op, e(expr.right))
    elif expr in A(astlib.StructPath):
        return astlib.StructPath([e(elem) for elem in expr.path])
    return _e(expr)


class Desugar(layers.Layer):

    def __init__(self):
        self.b = layers.b(Desugar)

    def declaration(self, decl):
        return type(decl)(decl.name, decl.type_, e(decl.expr))

    def struct_like_decl(self, decl):
        return type(decl)(
            decl.name, decl.parameters, decl.implemented_protocols,
            self.b(decl.body))

    @layers.register(astlib.LetDecl)
    def let_declaration(self, decl):
        yield self.declaration(decl)

    @layers.register(astlib.VarDecl)
    def var_declaration(self, decl):
        yield self.declaration(decl)

    @layers.register(astlib.StructDecl)
    def struct_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ExtensionDecl)
    def extension_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.ProtocolDecl)
    def protocol_declaration(self, decl):
        yield self.struct_like_decl(decl)

    @layers.register(astlib.FuncProtoDecl)
    def func_proto_declaration(self, decl):
        yield astlib.FuncProtoDecl(decl.name, decl.args, decl.rettype)

    @layers.register(astlib.FuncDecl)
    def func_declaration(self, decl):
        yield astlib.FuncDecl(
            decl.name, decl.args, decl.rettype, self.b(decl.body))

    @layers.register(astlib.MethodDecl)
    def method_declaration(self, decl):
        yield astlib.MethodDecl(
            decl.name, decl.args, decl.rettype, self.b(decl.body))

    @layers.register(astlib.FieldDecl)
    def field_declaration(self, decl):
        yield astlib.FieldDecl(decl.name, decl.type_)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            e(stmt.left), stmt.op, e(stmt.right))

    @layers.register(astlib.Cond)
    def cond(self, stmt):
        yield astlib.Cond(
            self.if_stmt(stmt.if_stmt),
            [self.elif_stmt(elif_) for elif_ in stmt.elifs],
            (None if stmt.else_stmt is None else self.else_stmt(stmt.else_stmt)))

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        yield astlib.While(e(stmt.expr), self.b(stmt.body))

    @layers.register(astlib.For)
    def for_stmt(self, stmt):
        yield astlib.For(
            stmt.names, e(stmt.container), self.b(stmt.body))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(e(stmt.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield e(call)

    @layers.register(astlib.StructPath)
    def struct_path(self, struct_path):
        yield e(struct_path)

    def if_stmt(self, stmt):
        return astlib.If(e(stmt.expr), self.b(stmt.body))

    def elif_stmt(self, stmt):
        return astlib.Elif(e(stmt.expr), self.b(stmt.body))

    def else_stmt(self, stmt):
        return astlib.Else(self.b(stmt.body))
