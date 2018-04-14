from . import astlib, inference, layers, env_api
from .context import context
from .utils import A


class TypeInference(layers.Layer):

    def __init__(self):
        self.b = layers.b(TypeInference)

    def a(self, args):
        if not args:
            return []
        elif isinstance(args[0], tuple):
            return args
        return [self.e(arg) for arg in args]

    def annotate_struct_field(self, data_member):
        if data_member in A(astlib.DataMember):
            type_ = inference.infer_general_type(data_member)
            return astlib.AnnotatedStructField(
                self.annotate_struct_field(data_member.parent),
                astlib.AnnotatedName(
                    str(data_member.member), type_), type_)
        return astlib.AnnotatedName(
            str(data_member), inference.infer_general_type(data_member))

    def e(self, expr):
        if expr in A(astlib.Name):
            return astlib.AnnotatedName(
                str(expr), inference.infer_general_type(expr))
        elif expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return self.annotate_struct_field(expr)
        return expr

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        env_api.register(stmt)
        expr = astlib.Empty()
        if stmt.decltype != astlib.DeclT.field:
            expr = self.e(stmt.expr)
        yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        env_api.register(stmt)
        left, right = self.e(stmt.left), self.e(stmt.right)
        yield astlib.Assignment(left, stmt.op, right)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        env_api.register(stmt)
        +context.env
        env_api.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent,
            astlib.AnnotatedName(stmt.name, stmt.rettype), self.a(stmt.args),
            stmt.rettype, self.b(stmt.body))
        -context.env

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        +context.env
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        env_api.register(stmt)
        context.parent = stmt.name
        +context.env
        yield astlib.StructDecl(
            stmt.name, stmt.params, stmt.protocols, self.b(stmt.body))
        -context.env
