"""Mangle names and adds prefixes."""

from . import layers, astlib, errors, defs
from .context import context

class NameSpacing(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.CType):
            return type_
        elif isinstance(type_, astlib.Ref):
            return astlib.Ref(self.type_(type_.literal))
        elif isinstance(type_, astlib.TypeName):
            return astlib.Name(defs.STRUCT_PREFIX + str(type_))
        elif isinstance(type_, astlib.Empty):
            return astlib.Empty()
        errors.not_implemented(
            "type is not supported (name spacing layer)")

    def expr(self, expr):
        if isinstance(expr, astlib.VariableName):
            return astlib.Name(defs.VAR_PREFIX + str(expr))
        elif isinstance(expr, astlib.Ref):
            return astlib.Ref(self.expr(expr.literal))
        elif isinstance(expr, astlib.Unref):
            return astlib.Unref(self.expr(expr.literal))
        elif isinstance(expr, astlib.StructElem):
            return astlib.StructElem(
                self.expr(expr.name),
                astlib.Name(defs.FIELD_PREFIX + str(expr.elem)))
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.SExpr):
            return astlib.SExpr(
                expr.op, self.expr(expr.expr1),
                self.expr(expr.expr2))
        elif isinstance(expr, (
                astlib.CIntFast8, astlib.CIntFast32,
                astlib.CUIntFast8, astlib.CUIntFast32,
                astlib.CUIntFast64, astlib.CIntFast64)):
            return expr
        elif isinstance(expr, astlib.StructScalar):
            return astlib.StructScalar(
                astlib.Name(defs.STRUCT_PREFIX + str(expr.name)))
        elif isinstance(expr, astlib.CFuncCall):
            return astlib.CFuncCall(
                expr.name, self.call_args(expr.args))
        errors.not_implemented(
            "expr is not supported (name spacing layer)")

    def call_args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.CallArgs(
            self.expr(args.arg),
            self.call_args(args.rest))

    def args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.Args(
            self.expr(args.name),
            self.type_(args.type_),
            self.args(args.rest))

    def body(self, body):
        reg = NameSpacing().get_registry()
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        return astlib.Body(
            list(layers.transform_node(body.stmt, registry=reg))[0],
            self.body(body.rest))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        name = astlib.Name(defs.VAR_PREFIX + str(decl.name))
        yield astlib.Decl(
            name, self.type_(decl.type_), self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield astlib.Assignment(
            self.expr(assignment.name), assignment.op,
            self.expr(assignment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            astlib.Name(defs.FUNC_PREFIX + str(call.name)),
            self.call_args(call.args))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield astlib.CFuncCall(
            call.name, self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    @layers.register(astlib.Func)
    def func(self, func):
        yield astlib.Func(
            astlib.Name(defs.FUNC_PREFIX + str(func.name)),
            self.args(func.args), self.type_(func.type_),
            self.body(func.body))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        yield astlib.Struct(
            astlib.Name(defs.STRUCT_PREFIX + str(struct.name)),
            self.body(struct.body))

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            astlib.Name(defs.FIELD_PREFIX + str(field.name)),
            self.type_(field.type_))
