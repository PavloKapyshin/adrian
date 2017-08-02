"""Mangle names and adds prefixes."""

from . import layers, astlib, errors, defs
from .context import context


class NameSpacing(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, (astlib.CType, astlib.Empty, astlib.CPtr)):
            return type_
        elif isinstance(type_, astlib.Ref):
            return astlib.Ref(self.type_(type_.literal))
        elif isinstance(type_, astlib.Name):
            return self.name(type_)
        elif isinstance(type_, astlib.ParamedType):
            return self.name(type_.base)
        errors.not_implemented(
            "type is not supported (name spacing)")

    def expr(self, expr):
        if isinstance(expr, astlib.Name):
            return self.name(expr)
        elif isinstance(expr, astlib.Ref):
            return astlib.Ref(self.expr(expr.literal))
        elif isinstance(expr, astlib.Unref):
            return astlib.Unref(self.expr(expr.literal))
        elif isinstance(expr, astlib.StructElem):
            return astlib.StructElem(
                self.expr(expr.name),
                self.name(expr.elem))
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.Expr):
            return astlib.Expr(
                expr.op, self.expr(expr.lexpr),
                self.expr(expr.rexpr))
        elif isinstance(expr, astlib.CINT_TYPES):
            return expr
        elif isinstance(expr, astlib.StructScalar):
            return astlib.StructScalar(self.name(expr.name))
        elif isinstance(expr, astlib.CFuncCall):
            return astlib.CFuncCall(
                expr.name, self.call_args(expr.args))
        elif isinstance(expr, str):
            # LOL
            return self.name(expr)
        elif isinstance(expr, astlib.CCast):
            return astlib.CCast(self.expr(expr.expr), to=self.type_(expr.to))
        errors.not_implemented(
            "expr is not supported (name spacing)")

    def name(self, name):
        string = "_".join([
            defs.ADR_PREFIX, context.file_hash, str(name)])
        return astlib.Name(string)

    def call_args(self, args):
        return [self.expr(arg) for arg in args]

    def args(self, args):
        return [astlib.Arg(self.expr(arg.name), self.type_(arg.type_))
                for arg in args]

    def body(self, body):
        reg = NameSpacing().get_registry()
        return [list(layers.transform_node(stmt, registry=reg))[0]
                for stmt in body]

    @layers.register(astlib.Decl)
    def decl(self, decl):
        name = self.name(decl.name)
        yield astlib.Decl(
            name, self.type_(decl.type_), self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            self.expr(assment.var), assment.op,
            self.expr(assment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            self.name(call.name),
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
            self.name(func.name), self.args(func.args),
            self.type_(func.rettype), self.body(func.body))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        yield astlib.Struct(
            self.name(struct.name), struct.param_types, self.body(struct.body))

    @layers.register(astlib.Field)
    def field(self, field):
        yield astlib.Field(
            self.name(field.name), self.type_(field.type_))