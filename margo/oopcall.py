"""Translates method calls into function calls."""

from . import layers, astlib, errors, defs
from .context import context


class OOPCall(layers.Layer):

    def expr(self, expr):
        if isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.SExpr):
            return astlib.SExpr(
                op=expr.op, expr1=self.expr(expr.expr1),
                expr2=self.expr(expr.expr2))
        elif isinstance(expr, astlib.VariableName):
            type_ = context.ns.get(str(expr))["type_"]
            if isinstance(type_, astlib.CType):
                return expr
            return astlib.FuncCall(
                astlib.FunctionName(
                    "".join([defs.COPY_METHOD_NAME, str(type_)])),
                args=astlib.CallArgs(expr, astlib.Empty()))
        return expr

    def body(self, body, registry):
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        return astlib.Body(
            list(layers.transform_node(body.stmt, registry=registry))[0],
            self.body(body.rest, registry))

    def call_args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.CallArgs(
            self.expr(args.arg),
            self.call_args(args.rest))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), {"type_": decl.type_})
        yield astlib.Decl(decl.name, decl.type_, self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield astlib.Assignment(
            assignment.name, assignment.op,
            self.expr(assignment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            call.name, self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        registry = OOPCall().get_registry()
        yield astlib.Struct(
            struct.name, self.body(struct.body, registry))

    @layers.register(astlib.Func)
    def func(self, func):
        registry = OOPCall().get_registry()
        yield astlib.Func(
            func.name, args=func.args,
            type_=func.type_, body=self.body(func.body, registry))

    @layers.register(astlib.Method)
    def method(self, method):
        registry = OOPCall().get_registry()
        yield astlib.Method(
            method.name, args=method.args,
            type_=method.type_, body=self.body(method.body, registry))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        struct = context.ns.get(str(call.struct))["type_"]
        yield astlib.FuncCall(
            astlib.FunctionName(
                "".join([str(call.method), str(struct)])),
            self.call_args(call.args))
