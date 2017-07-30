"""Translates method calls into function calls."""

from . import layers, astlib, errors, defs
from .context import context


class MethodCallsToFuncCalls(layers.Layer):

    def get_base_type(self, base):
        if isinstance(base, astlib.Name):
            return context.ns.get(str(base))
        elif isinstance(base, astlib.StructElem):
            name_type = context.ns.get(str(base.name))
            return context.ts.get(str(name_type))[str(base.elem)]
        elif isinstance(base, astlib.Unref):
            return self.get_base_type(base.literal)
        errors.not_implemented("can't get_base_type (mcall->fcall)")

    def expr(self, expr):
        if isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.Instance):
            return list(self.instance(expr))[0]
        elif isinstance(expr, astlib.Expr):
            return astlib.Expr(
                expr.op, self.expr(expr.lexpr), self.expr(expr.rexpr))
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

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            assment.var, assment.op,
            self.expr(assment.expr))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        base_type = self.get_base_type(call.base)
        if isinstance(base_type, astlib.Ref):
            base_type = base_type.literal
        args = self.call_args(call.args)
        if str(call.method) != defs.INIT_METHOD_NAME:
            args = astlib.CallArgs(call.base, args)
        yield astlib.FuncCall(
            astlib.Name("".join([str(base_type), str(call.method)])),
            args)

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            call.name, self.call_args(call.args))

    @layers.register(astlib.Instance)
    def instance(self, instance):
        base_type = instance.name
        args = self.call_args(instance.args)
        yield astlib.FuncCall(
            astlib.Name(
                "".join([str(base_type), str(defs.INIT_METHOD_NAME)])),
            args)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        reg = MethodCallsToFuncCalls().get_registry()
        body = self.body(struct.body, reg)
        field_types = {}
        for field_decl in body.as_list():
            field_types[str(field_decl.name)] = field_decl.type_
        context.ts.add(str(struct.name), field_types)
        yield astlib.Struct(struct.name, body)
        context.ns.del_scope()

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        reg = MethodCallsToFuncCalls().get_registry()
        for arg in func.args.as_list():
            context.ns.add(str(arg[0]), arg[1])
        yield astlib.Func(
            func.name, args=func.args,
            rettype=func.rettype, body=self.body(func.body, reg))
        context.ns.del_scope()

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), decl.type_)
        yield astlib.Decl(decl.name, decl.type_, self.expr(decl.expr))