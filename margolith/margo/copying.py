"""Make copies where needed."""

import enum

from . import layers, astlib, errors, defs
from .context import context


class NodeTypes(enum.Enum):
    variable = 1
    argument = 2


def copy(expr):
    return astlib.MethodCall(
        base=expr, method=defs.COPY_METHOD_NAME,
        args=astlib.Empty())


class Copying(layers.Layer):

    def expr(self, expr):
        if isinstance(expr, astlib.Name):
            type_ = context.ns.get(str(expr))
            if not isinstance(type_, (astlib.CType, astlib.Ref)):
                return copy(expr)
        elif isinstance(expr, astlib.Expr):
            return astlib.Expr(
                expr.op, self.expr(expr.lexpr), self.expr(expr.rexpr))
        elif isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.Unref):
            inner = expr.literal
            if isinstance(inner, astlib.StructElem):
                type_ = context.ns.get(str(inner.name))
                if isinstance(type_, astlib.Ref):
                    field_types = context.ts.get(str(type_.literal))
                else:
                    field_types = context.ts.get(str(type_))
                var_type = field_types[str(inner.elem)]
                if isinstance(var_type, astlib.Ref):
                    var_type = var_type.literal
            else:
                var_type = context.ns.get(str(inner)).literal
            if not isinstance(var_type, astlib.CType):
                return copy(inner)
        return expr

    def body(self, body, reg):
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        return astlib.Body(
            list(layers.transform_node(body.stmt, registry=reg))[0],
            self.body(body.rest, reg))

    def call_args(self, args):
        if isinstance(args, astlib.Empty):
            return astlib.Empty()
        return astlib.CallArgs(
            self.expr(args.arg),
            self.call_args(args.rest))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), decl.type_)
        yield astlib.Decl(decl.name, decl.type_, self.expr(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            assment.name, assment.op,
            self.expr(assment.expr))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            call.name, self.call_args(call.args))

    @layers.register(astlib.Instance)
    def instance(self, instance):
        yield astlib.Instance(
            instance.name, self.call_args(instance.args))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        yield astlib.MethodCall(
            call.base, call.method, self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        # Don't copy return expression.
        expr = self.expr(return_.expr)
        if isinstance(return_.expr, astlib.Name):
            expr = return_.expr
        yield astlib.Return(expr)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        reg = Copying().get_registry()
        body = self.body(struct.body, reg)
        field_types = {}
        for field_decl in body.as_list():
            field_types[str(field_decl.name)] = field_decl.type_
        context.ts.add(str(struct.name), field_types)
        yield astlib.Struct(struct.name, body)
        context.ns.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        context.ns.add_scope()
        reg = Copying().get_registry()
        for arg in method.args.as_list():
            context.ns.add(str(arg[0]), arg[1])
        yield astlib.Method(
            method.name, method.args, method.rettype,
            self.body(method.body, reg))
        context.ns.del_scope()

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        reg = Copying().get_registry()
        for arg in func.args.as_list():
            context.ns.add(str(arg[0]), arg[1])
        yield astlib.Func(
            func.name, func.args, func.rettype, self.body(func.body, reg))
        context.ns.del_scope()