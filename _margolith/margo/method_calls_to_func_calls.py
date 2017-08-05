"""Translates method calls into function calls."""

import sys

from . import layers, astlib, errors, defs
from .context import context


class MethodCallsToFuncCalls(layers.Layer):

    def get_base_type(self, base):
        if isinstance(base, astlib.Name):
            return context.ns.get(str(base))["type_"]
        elif isinstance(base, astlib.StructElem):
            name_info = context.ns.get(str(base.name))
            name_type = name_info["type_"]
            if isinstance(name_type, astlib.ParamedType):
                name_type = name_type.base
            result = context.ts.get(str(name_type))["fields"][str(base.elem)]
            if defs.VAR_NAME_REGEX.fullmatch(str(result)):
                result = name_info["params"][str(result)]
            return result
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
        return [list(layers.transform_node(stmt, registry=registry))[0]
                for stmt in body]

    def call_args(self, args):
        return [self.expr(arg) for arg in args]

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            assment.var, assment.op,
            self.expr(assment.expr))

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        base_type = self.get_base_type(call.base)
        if isinstance(base_type, astlib.CType):
            pass
        else:
            if isinstance(base_type, astlib.Ref):
                base_type = base_type.literal
            elif isinstance(base_type, astlib.ParamedType):
                base_type = base_type.base
            args = self.call_args(call.args)
            if str(call.method) != defs.INIT_METHOD_NAME:
                args = [call.base] + args
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
        field_types = {}
        for field_decl in struct.body:
            field_types[str(field_decl.name)] = field_decl.type_
        context.ts.add(str(struct.name), {
            "fields": field_types,
            "param_types": struct.param_types
        })
        reg = MethodCallsToFuncCalls().get_registry()
        body = self.body(struct.body, reg)
        yield astlib.Struct(struct.name, struct.param_types, body)
        context.ns.del_scope()

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        reg = MethodCallsToFuncCalls().get_registry()
        for arg in func.args:
            context.ns.add(str(arg.name), {
                "type_": arg.type_,
                "params": self.get_params(arg.type_),
            })
        yield astlib.Func(
            func.name, args=func.args,
            rettype=func.rettype, body=self.body(func.body, reg))
        context.ns.del_scope()

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.expr(return_.expr))

    def get_params(self, type_):
        params = None
        if isinstance(type_, astlib.ParamedType):
            params = {}
            index = 0
            for param_type in context.ts.get(str(type_.base))["param_types"]:
                params[str(param_type)] = type_.params[index]
                index += 1
        return params

    @layers.register(astlib.Decl)
    def decl(self, decl):
        params = self.get_params(decl.type_)
        context.ns.add(str(decl.name), {
            "type_": decl.type_,
            "params": params
        })
        yield astlib.Decl(decl.name, decl.type_, self.expr(decl.expr))