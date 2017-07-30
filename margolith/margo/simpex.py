"""Translates expressions into more simple."""

import sys

from . import layers, astlib, errors
from .context import context


def add_to_call_args(args, arg):
    if isinstance(args, astlib.Empty):
        return astlib.CallArgs(arg, astlib.Empty())
    args.append(arg)
    return args


class SimpEx(layers.Layer):
    tmp_string = "__tmp"

    def __init__(self):
        self.tmp_count = 0

    def get_func_rettype(self, call):
        if isinstance(call, astlib.FuncCall):
            return context.fs.get(str(call.name))
        elif isinstance(call, astlib.Instance):
            # __init__ method must return struct instance.
            return call.name
        elif isinstance(call, astlib.MethodCall):
            base_type = context.ns.get(str(call.base))
            return context.ts.get(base_type)[str(call.method)]
        errors.not_implemented("argument type is not supported (simpex)")

    def new_tmp(self, type_, expr):
        tmp_name = astlib.Name(
            "".join([self.tmp_string, str(self.tmp_count)]))
        self.tmp_count += 1
        return astlib.Decl(tmp_name, type_, expr)

    def expr(self, expr):
        if isinstance(expr, astlib.CINT_TYPES + (
                astlib.Name, astlib.StructElem, astlib.CFuncCall)):
            return expr, []
        elif isinstance(expr, astlib.Expr):
            decls = []
            lexpr_tmp, lexpr_decls = self.expr(expr.lexpr)
            rexpr_tmp, rexpr_decls = self.expr(expr.rexpr)
            decls.extend(lexpr_decls)
            decls.extend(rexpr_decls)
            return astlib.Expr(expr.op, lexpr_tmp, rexpr_tmp)
        elif isinstance(expr, (astlib.Ref, astlib.Unref)):
            expr_type = type(expr)
            tmp, decls = self.expr(expr.literal)
            return expr_type(tmp), decls
        elif isinstance(
                expr, (astlib.FuncCall, astlib.Instance, astlib.MethodCall)):
            translating_function = self.func_call
            if isinstance(expr, astlib.Instance):
                translating_function = self.instance
            elif isinstance(expr, astlib.MethodCall):
                translating_function = self.method_call
            result = list(translating_function(expr))
            tmp, decls = result[-1], result[:-1]
            return tmp, decls
        print("EXPR", expr, file=sys.stderr)
        print("EXPR_TYPE", type(expr), file=sys.stderr)
        errors.not_implemented("expr is not supported (simpex)")

    def body(self, body):
        reg = SimpEx().get_registry()
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        gened = list(layers.transform_node(body.stmt, registry=reg))
        result = astlib.Body(gened[0], astlib.Empty())
        result.extend_from_list(gened[1:])
        result.extend(self.body(body.rest))
        return result

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        tmp, decls = self.expr(assment.expr)
        yield from decls
        yield astlib.Assignment(
            assment.var, assment.op, tmp)

    @layers.register(astlib.Return)
    def return_(self, return_):
        tmp, decls = self.expr(return_.expr)
        yield from decls
        yield astlib.Return(tmp)

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), decl.type_)
        tmp, decls = self.expr(decl.expr)
        yield from decls
        yield astlib.Decl(decl.name, decl.type_, tmp)

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        context.fs.add(str(func.name), func.rettype)
        yield astlib.Func(
            func.name, func.args, func.rettype, self.body(func.body))
        context.ns.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        context.ns.add_scope()
        # TODO: add to fs.
        yield astlib.Method(
            method.name, method.args, method.rettype,
            self.body(method.body))
        context.ns.del_scope()

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body.as_list():
            if isinstance(stmt, astlib.Field):
                fields.append(stmt)
            else:
                methods.append(stmt)
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        fields, methods = self.split_body(struct.body)
        method_to_rettype = {}
        for method in methods:
            method_to_rettype[str(method.name)] = method.rettype
        context.ts.add(str(struct.name), method_to_rettype)
        yield astlib.Struct(struct.name, self.body(struct.body))
        context.ns.del_scope()

    def call_args(self, args):
        tmps, decls = astlib.Empty(), []
        for arg in args.as_list():
            if isinstance(arg, (
                        astlib.FuncCall, astlib.Instance, astlib.MethodCall)):
                if isinstance(arg, astlib.FuncCall):
                    result = list(self.func_call(arg))
                elif isinstance(arg, astlib.MethodCall):
                    result = list(self.method_call(arg))
                else:
                    result = list(self.instance(arg))
                func_rettype = self.get_func_rettype(arg)
                result_tmp, result_decls = result[-1], result[:-1]
                tmp_decl = self.new_tmp(func_rettype, result_tmp)
                tmp = tmp_decl.name
                decls.extend(result_decls)
                decls.append(tmp_decl)
                tmps = add_to_call_args(tmps, tmp)
            else:
                tmp, decls_ = self.expr(arg)
                decls.extend(decls_)
                tmps = add_to_call_args(tmps, tmp)
        return tmps, decls

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        tmps, decls = self.call_args(call.args)
        yield from decls
        yield astlib.FuncCall(call.name, tmps)

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        tmps, decls = self.call_args(call.args)
        yield from decls
        yield astlib.MethodCall(call.base, call.method, tmps)

    @layers.register(astlib.Instance)
    def instance(self, instance):
        tmps, decls = self.call_args(instance.args)
        yield from decls
        yield astlib.Instance(instance.name, tmps)
