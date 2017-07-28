"""Translates expressions into more simple."""

from . import layers, astlib, errors, cdefs, defs
from .context import context


class SimpEx(layers.Layer):
    tmp_string = "tmp"

    def __init__(self, tmp_count=0):
        self.tmp_count = tmp_count

    def tmp(self, type_, expr):
        self.tmp_count += 1
        tmp_name = self.tmp_string + str(self.tmp_count)
        return astlib.Decl(astlib.Name(defs.TMP_PREFIX + tmp_name), type_, expr)

    def info_from_call(self, name):
        if isinstance(name, astlib.Name):
            return context.fs.get(name)
        errors.not_implemented("func is not supported (simpex layer)")

    def expr(self, expr):
        if isinstance(expr, (
                astlib.CIntFast8, astlib.CIntFast32,
                astlib.CUIntFast8, astlib.CUIntFast32,
                astlib.CUIntFast64, astlib.CIntFast64)):
            return expr, []
        elif isinstance(expr, astlib.SExpr):
            decls = []
            expr1_tmp, expr1_decls = self.expr(expr.expr1)
            expr2_tmp, expr2_decls = self.expr(expr.expr2)
            decls.extend(expr1_decls)
            decls.extend(expr2_decls)
            return astlib.SExpr(
                expr.op, expr1_tmp, expr2_tmp), decls
        elif isinstance(expr, astlib.Name):
            return expr, []
        elif isinstance(expr, astlib.StructElem):
            return expr, []
        elif isinstance(expr, astlib.FuncCall):
            result = list(self.func_call(expr))
            decls, tmp = result[:-1], result[-1]
            return tmp, decls
        elif isinstance(expr, astlib.CFuncCall):
            return expr, []
        elif isinstance(expr, astlib.Ref):
            tmp, decls =  self.expr(expr.literal)
            return astlib.Ref(tmp), decls
        elif isinstance(expr, astlib.Unref):
            tmp, decls =  self.expr(expr.literal)
            return astlib.Unref(tmp), decls
        errors.not_implemented("expr is not supported (simpex layer)")

    def add_to_call_args(self, args, arg):
        if isinstance(args, astlib.Empty):
            return astlib.CallArgs(arg, astlib.Empty())
        args.append(arg)
        return args

    def call_args(self, args):
        tmps, decls = astlib.Empty(), []
        for arg in args.as_list():
            if isinstance(arg, astlib.FuncCall):
                func_info = self.info_from_call(arg.name)
                res = list(self.func_call(arg))
                res_tmp, res_decls = res[-1], res[:-1]
                decl = self.tmp(func_info["rettype"], res_tmp)
                tmp = decl.name
                decls.extend(res_decls)
                decls.append(decl)
                tmps = self.add_to_call_args(tmps, tmp)
            else:
                tmp, decls_ = self.expr(arg)
                decls.extend(decls_)
                tmps = self.add_to_call_args(tmps, tmp)
        return tmps, decls

    def body(self, body):
        reg = SimpEx().get_registry()
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        gened = list(layers.transform_node(body.stmt, registry=reg))
        result = astlib.Body(gened[0], astlib.Empty())
        result.extend_from_list(gened[1:])
        result.extend(self.body(body.rest))
        return result

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), {
            "type_": decl.type_
        })
        tmp, decls = self.expr(decl.expr)
        yield from decls
        yield astlib.Decl(decl.name, decl.type_, tmp)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        tmp, decls = self.expr(assignment.expr)
        yield from decls
        yield astlib.Assignment(
            assignment.name, assignment.op, tmp)

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        tmps, decls = self.call_args(call.args)
        yield from decls
        yield astlib.FuncCall(call.name, tmps)

    @layers.register(astlib.Return)
    def return_(self, return_):
        tmp, decls = self.expr(return_.expr)
        yield from decls
        yield astlib.Return(tmp)

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        context.fs.add(str(func.name), {
            "rettype": func.type_
        })
        yield astlib.Func(
            func.name, func.args, func.type_, self.body(func.body))
        context.ns.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        yield astlib.Struct(struct.name, self.body(struct.body))
        context.ns.del_scope()
