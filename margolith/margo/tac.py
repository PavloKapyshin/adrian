from itertools import chain

from . import astlib, layers, inference, errors
from .context import context
from .patterns import A


def split_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.Field):
            fields.append(stmt)
        if stmt in A(astlib.Method):
            methods.append(stmt)
    return fields, methods


# Temporary variable prefix.
T_STRING = "t"


def new_tmp(expr):
    tmp_name = astlib.Name(
        "".join([T_STRING, str(context.tmp_count)]), is_tmp=True)
    type_ = inference.infer(expr)
    context.env.add(str(tmp_name), {
        "type": type_
    })
    context.tmp_count += 1
    return tmp_name, [astlib.Decl(tmp_name, type_, expr)]


def inner_expr(expr):
    if expr in A(astlib.Expr):
        expr_, decls_ = e(expr)
        tmp, decls = new_tmp(expr_)
        return tmp, decls_ + decls

    if expr in A(astlib.FuncCall, astlib.StructCall):
        new_args, decls_ = call_args(expr.args)
        tmp, decls = new_tmp(type(expr)(expr.name, new_args))
        return tmp, decls_ + decls

    if expr in A(astlib.MethodCall):
        new_args, decls_ = call_args(expr.args)
        tmp, decls = new_tmp(astlib.MethodCall(expr.base, expr.method, new_args))
        return tmp, decls_ + decls

    if expr in A(astlib.StructElem):
        if expr.elem in A(astlib.StructElem):
            tmp, decls_ = new_tmp(astlib.StructElem(expr.name, expr.elem.name))
            result_tmp, decls = inner_expr(astlib.StructElem(tmp, expr.elem.elem))
            return result_tmp, decls_ + decls
        else:
            return new_tmp(expr)
    if expr in A(astlib.Name):
        return expr, []

    return new_tmp(expr)


def call_args(args):
    new_args, decls = [], []
    for arg in args:
        arg_, decls_ = inner_expr(arg)
        decls.extend(decls_)
        new_args.append(arg_)
    return new_args, decls


def e(expr):
    if expr in A(astlib.Expr):
        lexpr_tmp, lexpr_decls = inner_expr(expr.lexpr)
        rexpr_tmp, rexpr_decls = inner_expr(expr.rexpr)
        tmp_decls = lexpr_decls + rexpr_decls
        return astlib.Expr(expr.op, lexpr_tmp, rexpr_tmp), tmp_decls

    if expr in A(astlib.FuncCall, astlib.StructCall):
        new_args, decls = call_args(expr.args)
        return type(expr)(expr.name, new_args), decls

    if expr in A(astlib.MethodCall):
        new_args, decls = call_args(expr.args)
        return astlib.MethodCall(
            expr.base, expr.method, new_args), decls

    if expr in A(astlib.StructElem):
        if expr.elem in A(astlib.StructElem):
            return inner_expr(expr)
        else:
            return expr, []

    if expr in A(astlib.CTYPES + (astlib.Name, astlib.CFuncCall)):
        return expr, []

    errors.not_implemented(
        context.exit_on_error, "tac:e (expr {})".format(expr))


class TAC(layers.Layer):

    def b(self, body):
        reg = TAC().get_registry()
        return list(chain.from_iterable(
            map(lambda stmt: list(layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
             "type": decl.type_
        })
        new_expr, tmp_decls = e(decl.expr)
        yield from tmp_decls
        yield astlib.Decl(decl.name, decl.type_, new_expr)

    @layers.register(astlib.AssignmentAndAlloc)
    def assment_and_alloc(self, stmt):
        new_expr, tmp_decls = e(stmt.expr)
        yield from tmp_decls
        yield astlib.AssignmentAndAlloc(
            stmt.name, stmt.type_, new_expr)

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        new_expr, tmp_decls = e(assment.expr)
        yield from tmp_decls
        yield astlib.Assignment(assment.var, assment.op, new_expr)

    @layers.register(astlib.Return)
    def return_(self, return_):
        new_expr, tmp_decls = inner_expr(return_.expr)
        yield from tmp_decls
        yield astlib.Return(new_expr)

    @layers.register(astlib.Func)
    def func(self, func):
        context.env.add(str(func.name), {
            "type": func.rettype
        })
        context.env.add_scope()
        for arg in func.args:
            context.env.add(str(arg.name), {
                "type": arg.type_
            })

        yield astlib.Func(
            func.name, func.args, func.rettype,
            self.b(func.body))
        context.env.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        field_decls, method_decls = split_body(struct.body)
        fields = {}
        for field_decl in field_decls:
            fields[str(field_decl.name)] = field_decl.type_

        methods = {}
        for method in method_decls:
            methods[str(method.name)] = {
                "type": method.rettype
            }

        context.env.add(str(struct.name), {
            "type": struct.name,
            "fields": fields,
            "methods": methods
        })

        context.env.add_scope()

        context.env.add("self", {
            "type": struct.name
        })

        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            self.b(struct.body))
        context.env.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        context.env.add_scope()
        context.env.add(str(method.name), {
            "type": method.rettype
        })

        for arg in method.args:
            context.env.add(str(arg.name), {
                "type": arg.type_
            })

        yield astlib.Method(
            method.name, method.args, method.rettype,
            self.b(method.body))
        context.env.del_scope()
