from itertools import chain

from . import layers, astlib, errors, inference
from .context import context, get
from .patterns import A


def split_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.Field):
            fields.append(stmt)
        if stmt in A(astlib.Method):
            methods.append(stmt)
    return fields, methods


def get_assment(name, val):
    return astlib.Assignment(
        astlib.Deref(name), "=", val)


def get_val(value):
    if value in A(astlib.Name):
        return astlib.Deref(value)
    if value in A(astlib.Expr):
        return astlib.Expr(
            value.op, get_val(value.lexpr), get_val(value.rexpr))
    # I dont sure :D
    if value in A(astlib.StructElem):
        return astlib.Deref(value)
    return value


def heapify(expr, name):
    type_ = inference.infer(expr)
    allocation = astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(type_)])])
    assignment = get_assment(name, get_val(expr))
    return allocation, [assignment]

def e(expr, name):
    if expr in A(astlib.CTYPES):
        return heapify(expr, name)

    if expr in A(astlib.Name):
        if get(expr)["type"] in A(astlib.CType):
            return heapify(expr, name)
        return expr, []

    if expr in A(astlib.Expr, astlib.StructElem):
        return heapify(expr, name)

    if expr in A(astlib.CFuncCall, astlib.FuncCall, astlib.StructCall, astlib.MethodCall):
        return expr, []

    errors.not_implemented(
        context.exit_on_error,
        "copying:e (expr {})".format(expr))


class Copying(layers.Layer):

    def b(self, body):
        reg = Copying().get_registry()
        return list(chain.from_iterable(
            map(lambda stmt: list(layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
            "type": decl.type_
        })
        new_expr, assignments = e(decl.expr, decl.name)
        yield astlib.Decl(decl.name, decl.type_, new_expr)
        yield from assignments

    @layers.register(astlib.AssignmentAndAlloc)
    def assment_and_alloc(self, stmt):
        new_expr, assignments = e(stmt.expr, stmt.name)
        yield astlib.Assignment(stmt.name, "=", new_expr)
        yield from assignments

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        expr_type = inference.infer(assment.expr)
        if expr_type in A(astlib.Name):
            yield from self.assment_and_alloc(
                astlib.AssignmentAndAlloc(
                    assment.var, expr_type, assment.expr))
        else:
            yield get_assment(assment.var, assment.expr)

    @layers.register(astlib.Return)
    def return_(self, return_):
        # We don't use e function, for now.
        yield return_

    @layers.register(astlib.Func)
    def func(self, func):
        context.env.add_scope()
        context.env.add(str(func.name), {
            "type": func.rettype
        })
        for arg in func.args:
            context.env.add(str(arg.name), {
                "type": arg.type_
            })

        yield astlib.Func(
            func.name, func.args, func.rettype,
            self.b(func.body))
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
