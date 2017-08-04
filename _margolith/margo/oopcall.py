"""
Translates method calls into function calls and
copies variables.
"""

import enum

from . import layers, astlib, errors, defs
from .context import context


def to_method_name(struct_name, method_name):
    return "".join([struct_name, method_name])


def to_copy_method_name(struct_name):
    return to_method_name(str(struct_name), defs.COPY_METHOD_NAME)


class NodeTypes(enum.Enum):
    variable = 1
    argument = 2


class OOPCall(layers.Layer):

    def expr(self, expr):
        if isinstance(expr, astlib.MethodCall):
            return list(self.method_call(expr))[0]
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.SExpr):
            return astlib.SExpr(
                op=expr.op, expr1=self.expr(expr.expr1),
                expr2=self.expr(expr.expr2))
        elif isinstance(expr, astlib.VariableName):
            var_info = context.ns.get(str(expr))
            var_type = var_info["type_"]
            if isinstance(var_type, astlib.Ref):
                return expr
            if not isinstance(var_type, astlib.CType):
                return astlib.FuncCall(
                    astlib.FunctionName(
                        to_copy_method_name(var_type)),
                    args=astlib.CallArgs(expr, astlib.Empty()))
        elif isinstance(expr, astlib.Unref):
            literal = expr.literal
            if isinstance(literal, astlib.StructElem):
                type_ = context.ns.get(str(literal.name))["type_"]
                if isinstance(type_, astlib.Ref):
                    struct_info = context.ts.get(str(type_.literal))
                else:
                    struct_info = context.ts.get(str(type_))
                var_type = struct_info["field_types"][str(literal.elem)]
                if isinstance(var_type, astlib.Ref):
                    var_type = var_type.literal
            else:
                var_info = context.ns.get(str(literal))
                var_type = var_info["type_"].literal
            if not isinstance(var_type, astlib.CType):
                return astlib.FuncCall(
                    astlib.FunctionName(
                        to_copy_method_name(var_type)),
                    args=astlib.CallArgs(literal, astlib.Empty()))
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
        expr = self.expr(return_.expr)
        if isinstance(return_.expr, astlib.VariableName):
            expr = return_.expr
        yield astlib.Return(expr)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        reg = OOPCall().get_registry()
        body = self.body(struct.body, reg)
        field_types = {}
        for field_decl in body.as_list():
            field_types[str(field_decl.name)] = field_decl.type_
        context.ts.add(str(struct.name), {
            "field_types": field_types
        })
        yield astlib.Struct(struct.name, body)
        context.ns.del_scope()

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        context.fs.add(str(func.name), {
            "rettype": func.type_,
        })
        reg = OOPCall().get_registry()
        args_ = func.args
        while not isinstance(args_, astlib.Empty):
            context.ns.add(str(args_.name), {
                "node_type": NodeTypes.argument,
                "type_": args_.type_
            })
            args_ = args_.rest

        yield astlib.Func(
            func.name, args=func.args,
            type_=func.type_, body=self.body(func.body, reg))
        context.ns.del_scope()

    @layers.register(astlib.Method)
    def _method(self, method):
        errors.not_implemented("something went wrong (oopcall)")

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        struct = context.ns.get(str(call.struct))["type_"]
        args = self.call_args(call.args)
        if str(call.method) != defs.INIT_METHOD_NAME:
            args = astlib.CallArgs(call.struct, args)
        yield astlib.FuncCall(
            astlib.FunctionName(
                to_method_name(str(struct), str(call.method))),
            args)

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), {
            "node_type": NodeTypes.variable,
            "type_": decl.type_,
        })
        yield astlib.Decl(decl.name, decl.type_, self.expr(decl.expr))