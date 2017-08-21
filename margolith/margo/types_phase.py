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


class TypeInference(layers.Layer):

    def b(self, body):
        reg = TypeInference().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        type_ = decl.type_
        if type_ in A(astlib.Empty):
            type_ = inference.infer(decl.expr)
        context.env.add(str(decl.name), {
            "type": type_
        })
        yield astlib.Decl(decl.name, type_, decl.expr)

    @layers.register(astlib.Func)
    def func(self, func):
        # We don't have type inference for func rettype, for now.
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

    @layers.register(astlib.Method)
    def method(self, method):
        # We don't have type inference for method rettype, for now.
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
