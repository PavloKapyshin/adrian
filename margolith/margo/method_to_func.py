from . import layers, astlib, errors, defs
from .patterns import A
from .context import context



def split_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.Field):
            fields.append(stmt)
        else:
            methods.append(stmt)
    return fields, methods


class MethodToFunc(layers.Layer):

    def b(self, body):
        reg = MethodToFunc().get_registry()
        return list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Method)
    def method(self, method):
        yield astlib.Func(
            method.name, method.args, method.rettype,
            method.body)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = split_body(struct.body)

        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            fields)

        yield from self.b(methods)
