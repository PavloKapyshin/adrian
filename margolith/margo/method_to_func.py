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


def decl_args(struct_name, name, args):
    if str(name) == defs.INIT_METHOD_NAME:
        return args
    return [astlib.Arg("self", struct_name)] + args


class MethodToFunc(layers.Layer):

    def __init__(self, struct_name=None):
        self.struct_name = struct_name

    def b(self, body):
        reg = MethodToFunc(struct_name=self.struct_name).get_registry()
        return list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Method)
    def method(self, method):
        yield astlib.Func(
            method.name,
            decl_args(self.struct_name, method.name, method.args),
            method.rettype, method.body)

    @layers.register(astlib.Struct)
    def struct(self, struct):
        self.struct_name = struct.name
        fields, methods = split_body(struct.body)

        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            fields)

        yield from self.b(methods)
