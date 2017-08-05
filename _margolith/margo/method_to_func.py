"""Translates method declarations into function declarations."""

from . import layers, astlib, errors, defs


class MethodToFunc(layers.Layer):

    def other_method(self, method, struct_name):
        args = [astlib.Arg(astlib.Name("self"), struct_name)] + method.args
        return astlib.Func(
            astlib.Name("".join([str(struct_name), str(method.name)])),
            args, method.rettype, method.body)

    def init_method(self, method, struct_name):
        return astlib.Func(
            astlib.Name("".join([str(struct_name), str(method.name)])),
            method.args, struct_name, method.body)

    def method_to_func(self, method, struct_name):
        if method.name == defs.INIT_METHOD_NAME:
            return self.init_method(method, struct_name)
        return self.other_method(method, struct_name)

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body:
            if isinstance(stmt, astlib.Field):
                fields.append(stmt)
            else:
                methods.append(stmt)
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)
        yield astlib.Struct(struct.name, struct.param_types, fields)
        for method in methods:
            yield self.method_to_func(method, struct.name)