"""Translates method declarations into function declarations."""

from . import layers, astlib, errors, defs


class MethodToFunc(layers.Layer):

    def other_method(self, method, struct_name):
        args = astlib.Args(
            name=astlib.Name("self"), type_=struct_name,
            rest=method.args)
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
        fields, methods = astlib.Empty(), []
        current_stmt = body
        while not isinstance(current_stmt, astlib.Empty):
            if isinstance(current_stmt.stmt, astlib.Field):
                fields = add_to_body(fields, current_stmt.stmt)
            else:
                methods.append(current_stmt.stmt)
            current_stmt = current_stmt.rest
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)
        yield astlib.Struct(struct.name, fields)
        for method in methods:
            yield self.method_to_func(method, struct.name)