"""Translates method declarations into function declarations."""

from . import layers, astlib, errors, cdefs, defs
from .context import context


class OOPDef(layers.Layer):

    def std_init_method(self, struct):
        errors.not_implemented("write init method, please :D")

    def std_deinit_method(self, struct):
        free = astlib.CFuncCall(
            "free", args=astlib.CallArgs(
                astlib.VariableName("self"), astlib.Empty()))
        body = astlib.Body(free, astlib.Empty())

        return astlib.Func(
            "".join([defs.DEINIT_METHOD_NAME, str(struct.name)]),
            args=astlib.Args(
                astlib.VariableName("self"), struct.name, astlib.Empty()),
            type_=astlib.CType("Void"),
            body=body)

    def init_method(self, method, struct_name):
        self_decl = astlib.Decl(
            astlib.VariableName("self"),
            type_=struct_name,
            expr=astlib.CFuncCall(
                "malloc", args=astlib.CallArgs(astlib.CFuncCall(
                    "sizeof", args=astlib.CallArgs(astlib.StructScalar(
                        struct_name), astlib.Empty()),
                ), astlib.Empty())))

        return_stmt = astlib.Return(astlib.VariableName("self"))

        body = astlib.Body(self_decl, astlib.Empty())
        body.extend(method.body)
        body.append(return_stmt)

        return astlib.Func(
            "".join([str(method.name), str(struct_name)]),
            args=method.args,
            type_=struct_name,
            body=body)

    def method_to_func(self, method, struct_name):
        if method.name == defs.INIT_METHOD_NAME:
            return self.init_method(method, struct_name)
        errors.not_implemented("method is not supported")

    def add_to_body(self, body, stmt):
        if isinstance(body, astlib.Empty):
            return astlib.Body(stmt, astlib.Empty())
        body.append(stmt)
        return body

    def split_body(self, body):
        fields, methods = astlib.Empty(), astlib.Empty()
        current_stmt = body
        while not isinstance(current_stmt, astlib.Empty):
            if isinstance(current_stmt.stmt, astlib.Field):
                fields = self.add_to_body(fields, current_stmt.stmt)
            else:
                methods = self.add_to_body(methods, current_stmt.stmt)
            current_stmt = current_stmt.rest
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)
        yield astlib.Struct(struct.name, fields)
        have_init = False
        have_deinit = False
        funcs = []
        for method in (
                methods.as_list() if isinstance(methods, astlib.Body)
                else []):
            if method.name == defs.INIT_METHOD_NAME:
                have_init = True
            elif method.name == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            funcs.append(self.method_to_func(method, struct.name))

        add_funcs = []
        if not have_deinit:
            add_funcs.append(self.std_deinit_method(struct))
        if not have_init:
            add_funcs.append(self.std_init_method(struct))
        yield from add_funcs + funcs