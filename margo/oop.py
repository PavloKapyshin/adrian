"""Translates method declarations into function declarations."""

from . import layers, astlib, errors, defs
from .context import context


class OOP(layers.Layer):

    def _init_method(self, method, struct_name):
        self_decl = astlib.Decl(
            name=astlib.VariableName("self"),
            type_=astlib.CType("Memory"),
            expr=astlib.CFuncCall(
                name="malloc",
                args=astlib.CallArgs(astlib.CFuncCall(
                    name="sizeof",
                    args=astlib.CallArgs(astlib.StructScalar(
                        name=struct_name), astlib.Empty()),
                ), astlib.Empty())))
        return_stmt = astlib.Return(expr=astlib.VariableName("self"))
        body = astlib.Body(self_decl, astlib.Empty())
        body.extend(method.body)
        body.append(return_stmt)
        init_func = astlib.FuncDecl(
            name="".join([str(method.name), str(struct_name)]),
            args=method.args,
            type_=struct_name,
            body=body)
        return init_func

    def _method_to_func(self, method, struct_name):
        if method.name == defs.INIT_METHOD_NAME:
            return self._init_method(method, struct_name)
        errors.not_implemented()

    def _add_to_body(self, body, stmt):
        if isinstance(body, astlib.Empty):
            return astlib.Body(stmt, astlib.Empty())
        body.append(stmt)
        return body

    def _split_body(self, body):
        current_body = body
        fields, methods = astlib.Empty(), astlib.Empty()
        while not isinstance(current_body, astlib.Empty):
            pair = current_body.stmt
            if isinstance(pair.stmt, astlib.FieldDecl):
                fields = self._add_to_body(fields, pair)
            else:
                methods = self._add_to_body(methods, pair)
            current_body = current_body.rest
        return fields, methods

    @layers.preregister(astlib.StructDecl)
    def _struct_decl(self, struct_decl):
        fields, methods = self._split_body(struct_decl.body)
        plain_struct = layers.create_with(struct_decl, body=fields)
        result = [plain_struct]
        for pair in methods.as_list():
            result.append(self._method_to_func(pair.stmt, struct_decl.name))
        yield from result
