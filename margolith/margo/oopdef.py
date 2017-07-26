"""Translates method declarations into function declarations."""

from . import layers, astlib, errors, cdefs, defs
from .context import context


SELF_VAR_NAME = astlib.VariableName("self")


def to_method_name(struct_name, method_name):
    return "".join([struct_name, method_name])


def std_methods_maker(method_name):
    def wrapper(struct_name):
        return to_method_name(str(struct_name), method_name)
    return wrapper


to_copy_method_name = std_methods_maker(defs.COPY_METHOD_NAME)
to_init_method_name = std_methods_maker(defs.INIT_METHOD_NAME)
to_deinit_method_name = std_methods_maker(defs.DEINIT_METHOD_NAME)


def field_of_maker(struct_name):
    def wrapper(field_name):
        return astlib.StructElem(struct_name, field_name)
    return wrapper


field_of_self = field_of_maker(SELF_VAR_NAME)


def malloc(struct_name):
    return astlib.CFuncCall(
        name="malloc", args=astlib.CallArgs(
            astlib.CFuncCall(
                name="sizeof", args=astlib.CallArgs(
                    astlib.StructScalar(struct_name), astlib.Empty())),
            astlib.Empty()))


class OOPDef(layers.Layer):

    def method(self, method, struct_name):
        # Just prepending self to args and translating method name to
        # function name with adding of a namespace.
        args = astlib.Args(
            name=SELF_VAR_NAME, type_=struct_name, rest=method.args)
        return astlib.Func(
            name=astlib.FunctionName(
                to_method_name(str(struct_name), str(method.name))),
            args=args, type_=method.type_, body=method.body)

    def default_copy_method(self, struct):
        new_var_name = astlib.VariableName("new")

        field_of_new = field_of_maker(new_var_name)

        declaration_of_new = astlib.Decl(
            new_var_name, type_=struct.name, expr=malloc(struct.name))

        field_inits = astlib.Empty()
        for field_decl in [
                stmt for stmt in struct.body.as_list()
                if isinstance(stmt, astlib.Field)]:
            if isinstance(field_decl.type_, astlib.TypeName):
                field_init_expr = astlib.FuncCall(
                    name=astlib.FunctionName(
                        to_copy_method_name(field_decl.type_)),
                    args=astlib.CallArgs(
                        field_of_self(field_decl.name), astlib.Empty()))
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                name=field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits = self.add_to_body(field_inits, field_init_decl)

        return_new = astlib.Return(new_var_name)
        body = astlib.Body(declaration_of_new, astlib.Empty())
        body.extend(field_inits)
        body.append(return_new)
        return astlib.Func(
            name=astlib.FunctionName(
                to_copy_method_name(struct.name)),
            args=astlib.Args(
                name=SELF_VAR_NAME, type_=struct.name, rest=astlib.Empty()),
            type_=struct.name,
            body=body)

    def std_init_method(self, struct):
        self_decl = astlib.Decl(
            astlib.VariableName("self"),
            type_=struct.name,
            expr=astlib.CFuncCall(
                "malloc", args=astlib.CallArgs(astlib.CFuncCall(
                    "sizeof", args=astlib.CallArgs(astlib.StructScalar(
                        struct.name), astlib.Empty()),
                ), astlib.Empty())))
        return_stmt = astlib.Return(astlib.VariableName("self"))

        field_inits = astlib.Empty()
        args = astlib.Empty()
        for field in struct.body.as_list():
            if isinstance(field, astlib.Field):
                args = self.add_to_args(args, field.name, field.type_)
                field_inits = self.add_to_body(
                    field_inits, astlib.Assignment(
                        astlib.StructElem(astlib.VariableName("self"), field.name),
                        op="=", expr=field.name))
        body = astlib.Body(self_decl, astlib.Empty())
        body.extend(field_inits)
        body.append(return_stmt)
        return astlib.Func(
            "".join([str(struct.name), defs.INIT_METHOD_NAME]),
            args=args,
            type_=struct.name,
            body=body)

    def std_deinit_method(self, struct):
        frees = []

        for field in struct.body.as_list():
            if (isinstance(field, astlib.Field) and \
                    isinstance(field.type_, astlib.TypeName)):
                frees.append(astlib.FuncCall(
                    astlib.FunctionName(
                        "".join([str(field.type_), defs.DEINIT_METHOD_NAME])),
                    args=astlib.CallArgs(astlib.StructElem(
                        name=astlib.VariableName("self"), elem=field.name), astlib.Empty())))

        free = astlib.CFuncCall(
            "free", args=astlib.CallArgs(
                astlib.VariableName("self"), astlib.Empty()))
        frees.append(free)
        body = astlib.Body(frees[0], astlib.Empty())
        body.extend_from_list(frees[1:])
        return astlib.Func(
            "".join([str(struct.name), defs.DEINIT_METHOD_NAME]),
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
            "".join([str(struct_name), str(method.name)]),
            args=method.args,
            type_=struct_name,
            body=body)

    def method_to_func(self, method, struct_name):
        if method.name == defs.INIT_METHOD_NAME:
            return self.init_method(method, struct_name)
        return self.method(method, struct_name)

    def add_to_args(self, args, name, type_):
        if isinstance(args, astlib.Empty):
            return astlib.Args(name, type_, astlib.Empty())
        args.append(name, type_)
        return args

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
        have_copy = False
        funcs = []
        for method in methods.as_list():
            if method.name == defs.INIT_METHOD_NAME:
                have_init = True
            elif method.name == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            elif method.name == defs.COPY_METHOD_NAME:
                have_copy = True
            funcs.append(self.method_to_func(method, struct.name))

        add_funcs = []
        if not have_copy:
            add_funcs.append(self.default_copy_method(struct))
        if not have_deinit:
            add_funcs.append(self.std_deinit_method(struct))
        if not have_init:
            add_funcs.append(self.std_init_method(struct))
        yield from add_funcs + funcs
