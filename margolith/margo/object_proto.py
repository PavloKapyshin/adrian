from . import layers, astlib, errors
from . import cdefs, defs
from .context import context
from .patterns import A


SELF = astlib.Name("self")


def field_of_maker(struct_name):
    def wrapper(field_name):
        return astlib.StructElem(struct_name, field_name)
    return wrapper


field_of_self = field_of_maker(SELF)


def deinit(arg):
    return astlib.MethodCall(
        arg, defs.DEINIT_METHOD_NAME, [])


def free(arg):
    return astlib.CFuncCall("free", [arg])


def malloc(struct_name):
    return astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(struct_name)])])


def only_field_decls(body):
    return split_body(body)[0]


def split_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.Field):
            fields.append(stmt)
        if stmt in A(astlib.Method):
            methods.append(stmt)
    return fields, methods


def default_deinit_method(struct):
    body = []
    for field_decl in only_field_decls(struct.body):
        if field_decl.type_ in A(astlib.Name):
            body.append(deinit(field_of_self(field_decl.name)))
        else:
            body.append(free(field_of_self(field_decl.name)))
    body.append(free(SELF))
    return astlib.Method(
        astlib.Name(defs.DEINIT_METHOD_NAME),
        [], rettype=astlib.CType("Void"), body=body)


def default_copy_method(struct):
    new_var_name = astlib.Name("new")
    field_of_new = field_of_maker(new_var_name)
    declaration_of_new = astlib.Decl(
        new_var_name, struct.name, malloc(struct.name))
    field_inits = []
    for field_decl in only_field_decls(struct.body):
        if field_decl.type_ in A(astlib.Name):
            field_init_expr = astlib.MethodCall(
                field_of_self(field_decl.name),
                defs.COPY_METHOD_NAME,
                [])
        else:
            field_init_expr = field_of_self(field_decl.name)
        field_init_decl = astlib.AssignmentAndAlloc(
            field_of_new(field_decl.name), type_=field_decl.type_,
            expr=field_init_expr)
        field_inits.append(field_init_decl)
    return_new = astlib.Return(new_var_name)
    body = [declaration_of_new] + field_inits + [return_new]
    return astlib.Method(
        astlib.Name(defs.COPY_METHOD_NAME),
        [], struct.name, body=body)


def complete_copy_method(method, struct):
    errors.not_implemented(
        context.exit_on_error, "custom copy method is not supported")


def complete_deinit_method(method, struct):
    errors.not_implemented(
        context.exit_on_error, "custom deinit method is not supported")


def complete_init_method(method, struct):
    declaration_of_self = astlib.Decl(
        SELF, struct.name, malloc(struct.name))
    return_self = astlib.Return(SELF)
    new_method_body = []
    fields_ = only_field_decls(struct.body)
    fields = {}
    for decl in fields_:
        fields[str(decl.name)] = decl.type_
    for stmt in method.body:
        if stmt in A(astlib.Assignment):
            type_ = fields[str(stmt.var.elem)]
            new_method_body.append(
                astlib.AssignmentAndAlloc(
                    stmt.var, type_, stmt.expr))
        else:
            new_method_body.append(stmt)
    body = [declaration_of_self] + new_method_body + [return_self]
    rettype = struct.name
    return astlib.Method(
        astlib.Name(method.name), method.args,
        rettype, body)


def default_init_method(struct):
    declaration_of_self = astlib.Decl(
        SELF, struct.name, malloc(struct.name))
    return_self = astlib.Return(SELF)
    field_inits, args = [], []
    for field_decl in only_field_decls(struct.body):
        args.append(
            astlib.Arg(field_decl.name, field_decl.type_))
        if field_decl.type_ in A(astlib.Name):
            field_init_expr = astlib.MethodCall(
                field_decl.name,
                defs.COPY_METHOD_NAME,
                [])
        else:
            field_init_expr = field_decl.name
        field_inits.append(
            astlib.AssignmentAndAlloc(
                field_of_self(field_decl.name), type_=field_decl.type_,
                expr=field_init_expr))
    body = [declaration_of_self] + field_inits + [return_self]
    rettype = struct.name
    return astlib.Method(
        astlib.Name(defs.INIT_METHOD_NAME), args,
        rettype, body)


class ObjectProto(layers.Layer):

    def method(self, method, struct):
        if str(method.name) == defs.INIT_METHOD_NAME:
            return complete_init_method(method, struct)
        if str(method.name) == defs.DEINIT_METHOD_NAME:
            return complete_deinit_method(method, struct)
        if str(method.name) == defs.COPY_METHOD_NAME:
            return complete_copy_method(method, struct)
        return method

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = split_body(struct.body)
        have_init, have_deinit, have_copy = False, False, False
        new_methods = []
        for method in methods:
            if str(method.name) == defs.INIT_METHOD_NAME:
                have_init = True

            if str(method.name) == defs.COPY_METHOD_NAME:
                have_copy = True

            if str(method.name) == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            new_methods.append(self.method(method, struct))
        add_methods = []
        if not have_init:
            add_methods.append(
                default_init_method(struct))
        if not have_deinit:
            add_methods.append(
                default_deinit_method(struct))
        if not have_copy:
            add_methods.append(
                default_copy_method(struct))
        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            fields + add_methods + new_methods)