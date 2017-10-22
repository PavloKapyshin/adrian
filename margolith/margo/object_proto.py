from . import layers, astlib, errors, defs
from .context import context, split_body
from .patterns import A


SELF = astlib.Name("self")


def field_of_maker(struct_name):
    def wrapper(field_name):
        return astlib.StructMember(struct_name, field_name)
    return wrapper


field_of_self = field_of_maker(SELF)


def deinit(struct_name, arg):
    return astlib.StructFuncCall(
        struct_name, astlib.Name(defs.DEINIT_METHOD_NAME), [arg])


def free(arg):
    return astlib.CFuncCall("free", [arg])


def malloc(struct_name):
    return astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(struct_name)])])


def only_field_decls(body):
    return split_body(body)[0]


def default_deinit_method(struct):
    body = []
    for field_decl in only_field_decls(struct.body):
        if field_decl.type_ in A(astlib.Name):
            body.append(
                deinit(
                    field_decl.type_,
                    field_of_self(field_decl.name)))
        else:
            body.append(free(field_of_self(field_decl.name)))
    body.append(free(SELF))
    return astlib.MethodDecl(
        astlib.Name(defs.DEINIT_METHOD_NAME),
        [astlib.Arg(astlib.Name("self"), struct.name)],
        rettype=astlib.CType("Void"), body=body)


def default_copy_method(struct):
    new_var_name = astlib.Name("new")
    field_of_new = field_of_maker(new_var_name)
    declaration_of_new = astlib.VarDecl(
        new_var_name, struct.name, malloc(struct.name))
    field_inits = []
    for field_decl in only_field_decls(struct.body):
        if field_decl.type_ in A(astlib.Name):
            field_init_expr = astlib.StructFuncCall(
                field_decl.type_,
                defs.COPY_METHOD_NAME,
                [field_of_self(field_decl.name)])
        else:
            field_init_expr = field_of_self(field_decl.name)
        field_init_decl = astlib.AssignmentAndAlloc(
            field_of_new(field_decl.name), type_=field_decl.type_,
            expr=field_init_expr)
        field_inits.append(field_init_decl)
    return_new = astlib.Return(new_var_name)
    body = [declaration_of_new] + field_inits + [return_new]
    return astlib.MethodDecl(
        astlib.Name(defs.COPY_METHOD_NAME),
        [astlib.Arg(astlib.Name("self"), struct.name)],
        struct.name, body=body)


def complete_copy_method(method, struct):
    errors.not_implemented(
        context.exit_on_error, "custom copy method is not supported")


def complete_deinit_method(method, struct):
    errors.not_implemented(
        context.exit_on_error, "custom deinit method is not supported")


def complete_init_method(method, struct):
    declaration_of_self = astlib.VarDecl(
        SELF, struct.name, malloc(struct.name))
    return_self = astlib.Return(SELF)
    new_method_body = []
    fields_ = only_field_decls(struct.body)
    fields = {}
    for decl in fields_:
        fields[str(decl.name)] = decl.type_
    for stmt in method.body:
        if stmt in A(astlib.Assignment):
            type_ = fields[str(stmt.variable.member)]
            new_method_body.append(
                astlib.AssignmentAndAlloc(
                    stmt.variable, type_, stmt.expr))
        else:
            new_method_body.append(stmt)
    body = [declaration_of_self] + new_method_body + [return_self]
    rettype = struct.name
    return astlib.MethodDecl(
        astlib.Name(defs.INIT_METHOD_NAME), method.args,
        rettype, body)


def default_init_method(struct):
    declaration_of_self = astlib.VarDecl(
        SELF, struct.name, malloc(struct.name))
    return_self = astlib.Return(SELF)
    field_inits, args = [], []
    for field_decl in only_field_decls(struct.body):
        args.append(
            astlib.Arg(field_decl.name, field_decl.type_))
        if field_decl.type_ in A(astlib.Name):
            field_init_expr = astlib.StructFuncCall(
                field_decl.type_,
                defs.COPY_METHOD_NAME,
                [field_decl.name])
        else:
            field_init_expr = field_decl.name
        field_inits.append(
            astlib.AssignmentAndAlloc(
                field_of_self(field_decl.name), field_decl.type_,
                field_init_expr))
    body = [declaration_of_self] + field_inits + [return_self]
    rettype = struct.name
    return astlib.MethodDecl(
        astlib.Name(defs.INIT_METHOD_NAME), args,
        rettype, body)


def method_to_struct_func(method, struct):
    return astlib.StructFuncDecl(
        struct.name, method.name, method.args,
        method.rettype, method.body)


class ObjectProto(layers.Layer):

    def method(self, method, struct):
        return astlib.MethodDecl(
            method.name,
            [astlib.Arg(
                astlib.Name("self"), struct.name)] + method.args,
            method.rettype, method.body)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        fields, methods = split_body(declaration.body)
        dict_for_haves = {
            key: (False, default_func, complete_func)
            for key, default_func, complete_func in (
                (defs.INIT_METHOD_NAME, default_init_method,
                    complete_init_method),
                (defs.DEINIT_METHOD_NAME, default_deinit_method,
                    complete_deinit_method),
                (defs.COPY_METHOD_NAME, default_copy_method,
                    complete_copy_method))
        }
        new_methods = []
        for method in methods:
            if str(method.name) in dict_for_haves:
                was = dict_for_haves[str(method.name)]
                dict_for_haves[str(method.name)] = (True, was[1], was[2])
                complete_func = dict_for_haves[str(method.name)][2]
                new_methods.append(complete_func(method, declaration))
            else:
                new_methods.append(self.method(method, declaration))
        additional_methods = []
        for method_name, (exists, default_func, _) in sorted(dict_for_haves.items()):
            if not exists:
                additional_methods.append(
                    default_func(declaration))
        yield astlib.StructDecl(
            declaration.name,
            declaration.var_types,
            fields)

        for method in additional_methods + new_methods:
            yield method_to_struct_func(method, declaration)
