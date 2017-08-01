"""Adds implementation of Object interface."""

from . import layers, astlib, errors, defs


SELF_NAME = astlib.Name("self")


def field_of_maker(struct_name):
    def wrapper(field_name):
        return astlib.StructElem(struct_name, field_name)
    return wrapper


field_of_self = field_of_maker(SELF_NAME)


def deinit(arg):
    return astlib.MethodCall(
        arg, defs.DEINIT_METHOD_NAME, [])


def free(arg):
    return astlib.CFuncCall("free", [arg])


def malloc(struct_name):
    return astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(struct_name)])])


class ObjectInf(layers.Layer):

    def only_field_decls(self, body):
        return [stmt for stmt in body if isinstance(stmt, astlib.Field)]

    def default_deepcopy_method(self, struct):
        def to_type(type_):
            if struct.param_types:
                return astlib.ParamedType(type_, struct.param_types)
            return type_
        new_var_name = astlib.Name("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, to_type(struct.name), malloc(struct.name))
        field_inits = []
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                field_init_expr = astlib.MethodCall(
                    field_of_self(field_decl.name),
                    defs.DEEPCOPY_METHOD_NAME,
                    [])
            elif isinstance(field_decl.type_, astlib.Ref):
                field_init_expr = astlib.MethodCall(
                    astlib.Unref(field_of_self(field_decl.name)),
                    defs.DEEPCOPY_METHOD_NAME,
                    [])
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits.append(field_init_decl)
        return_new = astlib.Return(new_var_name)
        body = [declaration_of_new] + field_inits + [return_new]
        return astlib.Method(
            astlib.Name(defs.DEEPCOPY_METHOD_NAME),
            [], to_type(struct.name), body=body)

    def default_copy_method(self, struct):
        def to_type(type_):
            if struct.param_types:
                return astlib.ParamedType(type_, struct.param_types)
            return type_
        new_var_name = astlib.Name("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, to_type(struct.name), malloc(struct.name))
        field_inits = []
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                field_init_expr = astlib.MethodCall(
                    field_of_self(field_decl.name),
                    astlib.Name(defs.COPY_METHOD_NAME),
                    [])
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits.append(field_init_decl)
        return_new = astlib.Return(new_var_name)
        body = [declaration_of_new] + field_inits + [return_new]
        return astlib.Method(
            astlib.Name(defs.COPY_METHOD_NAME),
            [], to_type(struct.name), body=body)

    def default_deinit_method(self, struct):
        body = []
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                body.append(deinit(field_of_self(field_decl.name)))
        body.append(free(SELF_NAME))
        return astlib.Method(
            astlib.Name(defs.DEINIT_METHOD_NAME),
            [], rettype=astlib.CType("Void"), body=body)

    def complete_init_method(self, method, struct):
        def to_type(type_):
            if struct.param_types:
                return astlib.ParamedType(type_, struct.param_types)
            return type_
        declaration_of_self = astlib.Decl(
            SELF_NAME, type_=struct.name, expr=malloc(struct.name))
        return_self = astlib.Return(SELF_NAME)
        body = [declaration_of_self] + method.body + [return_self]
        rettype = to_type(struct.name)
        return astlib.Method(
            astlib.Name(method.name), method.args,
            rettype, body=body)

    def default_init_method(self, struct):
        def to_type(type_):
            if struct.param_types:
                return astlib.ParamedType(type_, struct.param_types)
            return type_
        declaration_of_self = astlib.Decl(
            SELF_NAME, type_=to_type(struct.name), expr=malloc(struct.name))
        return_self = astlib.Return(SELF_NAME)
        field_inits, args = [], []
        for field_decl in self.only_field_decls(struct.body):
            args.append(astlib.Arg(field_decl.name, field_decl.type_))
            if isinstance(field_decl.type_, astlib.Name):
                field_init_expr = astlib.MethodCall(
                    field_decl.name, defs.COPY_METHOD_NAME, [])
            else:
                field_init_expr = field_decl.name
            field_inits.append(
                astlib.Assignment(
                    field_of_self(field_decl.name), op="=",
                    expr=field_init_expr))
        body = [declaration_of_self] + field_inits + [return_self]
        rettype = to_type(struct.name)
        return astlib.Method(
            astlib.Name(defs.INIT_METHOD_NAME), args,
            rettype, body=body)

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body:
            if isinstance(stmt, astlib.Field):
                fields.append(stmt)
            else:
                methods.append(stmt)
        return fields, methods

    def method(self, method, struct):
        if str(method.name) == defs.INIT_METHOD_NAME:
            return self.complete_init_method(method, struct)
        return method

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)
        have_init = False
        have_deinit = False
        have_copy = False
        have_deepcopy = False
        new_methods = []
        for method in methods:
            if method.name == defs.INIT_METHOD_NAME:
                have_init = True
            elif method.name == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            elif method.name == defs.COPY_METHOD_NAME:
                have_copy = True
            elif method.name == defs.DEEPCOPY_METHOD_NAME:
                have_deepcopy = True
            new_methods.append(self.method(method, struct))
        add_methods = []
        if not have_init:
            add_methods.append(self.default_init_method(struct))
        if not have_deinit:
            add_methods.append(self.default_deinit_method(struct))
        if not have_copy:
            add_methods.append(self.default_copy_method(struct))
        if not have_deepcopy:
            add_methods.append(self.default_deepcopy_method(struct))
        yield astlib.Struct(struct.name, struct.param_types, fields + add_methods + new_methods)
