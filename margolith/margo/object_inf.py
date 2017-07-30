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
        arg, defs.DEINIT_METHOD_NAME, astlib.Empty())


def free(arg):
    return astlib.CFuncCall(
        "free", astlib.CallArgs(arg, astlib.Empty()))


def malloc(struct_name):
    return astlib.CFuncCall(
        "malloc", astlib.CallArgs(
            astlib.CFuncCall(
                "sizeof", astlib.CallArgs(
                    astlib.StructScalar(struct_name), astlib.Empty())),
            astlib.Empty()))


def _add_to_maker(llist_type):
    def wrapper(llist, *args):
        if isinstance(llist, astlib.Empty):
            return llist_type(*args, rest=astlib.Empty())
        llist.append(*args)
        return llist
    return wrapper


add_to_args = _add_to_maker(astlib.Args)
add_to_body = _add_to_maker(astlib.Body)


class ObjectInf(layers.Layer):

    def only_field_decls(self, body):
        return [stmt for stmt in body.as_list()
            if isinstance(stmt, astlib.Field)]

    def default_deepcopy_method(self, struct):
        new_var_name = astlib.Name("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, type_=struct.name, expr=malloc(struct.name))
        field_inits = astlib.Empty()
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                field_init_expr = astlib.MethodCall(
                    field_of_self(field_decl.name),
                    defs.DEEPCOPY_METHOD_NAME,
                    astlib.Empty())
            elif isinstance(field_decl.type_, astlib.Ref):
                field_init_expr = astlib.MethodCall(
                    astlib.Unref(field_of_self(field_decl.name)),
                    defs.DEEPCOPY_METHOD_NAME,
                    astlib.Empty())
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits = add_to_body(field_inits, field_init_decl)
        return_new = astlib.Return(new_var_name)
        body = astlib.Body(declaration_of_new, astlib.Empty())
        body.extend(field_inits)
        body.append(return_new)
        return astlib.Method(
            astlib.Name(defs.DEEPCOPY_METHOD_NAME),
            astlib.Args(
                name=SELF_NAME, type_=struct.name, rest=astlib.Empty()),
            rettype=struct.name, body=body)

    def default_copy_method(self, struct):
        new_var_name = astlib.Name("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, type_=struct.name, expr=malloc(struct.name))
        field_inits = astlib.Empty()
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                field_init_expr = astlib.MethodCall(
                    field_of_self(field_decl.name),
                    astlib.Name(defs.COPY_METHOD_NAME),
                    astlib.Empty())
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits = add_to_body(field_inits, field_init_decl)
        return_new = astlib.Return(new_var_name)
        body = astlib.Body(declaration_of_new, astlib.Empty())
        body.extend(field_inits)
        body.append(return_new)
        return astlib.Method(
            astlib.Name(defs.COPY_METHOD_NAME),
            astlib.Args(name=SELF_NAME, type_=struct.name, rest=astlib.Empty()),
            rettype=struct.name, body=body)

    def default_deinit_method(self, struct):
        frees_and_deinits = []
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.Name):
                frees_and_deinits.append(
                    deinit(field_of_self(field_decl.name)))
        frees_and_deinits.append(free(SELF_NAME))
        body = astlib.Body(frees_and_deinits[0], astlib.Empty())
        body.extend_from_list(frees_and_deinits[1:])
        return astlib.Method(
            astlib.Name(defs.DEINIT_METHOD_NAME),
            astlib.Args(
                name=SELF_NAME, type_=struct.name, rest=astlib.Empty()),
            rettype=astlib.CType("Void"), body=body)

    def default_init_method(self, struct):
        declaration_of_self = astlib.Decl(
            SELF_NAME, type_=struct.name, expr=malloc(struct.name))
        return_self = astlib.Return(SELF_NAME)
        field_inits = []
        args = astlib.Empty()
        for field_decl in self.only_field_decls(struct.body):
            args = add_to_args(args, field_decl.name, field_decl.type_)
            field_inits.append(
                astlib.Assignment(
                    field_of_self(field_decl.name), op="=",
                    expr=field_decl.name))
        body = astlib.Body(declaration_of_self, astlib.Empty())
        body.extend_from_list(field_inits)
        body.append(return_self)
        return astlib.Method(
            astlib.Name(defs.INIT_METHOD_NAME), args,
            rettype=struct.name, body=body)

    def split_body(self, body):
        fields, methods = astlib.Empty(), astlib.Empty()
        current_stmt = body
        while not isinstance(current_stmt, astlib.Empty):
            if isinstance(current_stmt.stmt, astlib.Field):
                fields = add_to_body(fields, current_stmt.stmt)
            else:
                methods = add_to_body(methods, current_stmt.stmt)
            current_stmt = current_stmt.rest
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        fields, methods = self.split_body(struct.body)
        have_init = False
        have_deinit = False
        have_copy = False
        have_deepcopy = False
        for method in methods.as_list():
            if method.name == defs.INIT_METHOD_NAME:
                have_init = True
            elif method.name == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            elif method.name == defs.COPY_METHOD_NAME:
                have_copy = True
            elif method.name == defs.DEEPCOPY_METHOD_NAME:
                have_deepcopy = True
        add_methods = []
        if not have_init:
            add_methods.append(self.default_init_method(struct))
        if not have_deinit:
            add_methods.append(self.default_deinit_method(struct))
        if not have_copy:
            add_methods.append(self.default_copy_method(struct))
        if not have_deepcopy:
            add_methods.append(self.default_deepcopy_method(struct))
        list_of_methods = add_methods + methods.as_list()
        new_body = fields
        rest = list_of_methods
        if isinstance(new_body, astlib.Empty):
            new_body = astlib.Body(list_of_methods[0], astlib.Empty())
            rest = list_of_methods[1:]
        new_body.extend_from_list(rest)
        yield astlib.Struct(struct.name, new_body)
