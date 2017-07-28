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
to_deepcopy_method_name = std_methods_maker(defs.DEEPCOPY_METHOD_NAME)


def field_of_maker(struct_name):
    def wrapper(field_name):
        return astlib.StructElem(struct_name, field_name)
    return wrapper


field_of_self = field_of_maker(SELF_VAR_NAME)


def deinit(struct, arg):
    return astlib.FuncCall(
        name=astlib.FunctionName(
            to_deinit_method_name(struct)),
        args=astlib.CallArgs(arg, astlib.Empty()))


def free(arg):
    return astlib.CFuncCall(
        name="free", args=astlib.CallArgs(
            arg, astlib.Empty()))


def malloc(struct_name):
    return astlib.CFuncCall(
        name="malloc", args=astlib.CallArgs(
            astlib.CFuncCall(
                name="sizeof", args=astlib.CallArgs(
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


class OOPDef(layers.Layer):

    def only_field_decls(self, body):
        return [stmt for stmt in body.as_list()
            if isinstance(stmt, astlib.Field)]

    def other_method(self, method, struct_name):
        # Just prepending self to args and translating method name to
        # function name with adding of a namespace.
        args = astlib.Args(
            name=SELF_VAR_NAME, type_=struct_name, rest=method.args)
        return astlib.Func(
            name=astlib.FunctionName(
                to_method_name(str(struct_name), str(method.name))),
            args=args, type_=method.type_, body=method.body)

    def default_deepcopy_method(self, struct):
        new_var_name = astlib.VariableName("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, type_=struct.name, expr=malloc(struct.name))
        field_inits = astlib.Empty()
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.TypeName):
                field_init_expr = astlib.FuncCall(
                    name=astlib.FunctionName(
                        to_deepcopy_method_name(field_decl.type_)),
                    args=astlib.CallArgs(
                        field_of_self(field_decl.name), astlib.Empty()))
            elif isinstance(field_decl.type_, astlib.Ref):
                field_init_expr = astlib.FuncCall(
                    name=astlib.FunctionName(
                        to_deepcopy_method_name(field_decl.type_.literal)),
                    args=astlib.CallArgs(
                        astlib.Unref(field_of_self(field_decl.name)), astlib.Empty()))
            else:
                field_init_expr = field_of_self(field_decl.name)
            field_init_decl = astlib.Assignment(
                name=field_of_new(field_decl.name), op="=", expr=field_init_expr)
            field_inits = add_to_body(field_inits, field_init_decl)

        return_new = astlib.Return(new_var_name)
        body = astlib.Body(declaration_of_new, astlib.Empty())
        body.extend(field_inits)
        body.append(return_new)
        return astlib.Func(
            name=astlib.FunctionName(
                to_deepcopy_method_name(struct.name)),
            args=astlib.Args(
                name=SELF_VAR_NAME, type_=struct.name, rest=astlib.Empty()),
            type_=struct.name,
            body=body)

    def default_copy_method(self, struct):
        new_var_name = astlib.VariableName("new")
        field_of_new = field_of_maker(new_var_name)
        declaration_of_new = astlib.Decl(
            new_var_name, type_=struct.name, expr=malloc(struct.name))
        field_inits = astlib.Empty()
        for field_decl in self.only_field_decls(struct.body):
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
            field_inits = add_to_body(field_inits, field_init_decl)

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

    def default_init_method(self, struct):
        declaration_of_self = astlib.Decl(
            SELF_VAR_NAME, type_=struct.name, expr=malloc(struct.name))
        return_self = astlib.Return(SELF_VAR_NAME)
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
        return astlib.Func(
            name=astlib.FunctionName(
                to_init_method_name(struct.name)),
            args=args,
            type_=struct.name,
            body=body)

    def default_deinit_method(self, struct):
        frees_and_deinits = []
        for field_decl in self.only_field_decls(struct.body):
            if isinstance(field_decl.type_, astlib.TypeName):
                frees_and_deinits.append(
                    deinit(
                        struct=field_decl.type_,
                        arg=field_of_self(field_decl.name)))
        frees_and_deinits.append(free(SELF_VAR_NAME))
        body = astlib.Body(frees_and_deinits[0], astlib.Empty())
        body.extend_from_list(frees_and_deinits[1:])
        return astlib.Func(
            name=astlib.FunctionName(
                to_deinit_method_name(struct.name)),
            args=astlib.Args(
                name=SELF_VAR_NAME, type_=struct.name, rest=astlib.Empty()),
            type_=astlib.CType("Void"),
            body=body)

    def init_method(self, method, struct_name):
        declaration_of_self = astlib.Decl(
            SELF_VAR_NAME, type_=struct_name, expr=malloc(struct_name))
        return_self = astlib.Return(SELF_VAR_NAME)
        body = astlib.Body(declaration_of_self, astlib.Empty())
        body.extend(method.body)
        body.append(return_self)
        return astlib.Func(
            astlib.FunctionName(to_init_method_name(struct_name)),
            args=method.args,
            type_=struct_name,
            body=body)

    def method_to_func(self, method, struct_name):
        if method.name == defs.INIT_METHOD_NAME:
            return self.init_method(method, struct_name)
        return self.other_method(method, struct_name)

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
        yield astlib.Struct(struct.name, fields)
        have_init = False
        have_deinit = False
        have_copy = False
        have_deepcopy = False
        funcs = []
        for method in methods.as_list():
            if method.name == defs.INIT_METHOD_NAME:
                have_init = True
            elif method.name == defs.DEINIT_METHOD_NAME:
                have_deinit = True
            elif method.name == defs.COPY_METHOD_NAME:
                have_copy = True
            elif method.name == defs.DEEPCOPY_METHOD_NAME:
                have_deepcopy = True
            funcs.append(self.method_to_func(method, struct.name))

        add_funcs = []
        if not have_init:
            add_funcs.append(self.default_init_method(struct))
        if not have_deinit:
            add_funcs.append(self.default_deinit_method(struct))
        if not have_copy:
            add_funcs.append(self.default_copy_method(struct))
        if not have_deepcopy:
            add_funcs.append(self.default_deepcopy_method(struct))
        yield from add_funcs + funcs
