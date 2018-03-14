from . import layers, astlib, errors, defs
from .context import context
from .utils import A, split_body, only_fields


SELF = astlib.Name("self")


def totype(struct_decl):
    if len(struct_decl.params) != 0:
        return astlib.ParamedType(struct_decl.name, struct_decl.params)
    return struct_decl.name


def field_maker(struct_name):
    def wrapper(field_name):
        return astlib.DataMember(
            astlib.DataT.struct, struct_name, field_name)
    return wrapper

self_field = field_maker(SELF)


def copy(type_, name):
    return astlib.Callable(
        astlib.CallableT.struct_func,
        type_, defs.COPY_METHOD, [name])

def malloc(name):
    return astlib.Callable(
        astlib.CallableT.cfunc, astlib.Empty(), astlib.Name("malloc"),
        [astlib.Callable(
            astlib.CallableT.cfunc, astlib.Empty(), astlib.Name("sizeof"),
            [astlib.StructScalar(name)])])


def free(name):
    return astlib.Callable(
        astlib.CallableT.cfunc, astlib.Empty(),
        astlib.Name("free"), [name])


def deinit(struct, name):
    return astlib.Callable(
        astlib.CallableT.struct_func, struct,
        astlib.Name(defs.DEINIT_METHOD), [name])


def mtostructf(method, struct):
    return astlib.CallableDecl(
        astlib.DeclT.struct_func, struct.name,
        method.name, method.args, method.rettype,
        method.body)


class ObjectProtocol(layers.Layer):

    def complete_init_method(self, method, stmt):
        errors.not_now(errors.CUSTOM_OBJMETHOD)

    def complete_deinit_method(self, method, stmt):
        errors.not_now(errors.CUSTOM_OBJMETHOD)

    def complete_copy_method(self, method, stmt):
        errors.not_now(errors.CUSTOM_OBJMETHOD)

    def default_deinit_method(self, stmt):
        body = [deinit(
            field_decl.type_, self_field(field_decl.name))
            for field_decl in only_fields(stmt.body)] + [free(SELF)]
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(),
            astlib.Name(defs.DEINIT_METHOD),
            [astlib.Arg(SELF, totype(stmt))],
            astlib.Name("Void"), body)

    def default_copy_method(self, stmt):
        new = astlib.Name("new")
        field_of_new = field_maker(new)
        new_decl = astlib.Decl(
            astlib.DeclT.var, new, totype(stmt), malloc(stmt.name))
        field_inits = []
        for field_decl in only_fields(stmt.body):
            field_inits.append(astlib.Assignment(
                field_of_new(field_decl.name), field_decl.type_,
                copy(field_decl.type_,
                    self_field(field_decl.name))))
        return_new = astlib.Return(new)
        body = [new_decl] + field_inits + [return_new]
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(),
            astlib.Name(defs.COPY_METHOD),
            [astlib.Arg(SELF, totype(stmt))],
            totype(stmt), body)

    def default_init_method(self, stmt):
        self_decl = astlib.Decl(
            astlib.DeclT.var, SELF, totype(stmt), malloc(stmt.name))
        return_self = astlib.Return(SELF)
        field_inits, args = [], []
        for field_decl in only_fields(stmt.body):
            args.append(astlib.Arg(field_decl.name, field_decl.type_))
            field_inits.append(
                astlib.Assignment(
                    self_field(field_decl.name), "=",
                    copy(field_decl.type_, field_decl.name)))
        body = [self_decl] + field_inits + [return_self]
        rettype = totype(stmt)
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(),
            astlib.Name(defs.INIT_METHOD),
            args, rettype, body)

    def method(self, method, struct):
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(), method.name,
            [astlib.Arg(SELF, totype(struct))] + method.args,
            method.rettype, method.body)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct:
            fields, methods = split_body(stmt.body)
            have = {
                key: (
                    False,
                    getattr(self, "_".join(["complete", mname])),
                    getattr(self, "_".join(["default", mname])))
                for key, mname in (
                    (defs.INIT_METHOD, "init_method"),
                    (defs.DEINIT_METHOD, "deinit_method"),
                    (defs.COPY_METHOD, "copy_method"),
                )
            }
            new_methods = []
            for method in methods:
                entry = have.get(str(method.name))
                if entry:
                    have[str(method.name)] = (True, entry[1], entry[2])
                    new_methods.append(entry[1](method, stmt))
                else:
                    new_methods.append(self.method(method, stmt))
            additional_methods = []
            for method_name, (exists, _, f) in sorted(have.items()):
                if not exists:
                    additional_methods.append(f(stmt))
            yield astlib.DataDecl(
                stmt.decltype, stmt.name, stmt.params, fields)
            for method in additional_methods + new_methods:
                yield mtostructf(method, stmt)
        else:
            yield stmt
