from collections import OrderedDict

from . import layers, astlib, errors, defs
from .utils import A, split_body, only_fields
from .context import context


SELF = astlib.Name("self")


def totype(struct_decl):
    params = struct_decl.params
    if params:
        return astlib.GenericType(struct_decl.name, params)
    return struct_decl.name


def field_maker(struct_name):
    def wrapper(field_name):
        return astlib.DataMember(
            astlib.DataT.struct, struct_name, field_name)
    return wrapper

self_field = field_maker(SELF)


def init(type_, args):
    return astlib.Callable(
        astlib.CallableT.struct_func,
        type_, defs.INIT_METHOD, args)


def mtosf(method, struct):
    args = method.args
    body = method.body
    return astlib.CallableDecl(
        astlib.DeclT.struct_func, struct.name,
        method.name, args, method.rettype, body)


class ObjectProtocol(layers.Layer):

    def __init__(self):
        self.f_count = 0

    def complete_init_method(self, method, stmt):
        errors.later(errors.Version.v0m9.value)

    def default_init_method(self, stmt):
        self_decl = astlib.Decl(
            astlib.DeclT.var, SELF, totype(stmt), astlib.Alloc())
        return_self = astlib.Return(SELF)
        field_inits, args = [], []
        for field_decl in only_fields(stmt.body):
            args.append((field_decl.name, field_decl.type_))
            field_inits.append(
                astlib.Assignment(
                    self_field(field_decl.name), "=", field_decl.name))
        body = [self_decl] + field_inits + [return_self]
        rettype = totype(stmt)
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(),
            astlib.Name(defs.INIT_METHOD),
            args, rettype, body)

    def method(self, method, struct):
        args = method.args
        body = method.body
        return astlib.CallableDecl(
            astlib.DeclT.method, astlib.Empty(), method.name,
            [(SELF, totype(struct))] + args,
            method.rettype, body)

    def provide_methods_for_adt(self, adt_decl, adt_fields):
        adt_as_type = totype(adt_decl)
        def _create_conditional(type_, stmt):
            return astlib.Cond(
                if_=astlib.If(astlib.Is(SELF, type_), [stmt]),
                elifs_=[], else_=[])

        def _init_adt():
            ret = astlib.Return(astlib.Alloc())
            return astlib.CallableDecl(
                astlib.DeclT.struct_func, adt_decl.name,
                astlib.Name(defs.INIT_METHOD),
                [], adt_as_type, [ret])

        return [_init_adt()]

    def make_initial_impl_dict(self):
        return OrderedDict(sorted({
            key: (
                i, False,
                getattr(self, "_".join(["complete", mname])),
                getattr(self, "_".join(["default", mname])))
            for i, key, mname in (
                (1, defs.INIT_METHOD, "init_method"),
            )
        }.items(), key=lambda x: x[1]))

    def _provide_name_for_adt_field(self):
        name = astlib.Name("".join([defs.F_STRING, str(self.f_count)]),
            is_user_name=False)
        self.f_count += 1
        return name

    def _make_adt_body(self, body):
        return [
            astlib.Decl(astlib.DeclT.field, self._provide_name_for_adt_field(),
                type_, astlib.Empty()) for type_ in body]

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        fields, methods = split_body(stmt.body)
        implemented_methods = self.make_initial_impl_dict()
        new_methods = []
        for method in methods:
            entry = implemented_methods.get(str(method.name))
            if entry:
                implemented_methods.update(
                    {str(method.name): (True, entry[1], entry[2])})
                new_methods.append(entry[1](method, stmt))
            else:
                new_methods.append(self.method(method, stmt))
        object_protocol_methods = []
        for _, (_, exists, _, f) in implemented_methods.items():
            if not exists:
                object_protocol_methods.append(f(stmt))
        yield astlib.StructDecl(
            stmt.name, stmt.params, stmt.protocols,
            fields + [
                mtosf(method, stmt)
                for method in object_protocol_methods + new_methods])

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.protocol:
            yield stmt
        else:
            fields = self._make_adt_body(stmt.body)
            yield astlib.DataDecl(
                stmt.decltype, stmt.name, stmt.params,
                fields + self.provide_methods_for_adt(stmt, fields))
