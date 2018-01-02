"""
Implements Object protocol by adding and extending some methods.
Translates Methods to StructFuncs.
"""

from . import layers, astlib, defs, errors
from .context import context
from .utils import split_struct_body
from .patterns import A


SELF = astlib.Name("self")
NEW = astlib.Name("new")


def self_field(member):
    return astlib.StructMember(SELF, member)


def new_field(member):
    return astlib.StructMember(NEW, member)


def malloc(name):
    return astlib.CFuncCall(
        "malloc", [astlib.CFuncCall(
            "sizeof", [astlib.StructScalar(name)])])


def free(name):
    return astlib.CFuncCall("free", [name])


def deinit(type_, name):
    return astlib.StructFuncCall(
        type_, astlib.Name(defs.DEINIT_METHOD_NAME), [name])


def copy(type_, name):
    return astlib.StructFuncCall(type_, defs.COPY_METHOD_NAME, [name])


class ObjectProto(layers.Layer):

    def to_real_type(self, decl):
        if decl.var_types == []:
            return decl.name
        return astlib.ParameterizedType(decl.name, decl.var_types)

    def field_decls(self, body):
        return split_struct_body(body)[0]

    def default_deinit_method(self, decl):
        body = []
        for field_decl in self.field_decls(decl.body):
            body.append(
                deinit(field_decl.type_, self_field(field_decl.name)))
        body.append(free(SELF))
        return astlib.MethodDecl(
            astlib.Name(defs.DEINIT_METHOD_NAME), [],
            astlib.CType("Void"), body)

    def default_copy_method(self, decl):
        new_decl = astlib.VarDecl(
            NEW, self.to_real_type(decl), malloc(decl.name))
        return_ = astlib.Return(NEW)
        field_inits = []
        for field_decl in self.field_decls(decl.body):
            field_inits.append(
                astlib.AssignmentAndAlloc(
                    new_field(field_decl.name), field_decl.type_,
                    copy(field_decl.type_, self_field(field_decl.name))))
        return astlib.MethodDecl(
            astlib.Name(defs.COPY_METHOD_NAME), [],
            self.to_real_type(decl), [new_decl] + field_inits + [return_])

    def complete_copy_method(self, method, decl):
        errors.not_implemented("custom copy method is not supported")

    def complete_deinit_method(self, method, decl):
        errors.not_implemented("custom deinit method is not supported")

    def _common_init(self, decl, args, body):
        self_decl = astlib.VarDecl(
            SELF, self.to_real_type(decl), malloc(decl.name))
        return_ = astlib.Return(SELF)
        return astlib.MethodDecl(
            astlib.Name(defs.INIT_METHOD_NAME), args,
            self.to_real_type(decl), [self_decl] + body + [return_])

    def complete_init_method(self, method, decl):
        fields = {}
        for field_decl in self.field_decls(decl.body):
            fields[str(field_decl.name)] = field_decl.type_
        body = []
        for stmt in method.body:
            if (stmt in A(astlib.Assignment) and
                    stmt.variable in A(astlib.StructMember) and
                    stmt.variable.struct in A(astlib.Name) and
                    str(stmt.variable.struct) == "self" and
                    stmt.variable.member in A(astlib.Name)):
                type_ = fields[str(stmt.variable.member)]
                body.append(
                    astlib.AssignmentAndAlloc(
                        stmt.variable, type_, stmt.expr))
            else:
                body.append(stmt)
        return self._common_init(decl, method.args, body)

    def default_init_method(self, decl):
        field_inits, args = [], []
        for field_decl in self.field_decls(decl.body):
            args.append(
                astlib.Arg(field_decl.name, field_decl.type_))
            field_inits.append(
                astlib.AssignmentAndAlloc(
                    self_field(field_decl.name), field_decl.type_,
                    copy(field_decl.type_, field_decl.name)))
        return self._common_init(decl, args, field_inits)

    def methods_to_struct_funcs(self, methods, struct):
        for method in methods:
            if str(method.name) == defs.INIT_METHOD_NAME:
                args = []
            else:
                args = [astlib.Arg(
                    SELF, self.to_real_type(struct))]
            yield astlib.StructFuncDecl(
                struct.name, method.name, args + method.args,
                method.rettype, method.body)

    @layers.register(astlib.StructDecl)
    def struct_decl(self, decl):
        fields, methods = split_struct_body(decl.body)
        dict_for_haves = {
            key: (False, default, complete)
            for key, default, complete in (
                (defs.INIT_METHOD_NAME, self.default_init_method,
                    self.complete_init_method),
                (defs.DEINIT_METHOD_NAME, self.default_deinit_method,
                    self.complete_deinit_method),
                (defs.COPY_METHOD_NAME, self.default_copy_method,
                    self.complete_copy_method))
        }
        new_methods = []
        for method in methods:
            if str(method.name) in dict_for_haves:
                entry = dict_for_haves[str(method.name)]
                dict_for_haves[str(method.name)] = (True, entry[1], entry[2])
                complete_func = entry[2]
                new_methods.append(complete_func(method, decl))
            else:
                new_methods.append(method)

        additional_methods = []
        for _, (exists, default, _) in sorted(dict_for_haves.items()):
            if not exists:
                additional_methods.append(default(decl))

        yield astlib.StructDecl(decl.name, decl.var_types, fields)
        yield from self.methods_to_struct_funcs(
            additional_methods + new_methods, decl)
