from . import astlib, layers, errors, defs, utils
from .context import context
from .utils import A


SELF = astlib.Name("self")


def field_of_self(field):
    return astlib.StructField(
        SELF, field)


def to_type(struct_decl):
    if struct_decl.parameters:
        return astlib.GenericType(struct_decl.name, struct_decl.parameters)
    return struct_decl.name


def complete_init_method(struct_decl, method_decl):
    rettype = to_type(struct_decl)
    return astlib.StructFuncDecl(
        method_decl.name, method_decl.args, rettype,
        ([astlib.VarDecl(SELF, rettype, astlib.Alloc(rettype))] +
            method_decl.body + [astlib.Return(SELF)]))


def default_init_method(struct_decl):
    rettype = to_type(struct_decl)
    fields, _ = utils.split_body(struct_decl.body)
    field_inits, args = [], []
    for field in fields:
        field_inits.append(
            astlib.Assignment(field_of_self(field.name), "=", field.name))
        args.append((field.name, field.type_))
    return astlib.StructFuncDecl(
        astlib.Name(defs.INIT_METHOD), args, rettype,
        ([astlib.VarDecl(SELF, rettype, astlib.Alloc(rettype))] +
            field_inits + [astlib.Return(SELF)]))


def translate_method(struct_decl, method_decl):
    return astlib.StructFuncDecl(
        method_decl.name, [(SELF, to_type(struct_decl))] + method_decl.args,
        method_decl.rettype, method_decl.body)


class ObjectProtocol(layers.Layer):

    @layers.register(astlib.ExtensionDecl)
    def extension_decl(self, stmt):
        yield astlib.ExtensionDecl(
            stmt.name, stmt.parameters, stmt.protocols,
            [translate_method(stmt, method) for method in stmt.body])

    @layers.register(astlib.StructDecl)
    def struct_decl(self, stmt):
        fields, methods = utils.split_body(stmt.body)
        has_init_method = False
        translated_methods = []
        for method in methods:
            if method.name == defs.INIT_METHOD:
                has_init_method = True
                translated_methods.append(
                    complete_init_method(stmt, method))
            else:
                translated_methods.append(
                    translate_method(stmt, method))

        if not has_init_method:
            translated_methods = [
                default_init_method(stmt)] + translated_methods

        yield astlib.StructDecl(
            stmt.name, stmt.parameters, stmt.protocols,
            fields + translated_methods)
