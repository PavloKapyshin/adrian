import sys

from . import layers, astlib, errors
from .context import context, get, add_to_env
from .patterns import A
from .env import Env


class Fixer:
    def fix(self, body):
        pass


class Applier:
    def apply(self, func, type_dict, arg_dict):
        pass


class Inlining(layers.Layer):

    def __init__(self, inlined_structs=None):
        self.inlined_structs = inlined_structs or Env()

    def check(self, declaration):
        if declaration in A(astlib.StructDecl):
            return declaration.var_types != [], declaration.var_types
        if declaration in A(astlib.StructFuncDecl):
            return self.inlined_structs.exists(
                str(declaration.struct)), []
        return False, []

    def replace_field_types(self, declaration):
        body = []
        for field_decl in declaration.body:
            if str(field_decl.type_) in declaration.var_types:
                type_ = astlib.CObject()
            else:
                type_ = field_decl.type_
            body.append(astlib.FieldDecl(field_decl.name, type_))
        return astlib.StructDecl(
            declaration.name, declaration.var_types, body)

    def need_to_inline(self, expr):
        if expr in A(astlib.StructCall):
            return self.inlined_structs.exists(str(expr.name))
        return False

    def inline(self, type_, expr):
        if expr in A(astlib.StructCall):
            if type_ in A(astlib.ParameterizedType):
                type_dict = {}
                entry = get(type_.type_)
                for parameter_name, parameter_value in zip(entry["var_types"], type_.parameters):
                    type_dict[str(parameter_name)] = parameter_value

                arg_dict = {}
                init_method = entry["methods"]["__init__"]
                for arg_name, arg_value in zip([arg.name for arg in init_method["args"]], expr.args):
                    arg_dict[str(arg_name)] = arg_value
                print("GOT (type dict): ", type_dict, file=sys.stderr)
                print("GOT (arg dict): ", arg_dict, file=sys.stderr)
            else:
                errors.not_implemented(
                    context.exit_on_error,
                    "no need to inline, but it is queried")

        return [], expr

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        if self.need_to_inline(declaration.expr):
            inlined_body, expr = self.inline(declaration.type_, declaration.expr)
            yield from inlined_body
            yield astlib.VarDecl(declaration.name, declaration.type_, expr)
        else:
            yield declaration

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        add_to_env(declaration)
        need_to_inline, var_types = self.check(declaration)
        if need_to_inline:
            self.inlined_structs.add(str(declaration.name), {
                "var_types": var_types,
                "funcs": {}
            })
            yield self.replace_field_types(declaration)
        else:
            yield declaration

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, declaration):
        add_to_env(declaration)
        need_to_inline, _ = self.check(declaration)
        if need_to_inline:
            entry = self.inlined_structs.get(str(declaration.struct))
            if entry:
                funcs = entry["funcs"]
                funcs[str(declaration.func)] = declaration
                self.inlined_structs.add(str(declaration.struct), {
                    "var_types": entry["var_types"],
                    "funcs": funcs
                })
            else:
                errors.not_implemented(
                    context.exit_on_error, "unknown error")
            yield from []
        else:
            yield declaration
