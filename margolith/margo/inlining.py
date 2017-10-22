from . import layers, astlib
from .patterns import A
from .env import Env


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

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        need_to_inline, var_types = self.check(declaration)
        if need_to_inline:
            self.inlined_structs.add(str(declaration.name), {
                "var_types": var_types,
            })
        yield declaration

    @layers.register(astlib.StructFuncDecl)
    def struct_func_decl(self, declaration):
        need_to_inline, _ = self.check(declaration)
        yield declaration
