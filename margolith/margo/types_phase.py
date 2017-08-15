from . import layers, astlib, errors, inference
from . import cdefs, defs
from .context import context, get
from .patterns import A



class TypeInference(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        type_ = decl.type_
        if type_ in A(astlib.Empty):
            type_ = inference.infer(decl.expr)
        context.env.add(str(decl.name), {
            "type": type_
        })
        yield astlib.Decl(decl.name, type_, decl.expr)