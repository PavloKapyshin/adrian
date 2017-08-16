from . import layers, astlib, errors, inference
from . import cdefs, defs
from .context import context, get
from .patterns import A



class TypeInference(layers.Layer):

    def b(self, body):
        reg = TypeInference().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        type_ = decl.type_
        if type_ in A(astlib.Empty):
            type_ = inference.infer(decl.expr)
        context.env.add(str(decl.name), {
            "type": type_
        })
        yield astlib.Decl(decl.name, type_, decl.expr)

    @layers.register(astlib.Func)
    def func(self, func):
        # We don't have type inference for func rettype, for now.
        context.env.add(str(func.name), {
            "type": func.rettype
        })
        for arg in func.args:
            context.env.add(str(arg.name), {
                "type": arg.type_
            })

        yield astlib.Func(
            func.name, func.args, func.rettype,
            self.b(func.body))