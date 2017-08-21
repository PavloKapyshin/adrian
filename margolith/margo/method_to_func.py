from . import layers, astlib, errors, defs, inference
from .patterns import A
from .context import context



def split_body(body):
    fields, methods = [], []
    for stmt in body:
        if stmt in A(astlib.Field):
            fields.append(stmt)
        else:
            methods.append(stmt)
    return fields, methods


class MethodToFunc(layers.Layer):

    def __init__(self, struct_name=None):
        self.struct_name = struct_name

    def decl_args(self, name, args):
        if str(name) == defs.INIT_METHOD_NAME:
            return args
        return [astlib.Arg("self", self.struct_name)] + args


    def call_args(self, args):
        return list(map(self.e, args))

    def e(self, expr):
        if expr in A(astlib.StructCall):
            return astlib.FuncCall(
                "_".join([str(expr.name), defs.INIT_METHOD_NAME]),
                expr.args)

        if expr in A(astlib.MethodCall):
            return list(self.method_call(expr))[0]
        return expr

    def b(self, body):
        reg = MethodToFunc(struct_name=self.struct_name).get_registry()
        return list(map(
            lambda stmt: list(layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
            "type": decl.type_
        })
        yield astlib.Decl(
            decl.name, decl.type_, self.e(decl.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, assment):
        yield astlib.Assignment(
            assment.var, assment.op, self.e(assment.expr))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(self.e(return_.expr))

    @layers.register(astlib.Method)
    def method(self, method):
        new_name = astlib.Name("_".join([str(self.struct_name), str(method.name)]))
        context.env.add_scope()
        context.env.add(str(new_name), {
            "type": method.rettype
        })
        yield astlib.Func(
            new_name,
            self.decl_args(method.name, method.args),
            method.rettype, self.b(method.body))
        context.env.del_scope()

    @layers.register(astlib.MethodCall)
    def method_call(self, call):
        struct_name = inference.infer(call.base)
        yield astlib.FuncCall(
            "_".join([str(struct_name), str(call.method)]),
            self.call_args([call.base] + call.args))

    @layers.register(astlib.Func)
    def func(self, func):
        context.env.add_scope()
        context.env.add(str(func.name), {
            "type": func.rettype
        })
        yield astlib.Func(
            func.name, func.args, func.rettype, self.b(func.body))
        context.env.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        self.struct_name = struct.name
        context.env.add_scope()
        context.env.add(str(struct.name), {
            "type": struct.name
        })
        fields, methods = split_body(struct.body)

        yield astlib.Struct(
            struct.name, struct.parameters, struct.protocols,
            fields)

        yield from self.b(methods)
        context.env.del_scope()
