"""ARC."""

from . import layers, astlib, errors, defs, structs
from .context import context


class ARC(layers.Layer):

    def __init__(self):
        self.to_free = structs.Namespace()

    def no_return_in(self, body):
        for stmt in body:
            # When iff, els, elf we need to look in it.
            if isinstance(stmt, astlib.Return):
                return False
        return True

    def free(self, name, type_):
        return astlib.MethodCall(
            astlib.Name(name), defs.DEINIT_METHOD_NAME, [])

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, type_ in sorted(space[scope].copy().items()):
            if not isinstance(type_, astlib.CType):
                yield self.free(key, type_)

    def body(self, body):
        new_instance = ARC()
        new_instance.to_free = self.to_free
        reg = new_instance.get_registry()
        result = []
        for stmt in body:
            result.extend(
                list(layers.transform_node(stmt, registry=reg)))
        return result

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.ns.add(str(decl.name), decl.type_)
        if not isinstance(decl.expr, astlib.Ref):
            type_ = decl.type_
            if isinstance(decl.expr, astlib.FuncCall):
                type_ = context.fs.get(str(decl.expr.name))
            elif isinstance(decl.expr, astlib.MethodCall):
                base_type = context.ns.get(str(decl.expr.base))
                if isinstance(base_type, astlib.Ref):
                    base_type = base_type.literal
                method_to_rettype = context.ts.get(str(base_type))
                type_ = method_to_rettype[str(decl.expr.method)]
            if not isinstance(type_, astlib.Ref):
                self.to_free.add(str(decl.name), decl.type_)
        yield decl

    @layers.register(astlib.Return)
    def return_(self, return_):
        arc = self.arc()
        if isinstance(return_.expr, astlib.Name):
            for free in arc:
                if isinstance(free.base, astlib.Name):
                    if not str(free.base) == str(return_.expr):
                        yield free
                else:
                    yield free
        else:
            yield from arc
        yield return_

    @layers.register(astlib.Func)
    def func(self, func):
        self.to_free.add_scope()
        context.ns.add_scope()
        context.fs.add(str(func.name), func.rettype)
        # Adding arguments to namespace.
        for arg in func.args:
            context.ns.add(str(arg.name), arg.type_)
        body = self.body(func.body)
        if self.no_return_in(body):
            arc = self.arc()
            body.extend(arc)
        yield astlib.Func(
            func.name, func.args, func.rettype, body)
        self.to_free.del_scope()
        context.ns.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        self.to_free.add_scope()
        context.ns.add_scope()
        # Adding arguments to namespace.
        for arg in method.args:
            context.ns.add(str(arg.name), arg.type_)
        body = self.body(method.body)
        if self.no_return_in(body):
            arc = self.arc()
            body.extend(arc)
        yield astlib.Method(
            method.name, method.args, method.rettype, body)
        self.to_free.del_scope()
        context.ns.del_scope()

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body:
            if isinstance(stmt, astlib.Field):
                fields.append(stmt)
            else:
                methods.append(stmt)
        return fields, methods

    @layers.register(astlib.Struct)
    def struct(self, struct):
        self.to_free.add_scope()
        context.ns.add_scope()

        fields, methods = self.split_body(struct.body)
        method_to_rettype = {}
        for method in methods:
            method_to_rettype[str(method.name)] = method.rettype
        context.ts.add(str(struct.name), method_to_rettype)

        yield astlib.Struct(
            struct.name, struct.param_types, self.body(struct.body))

        self.to_free.del_scope()
        context.ns.del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        for node in ast_:
            node_func = registry.get(type(node))
            if node_func:
                yield from list(node_func(node))
            else:
                yield node
        yield from self.arc()
