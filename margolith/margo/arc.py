"""ARC."""

from . import layers, astlib, errors, defs, structs
from .context import context


class ARC(layers.Layer):

    def __init__(self):
        self.to_free = structs.Namespace()

    def no_return_in(self, body):
        current_body = body
        while not isinstance(current_body, astlib.Empty):
            # When iff, els, elf we need to look in it.
            if isinstance(current_body.stmt, astlib.Return):
                return False
            current_body = current_body.rest
        return True

    def free(self, name, type_):
        return astlib.MethodCall(
            astlib.Name(name), defs.DEINIT_METHOD_NAME, astlib.Empty())

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
        if isinstance(body, astlib.Empty):
            return astlib.Empty()
        gened = list(layers.transform_node(body.stmt, registry=reg))
        result = astlib.Body(gened[0], astlib.Empty())
        result.extend_from_list(gened[1:])
        result.extend(self.body(body.rest))
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
        for arg in func.args.as_list():
            context.ns.add(str(arg[0]), arg[1])
        body = self.body(func.body)
        if self.no_return_in(body):
            arc = self.arc()
            if not isinstance(body, astlib.Empty):
                body.extend_from_list(arc)
        yield astlib.Func(
            func.name, func.args, func.rettype, body)
        self.to_free.del_scope()
        context.ns.del_scope()

    @layers.register(astlib.Method)
    def method(self, method):
        self.to_free.add_scope()
        context.ns.add_scope()
        # Adding arguments to namespace.
        for arg in method.args.as_list():
            context.ns.add(str(arg[0]), arg[1])
        body = self.body(method.body)
        if self.no_return_in(body):
            arc = self.arc()
            if not isinstance(body, astlib.Empty):
                body.extend_from_list(arc)
        yield astlib.Method(
            method.name, method.args, method.rettype, body)
        self.to_free.del_scope()
        context.ns.del_scope()

    def split_body(self, body):
        fields, methods = [], []
        for stmt in body.as_list():
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
            struct.name, self.body(struct.body))

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
