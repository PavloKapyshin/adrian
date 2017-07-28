"""ARC."""

from . import layers, astlib, errors, defs, structs
from .context import context


class ARC(layers.Layer):

    def __init__(self):
        self.to_free = structs.Namespace()

    def no_return_in(self, body):
        current_body = body
        no_return = True
        while not isinstance(current_body, astlib.Empty):
            # When iff, els, elf we need to look in it.
            if isinstance(current_body.stmt, astlib.Return):
                no_return = False
            current_body = current_body.rest
        return no_return

    def free(self, name, type_):
        deinit_name = astlib.Name(
            "".join([
                defs.FUNC_PREFIX, str(type_.replace(defs.STRUCT_PREFIX, "", 1)),
                defs.DEINIT_METHOD_NAME]))
        return astlib.FuncCall(
            deinit_name, astlib.CallArgs(astlib.Name(name), astlib.Empty()))

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, val in sorted(space[scope].copy().items()):
            type_ = val["type_"]
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
        context.ns.add(str(decl.name), {
            "type_": decl.type_
        })
        if not isinstance(decl.expr, astlib.Ref):
            if isinstance(decl.expr, astlib.Name):
                type_ = context.ns.get(str(decl.expr))["type_"]
                if not isinstance(type_, astlib.Ref):
                    self.to_free.add(str(decl.name), {
                        "type_": decl.type_
                    })
            elif isinstance(decl.expr, astlib.FuncCall):
                type_ = context.fs.get(str(decl.expr.name))["rettype"]
                if not isinstance(type_, astlib.Ref):
                    self.to_free.add(str(decl.name), {
                        "type_": decl.type_
                    })
            else:
                self.to_free.add(str(decl.name), {
                    "type_": decl.type_
                })
        yield decl

    @layers.register(astlib.Return)
    def return_(self, return_):
        arc = self.arc()
        if isinstance(return_.expr, astlib.Name):
            for free in arc:
                if isinstance(free.args.arg, astlib.Name):
                    if not str(free.args.arg) == str(return_.expr):
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

        context.fs.add(str(func.name), {
            "rettype": func.type_,
        })

        # Adding arguments to namespace.
        args_ = func.args
        while not isinstance(args_, astlib.Empty):
            context.ns.add(str(args_.name), {
                "type_": args_.type_
            })
            args_ = args_.rest
        body = self.body(func.body)
        if self.no_return_in(body):
            arc = self.arc()
            if not isinstance(body, astlib.Empty):
                body.extend_from_list(arc)
        yield astlib.Func(
            func.name, func.args, func.type_, body)

        self.to_free.del_scope()
        context.ns.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        self.to_free.add_scope()
        context.ns.add_scope()

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