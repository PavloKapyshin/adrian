"""ARC."""

from . import layers, astlib, errors, defs
from .context import context


class ARC(layers.Layer):

    def free(self, name, type_):
        deinit_name = astlib.FunctionName(
            "".join([defs.DEINIT_METHOD_NAME, str(type_)]))
        return astlib.FuncCall(
            deinit_name,
            astlib.CallArgs(astlib.VariableName(name), astlib.Empty()))

    def arc(self):
        space = context.ns.space()
        scope = context.ns.scope
        for key, val in sorted(space[scope].copy().items()):
            type_ = val["type_"]
            if not isinstance(type_, astlib.CType):
                yield self.free(key, type_)

    def body(self, body):
        reg = ARC().get_registry()
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
            "type_": decl.type_,
            "expr": decl.expr
        })
        yield decl

    @layers.register(astlib.Return)
    def return_(self, return_):
        arc = self.arc()
        # Deleting from arc's result deinit of return's expr.
        if isinstance(return_.expr, astlib.VariableName):
            for free in arc:
                if isinstance(free.args.arg, astlib.VariableName):
                    if not str(free.args.arg) == str(return_.expr):
                        yield free
                else:
                    yield free
        else:
            yield from arc
        yield return_

    @layers.register(astlib.Func)
    def func(self, func):
        context.ns.add_scope()
        yield astlib.Func(
            func.name, func.args, func.type_,
            self.body(func.body))
        context.ns.del_scope()

    @layers.register(astlib.Struct)
    def struct(self, struct):
        context.ns.add_scope()
        yield astlib.Struct(
            struct.name, self.body(struct.body))
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