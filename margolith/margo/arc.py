import itertools

from . import layers, astlib, errors, defs, env, inference
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


def is_ctype(type_):
    return type_ in A(astlib.CType)


def return_in_func(body):
    for stmt in body:
        if stmt in A(astlib.Return):
            return True
    return False


class ARC(layers.Layer):

    def __init__(self, to_free=None, expressions=None):
        self.to_free = to_free or env.Env()
        self.expressions = expressions or env.Env()

    def raw_free(self, arg, type_):
        if type_ in A(astlib.CType):
            return astlib.CFuncCall(
                "free", [arg])
        if type_ in A(astlib.Name):
            return astlib.StructFuncCall(
                type_, defs.DEINIT_METHOD_NAME, args=[arg])
        errors.not_implemented(
            context.exit_on_error, "can't free")

    def free(self, name, val):
        return self.raw_free(
            astlib.Name(name, is_tmp=val["is_tmp"]),
            val["type"])

    def arc(self):
        space = self.to_free.space()
        scope = self.to_free.scope
        for key, val in sorted(space[scope].copy().items()):
            yield self.free(key, val)

    def body(self, body):
        reg = ARC(to_free=self.to_free, expressions=self.expressions).get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        add_to_env(declaration)
        self.to_free.add(str(declaration.name), {
            "type": declaration.type_,
            "is_tmp": declaration.name.is_tmp
        })
        expr = declaration.expr
        if expr in A(astlib.CFuncCall):
            expr = None
        self.expressions.add(str(declaration.name), expr)
        yield declaration

    @layers.register(astlib.LetDecl)
    def let_decl(self, declaration):
        add_to_env(declaration)
        self.to_free.add(str(declaration.name), {
            "type": declaration.type_,
            "is_tmp": declaration.name.is_tmp
        })
        expr = declaration.expr
        if expr in A(astlib.CFuncCall):
            expr = None
        self.expressions.add(str(declaration.name), expr)
        yield declaration

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr = stmt.expr
        if expr in A(astlib.CFuncCall):
            expr = None
        if expr in A(astlib.Deref):
            expr = expr.expr
        if expr is None:
            type_ = astlib.CType("dummy")
        else:
            type_ = inference.infer(expr)
        if is_ctype(type_):
            yield stmt
        else:
            if self.expressions.get(str(stmt.variable)) is not None:
                yield self.raw_free(stmt.variable, type_)
            yield stmt
        self.expressions.add(str(stmt.variable), expr)

    @layers.register(astlib.Return)
    def return_(self, stmt):
        if stmt.expr in A(astlib.Name):
            # TODO: when multiply returns in one function
            # we need to free this variable in another return.
            self.to_free.del_(str(stmt.expr))
        yield from self.arc()
        yield stmt

    @layers.register(astlib.FuncDecl)
    def func(self, declaration):
        self.to_free.add_scope()
        add_to_env(declaration)
        add_scope()
        body = self.body(declaration.body)
        if not return_in_func(body):
            body.extend(self.arc())
        yield astlib.FuncDecl(
            declaration.name, declaration.args,
            declaration.rettype, body)
        del_scope()
        self.to_free.del_scope()

    @layers.register(astlib.StructFuncDecl)
    def struct_func(self, declaration):
        self.to_free.add_scope()
        add_to_env(declaration)
        add_scope()
        body = self.body(declaration.body)
        if not return_in_func(body):
            body.extend(self.arc())
        yield astlib.StructFuncDecl(
            declaration.struct, declaration.func,
            declaration.args, declaration.rettype, body)
        del_scope()
        self.to_free.del_scope()

    @layers.register(astlib.StructDecl)
    def struct(self, declaration):
        self.to_free.add_scope()
        add_to_env(declaration)
        add_scope()
        yield astlib.StructDecl(
            declaration.name, self.body(declaration.body))
        del_scope()
        self.to_free.del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()
