import itertools

from . import layers, astlib, errors, defs, env, inference
from .context import context, add_to_env, add_scope, del_scope, get
from .patterns import A


def is_ctype(type_):
    return type_ in A(astlib.CType)


def return_in_func(body):
    for stmt in body:
        if stmt in A(astlib.Return):
            return True
    return False


class ARC(layers.Layer):

    def __init__(self, to_free=None, initialization_list=None):
        self.to_free = to_free or env.Env()
        self.initialization_list = initialization_list or env.Env()

    def initialized(self, variable):
        return self.initialization_list.get(str(variable))

    def add_struct_fields_to_initialized(self, variable, expr):
        struct_entry = get(str(inference.infer(expr)))
        fields = struct_entry["fields"]
        for field_name, _ in fields.items():
            # TODO: fix this shit.
            self.initialization_list.add(
                "StructMember(struct='" + str(variable) + "', member='" + field_name + "')", True)

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

    def make_expr_for_initializion_list(self, expr):
        if expr in A(astlib.CFuncCall):
            return False
        return True

    def body(self, body):
        reg = ARC(to_free=self.to_free, initialization_list=self.initialization_list).get_registry()
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
        self.initialization_list.add(
            str(declaration.name),
            self.make_expr_for_initializion_list(declaration.expr))
        if declaration.expr not in A(astlib.CFuncCall) and not is_ctype(inference.infer(declaration.expr)):
            self.add_struct_fields_to_initialized(declaration.name, declaration.expr)
        yield declaration

    @layers.register(astlib.LetDecl)
    def let_decl(self, declaration):
        add_to_env(declaration)
        self.to_free.add(str(declaration.name), {
            "type": declaration.type_,
            "is_tmp": declaration.name.is_tmp
        })
        self.initialization_list.add(
            str(declaration.name),
            self.make_expr_for_initializion_list(declaration.expr))
        if declaration.expr not in A(astlib.CFuncCall) and not is_ctype(inference.infer(declaration.expr)):
            self.add_struct_fields_to_initialized(declaration.name, declaration.expr)
        yield declaration

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        type_of_variable = inference.infer(stmt.variable)
        if not is_ctype(type_of_variable) and self.initialized(stmt.variable):
            yield self.raw_free(stmt.variable, type_of_variable)
        yield stmt
        self.initialization_list.add(
            str(stmt.variable),
            self.make_expr_for_initializion_list(stmt.expr))

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
            declaration.name, declaration.var_types,
            self.body(declaration.body))
        del_scope()
        self.to_free.del_scope()

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        yield from layers.transform_ast(ast_, registry=registry)
        yield from self.arc()
