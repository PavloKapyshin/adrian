from . import astlib, layers, utils, inference
from .context import context
from .utils import A


def _for_env(type_):
    if type_ in A(astlib.ParamedType):
        return type_.type_
    return type_


class _CoreInlining(layers.Layer):

    def __init__(self, tmapping=None, amapping=None):
        self.type_mapping = tmapping
        self.arg_mapping = amapping
        self.b = layers._b(_CoreInlining, tmapping=tmapping, amapping=amapping)

    def _e_callable(self, stmt):
        if stmt.callabletype == astlib.CallableT.cfunc:
            return astlib.Callable(
                stmt.callabletype, stmt.parent, stmt.name,
                self.a(stmt.args))
        if stmt.callabletype == astlib.CallableT.struct_func:
            return astlib.Callable(
                stmt.callabletype, self.t(stmt.parent), stmt.name,
                self.a(stmt.args))
        return stmt

    def t(self, type_):
        if type_ in A(astlib.ParamedType):
            return astlib.ParamedType(
                type_.type_, [self.t(param) for param in type_.params])
        if type_ in A(astlib.Name):
            result = self.type_mapping.get(type_)
            return (result if result else type_)
        return type_

    def e(self, expr):
        if expr in A(astlib.Callable):
            return self._e_callable(expr)
        if expr in A(astlib.Name):
            result = self.arg_mapping.get(expr)
            return (result if result else expr)
        if expr in A(astlib.StructScalar):
            return astlib.StructScalar(self.t(expr.type_))
        return expr

    def a(self, args):
        return [self.e(arg) for arg in args]

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            self.e(stmt.left), stmt.op, self.e(stmt.right))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield self._e_callable(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        yield astlib.Decl(
            stmt.decltype, stmt.name, self.t(stmt.type_),
            self.e(stmt.expr))

    def main(self, stmt, type_mapping, arg_mapping):
        result = layers.transform_ast(
            [stmt], registry=_CoreInlining(
                tmapping=type_mapping, amapping=arg_mapping).get_registry())
        yield from list(result)


class Inlining(layers.Layer):

    def __init__(self, inliner=None, type_mapping=None, arg_mapping=None):
        self.inliner = inliner or _CoreInlining()
        self.b = layers._b(Inlining, inliner=self.inliner,
            type_mapping=type_mapping, arg_mapping=arg_mapping)
        self.type_mapping = type_mapping or {}
        self.arg_mapping = arg_mapping or {}

    # Inlining
    def add_mappings(self, type_mapping, arg_mapping):
        self.b = layers._b(
            Inlining, inliner=self.inliner, type_mapping=type_mapping,
            arg_mapping=arg_mapping)

    def mapping(self, params, type_):
        i = 0
        mapping = {}
        for param in params:
            mapping[param] = type_.params[i]
            i += 1
        return mapping

    def arg_mapping_(self, decl_args, args):
        i = 0
        mapping = {}
        for arg, _ in decl_args:
            mapping[arg] = args[i]
            i += 1
        return mapping

    def inline_struct_call(self, type_, call):
        type_mapping = self.mapping(
            context.env[call.name]["params"], type_)
        init_info = context.env[call.name]["methods"]["__init__"]
        arg_mapping = self.arg_mapping_(
            init_info["args"], call.args)
        +context.env
        self.add_mappings(type_mapping, arg_mapping)
        body = self.b(init_info["body"])
        -context.env
        new_body = []
        for stmt in body:
            new_body.extend(
                self.inliner.main(stmt, type_mapping, arg_mapping))
        expr = None
        if new_body and new_body[-1] in A(astlib.Return):
            expr = new_body[-1].expr
            new_body = new_body[:-1]
        return expr, new_body

    def inline_struct_func_call(self, type_, call):
        type_mapping = self.mapping(
            context.env[_for_env(call.parent)]["params"], type_)
        method_info = context.env[_for_env(call.parent)]["methods"][call.name]
        arg_mapping = self.arg_mapping_(method_info["args"], call.args)
        +context.env
        self.add_mappings(type_mapping, arg_mapping)
        body = self.b(method_info["body"])
        -context.env
        new_body = []
        for stmt in body:
            new_body.extend(
                self.inliner.main(stmt, type_mapping, arg_mapping))
        expr = None
        if new_body and new_body[-1] in A(astlib.Return):
            expr = new_body[-1].expr
            new_body = new_body[:-1]
        return expr, new_body

    # Misc.
    def replace_type(self, type_):
        if type_ in A(astlib.Name):
            if type_ in self.type_mapping:
                return self.type_mapping[type_]
        if type_ in A(astlib.ParamedType):
            return astlib.ParamedType(
                type_.type_, [self.replace_type(t) for t in type_.params])
        return type_

    def replace_arg(self, arg):
        if arg in A(astlib.Name):
            if arg in self.arg_mapping:
                return self.arg_mapping[arg]
        if arg in A(astlib.Callable):
            parent = self.replace_type(arg.parent)
            return astlib.Callable(
                arg.callabletype, parent, arg.name,
                [self.replace_arg(a) for a in arg.args])
        if arg in A(astlib.DataMember):
            return astlib.DataMember(
                arg.datatype, self.replace_arg(arg.parent), arg.member)
        return arg

    def _e_callable(self, type_, expr):
        if expr.callabletype == astlib.CallableT.struct:
            struct_info = context.env[expr.name]
            if struct_info["params"]:
                return self.inline_struct_call(type_, expr)
        if expr.callabletype == astlib.CallableT.struct_func:
            if (_for_env(expr.parent) in context.env and
                    utils.is_real_type(_for_env(expr.parent))):
                parent = expr.parent
            else:
                parent = self.type_mapping[expr.parent]
            struct_info = context.env[_for_env(parent)]
            if self.arg_mapping:
                args = []
                for arg in expr.args:
                    args.append(self.replace_arg(arg))
                expr = astlib.Callable(
                    expr.callabletype, parent, expr.name, args)
            if struct_info["params"]:
                return self.inline_struct_func_call(type_, expr)
        #elif expr.callabletype == astlib.CallableT.struct_func:
        return expr, []

    # Inner translation
    def e(self, type_, expr):
        if expr in A(astlib.Callable):
            return self._e_callable(type_, expr)
        if expr in A(astlib.DataMember):
            return expr, []
        return expr, []

    # Core
    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        if stmt.callabletype == astlib.CallableT.struct_func:
            expr, stmts = self._e_callable(stmt.parent, stmt)
            yield from stmts
            if expr:
                yield expr
        else:
            yield stmt

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        type_ = None
        if stmt.left in A(astlib.Name):
            type_ = context.env[stmt.left]["type_"]
        elif stmt.left in A(astlib.DataMember):
            type_ = context.env[_for_env(
                context.env[stmt.left.parent]["type_"]
                )]["fields"][stmt.left.member]
        expr, stmts = self.e(type_, stmt.right)
        yield from stmts
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield stmt

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            context.env.update(context.parent, {
                "fields": utils.add_dicts(
                    context.env[context.parent]["fields"], {
                    stmt.name : {
                        "type_": stmt.type_
                    }
                })
            })
            yield stmt
        else:
            context.env[stmt.name] = {
                "node_type": utils.declt_to_nodet(stmt.decltype),
                "type_": stmt.type_
            }
            expr, stmts = self.e(stmt.type_, stmt.expr)
            yield from stmts
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct_func:
            context.env.update(stmt.parent, {
                "methods": utils.add_dicts(context.env[stmt.parent], {
                    stmt.name : {
                        "type_": stmt.rettype,
                        "args": stmt.args,
                        "body": stmt.body
                    }
                })
            })
        yield stmt

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        context.env[stmt.name] = {
            "params": stmt.params,
            "node_type": utils.declt_to_nodet(stmt.decltype),
            "fields": {},
            "methods": {}
        }
        +context.env
        context.parent = stmt.name
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
