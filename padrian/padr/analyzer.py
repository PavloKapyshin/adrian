import itertools

from .context import context
from . import astlib, layers, errors, defs, inference, utils
from .utils import A


class Analyzer(layers.Layer):

    def __init__(self):
        self.b = layers._b(Analyzer)

    # Misc.
    def construct_type(self, tname):
        params = context.env[tname]["params"]
        if len(params) == 0:
            return tname
        return astlib.ParamedType(tname, params)

    def t_e(self, type_, expr):
        if not type_:
            expr = self.e(expr)
            return inference.infer_type(expr), expr
        elif not expr:
            type_ = self.t(type_)
            return type_, inference.infer_expr(type_)
        return self.t(type_), self.e(expr)

    def e_callable(self, callable_):
        callabletype = callable_.callabletype
        if (callabletype not in (
                    astlib.CallableT.cfunc, astlib.CallableT.struct_func)
                and utils.is_type(callable_.name)):
            callabletype = astlib.CallableT.struct
        return astlib.Callable(
            callabletype, callable_.parent,
            callable_.name, self.a(callable_.args))

    def e_struct_member(self, expr):
        if expr.member in A(astlib.Callable):
            call = expr.member
            parent = self.e(expr.parent)
            return astlib.Callable(
                astlib.CallableT.struct_func,
                inference.infer_type(parent),
                call.name, [parent] + self.a(call.args))
        return astlib.DataMember(
            expr.datatype, self.e(expr.parent),
            expr.member)

    # Inner ast nodes translation.
    def e(self, expr):
        # TODO:
        #   * add a lot of everything...
        if expr in A(astlib.Callable):
            return self.e_callable(expr)
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return self.e_struct_member(expr)
        if expr in A(astlib.Expr):
            # TODO:
            #   * Expr should be translated to struct_func calls:
            #       1 + 1 -> S.__add__(1, 1)
            return expr
        return expr

    def t(self, type_):
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                if str(type_.parent) != defs.CMODULE:
                    errors.not_now(errors.MODULE)
                return type_
        if type_ in A(astlib.ParamedType):
            params = type_.params
            if params in A(astlib.Empty):
                params = []
            else:
                params = [self.t(t) for t in params]
            return astlib.ParamedType(self.t(type_.type_), params)
        return type_

    def a(self, args):
        if args in A(astlib.Empty):
            return []
        elif args[0] in A(astlib.Arg):
            return [(arg.name, self.t(arg.type_)) for arg in args]
        return [self.e(arg) for arg in args]

    def p(self, params):
        if params in A(astlib.Empty):
            return []
        return params

    # Subcore funcs.
    def field_decl(self, stmt):
        # TODO?:
        #   * add stmt.parent to stmt
        type_ = self.t(stmt.type_)
        context.env.update(context.parent, {
            "fields": utils.add_dicts(
                context.env[context.parent]["fields"], {
                stmt.name: {
                    "type_": type_
                }
            })
        })
        yield astlib.Decl(
            stmt.decltype, stmt.name, type_, stmt.expr)

    def var_let_decl(self, stmt):
        type_, expr = self.t_e(stmt.type_, stmt.expr)
        context.env[stmt.name] = {
            "node_type": utils.declt_to_nodet(stmt.decltype),
            "type_": type_
        }
        yield astlib.Decl(
            stmt.decltype, stmt.name, type_, expr)

    def struct_func_decl(self, stmt):
        type_ = self.t(stmt.rettype)
        args = self.a(stmt.args)
        context.env.update(stmt.parent, {
            "methods": utils.add_dicts(
                context.env[stmt.parent]["methods"], {
                stmt.name: {
                    "type_": type_,
                    "args": args
                }
            })
        })
        +context.env
        for name, type_ in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": type_
            }
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        -context.env

    # Core funcs.
    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            self.e(stmt.left), stmt.op, self.e(stmt.right))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield astlib.Return(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield self.e_callable(stmt)

    @layers.register(astlib.DataMember)
    def data_member_stmt(self, stmt):
        if stmt.datatype == astlib.DataT.struct:
            yield self.e_struct_member(stmt)
        else:
            errors.not_now(errors.STRANGE_STMT)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield from self.field_decl(stmt)
        else:
            yield from self.var_let_decl(stmt)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.method:
            errors.not_now(errors.BAD)
        elif stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)
        else:
            args = self.a(stmt.args)
            type_ = self.t(stmt.rettype)
            context.env[stmt.name] = {
                "node_type": utils.declt_to_nodet(stmt.decltype),
                "type_": type_,
                "args": args
            }
            +context.env
            for name, type_ in args:
                context.env[name] = {
                    "node_type": astlib.NodeT.let,
                    "type_": type_
                }
            yield astlib.CallableDecl(
                stmt.decltype, stmt.parent, stmt.name,
                args, type_, self.b(stmt.body))
            -context.env

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        # TODO:
        #   * add adt support
        #   * add generic adt support
        #   * add protocol support
        params = self.p(stmt.params)
        context.env[stmt.name] = {
            "node_type": utils.declt_to_nodet(stmt.decltype),
            "params": params,
            "methods": {},
            "fields": {}
        }
        +context.env
        context.parent = stmt.name
        for param in params:
            context.env[param] = {
                "node_type": astlib.NodeT.commont
            }
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, params, self.b(stmt.body))
        -context.env
