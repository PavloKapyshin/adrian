from .context import context
from . import astlib, layers, errors, defs, inference, utils
from .utils import A


class Analyzer(layers.Layer):

    def __init__(self):
        self.b = layers._b(Analyzer)

    # Misc.
    def real_type(self, type_, parent):
        # TODO:
        #   * check: type_ is parameter -> replace by mapping[type_]
        return type_

    def get_mapping(self, type_):
        if type_ in A(astlib.ParamedType):
            mapping = {}
            struct_info = context.env[type_.type_]
            params = struct_info["params"]
            i = 0
            for param in params:
                if param not in mapping:
                    mapping[param] = type_.params[i]
                i += 1
            return mapping
        return {}

    def construct_type(self, tname):
        params = context.env[tname]["params"]
        if len(params) == 0:
            return tname
        return astlib.ParamedType(tname, params)

    def e_callable(self, callable_):
        callabletype = callable_.callabletype
        if (callabletype not in (
                    astlib.CallableT.cfunc, astlib.CallableT.struct_func)
                and utils.is_type(callable_.name)):
            callabletype = astlib.CallableT.struct
        if (callable_.name == defs.REF and
                callabletype == astlib.CallableT.fun):
            args = self.a(callable_.args)[0]
            if len(args) != 1:
                errors.wrong_n_args(len(args), expected=1)
            return astlib.Ref(args[0])
        return astlib.Callable(
            callabletype, callable_.parent,
            callable_.name, self.a(callable_.args))

    def e_struct_member(self, expr):
        if expr.member in A(astlib.Callable):
            call = expr.member
            parent = self.e(expr.parent)
            return astlib.Callable(
                astlib.CallableT.struct_func,
                self.real_type(
                    inference.infer_type(parent), parent),
                call.name, [parent] + self.a(call.args))
        return astlib.DataMember(
            expr.datatype, self.e(expr.parent),
            expr.member)

    # Inner ast nodes translation.
    def e(self, expr):
        if expr in A(astlib.Callable):
            return self.e_callable(expr)
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return self.e_struct_member(expr)
        if expr in A(astlib.Expr):
            left = self.e(expr.left)
            right = self.e(expr.right)
            return astlib.Callable(
                astlib.CallableT.struct_func,
                inference.infer_type(left),
                defs.OP_TO_METHOD[expr.op], [left, right])
        return expr

    def t(self, type_):
        # TODO:
        #   * add support of user modules
        if type_ in A(astlib.Name):
            if type_ == "Void":
                return astlib.Void()
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                if type_.parent != defs.CMODULE:
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
        if args in A(astlib.Empty) or len(args) == 0:
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
        # Maybe adding of parent to stmt can simplify something.
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
        if not stmt.type_:
            expr = self.e(stmt.expr)
            type_ = inference.infer_type(expr)
        elif not stmt.expr:
            type_ = self.t(stmt.type_)
            expr = inference.infer_expr(type_)
        else:
            type_, expr = self.t(stmt.type_), self.e(stmt.expr)
        context.env[stmt.name] = {
            "node_type": utils.declt_to_nodet(stmt.decltype),
            "type_": type_,
            "mapping": self.get_mapping(type_)
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
        for name, t in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": t
            }
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        -context.env

    def protocol_func_decl(self, stmt):
        args = self.a(stmt.args)
        type_ = self.t(stmt.rettype)
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
        for name, t in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": t
            }
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        -context.env

    def func_decl(self, stmt):
        type_ = self.t(stmt.rettype)
        args = self.a(stmt.args)
        context.env[stmt.name] = {
            "node_type": astlib.NodeT.fun,
            "type_": type_,
            "args": args
        }
        +context.env
        for name, t in args:
            context.env[name] = {
                "node_type": astlib.NodeT.let,
                "type_": t
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
        elif stmt.decltype == astlib.DeclT.fun:
            yield from self.func_decl(stmt)
        elif stmt.decltype == astlib.DeclT.protocol_func:
            yield from self.protocol_func_decl(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
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
        if stmt.decltype == astlib.DeclT.adt:
            body = [self.t(t) for t in stmt.body]
            possible_types = body
            context.env.update(stmt.name, {
                "possible_types": possible_types
            })
        else:
            body = self.b(stmt.body)
        yield astlib.DataDecl(stmt.decltype, stmt.name, params, body)
        -context.env
