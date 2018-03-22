from .context import context
from . import astlib, layers, errors, defs, inference, utils
from .utils import A


class Analyzer(layers.Layer):

    def __init__(self):
        self.b = layers._b(Analyzer)

    def e_callable(self, callable_):
        callabletype = callable_.callabletype
        if (callable_.name == defs.REF and
                callabletype == astlib.CallableT.fun):
            args = self.a(callable_.args)
            if len(args) != 1:
                errors.wrong_number_of_args(len(args), expected=1)
            return astlib.Ref(args[0])
        if (callabletype not in (
                    astlib.CallableT.cfunc, astlib.CallableT.struct_func)
                and utils.is_type(utils.get_node_type(callable_.name))):
            callabletype = astlib.CallableT.struct
        return astlib.Callable(
            callabletype, callable_.parent,
            callable_.name, self.a(callable_.args))

    def e_struct_member(self, expr):
        if expr.member in A(astlib.Callable):
            call = expr.member
            parent = self.e(expr.parent)
            return astlib.Callable(
                astlib.CallableT.struct_func, inference.infer_type(parent),
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
                astlib.CallableT.struct_func, inference.infer_type(left),
                defs.OP_TO_METHOD[expr.op], [left, right])
        return expr

    def t(self, type_):
        if type_ in A(astlib.Name):
            if type_ == "Void":
                return astlib.Void()
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                if type_.parent != defs.CMODULE:
                    errors.not_now(errors.MODULES)
        if type_ in A(astlib.ParamedType):
            return astlib.ParamedType(
                self.t(type_.base), list(map(self.t, type_.params)))
        return type_

    def a(self, args):
        if not args:
            return []
        elif args[0] in A(tuple):
            return [(name, self.t(type_)) for name, type_ in args]
        return list(map(self.e, args))

    def te(self, type_, expr):
        if not type_:
            expr = self.e(expr)
            return inference.infer_type(expr), expr
        elif not expr:
            type_ = self.t(type_)
            return type_, inference.infer_expr(type_)
        return self.t(type_), self.e(expr)

    # Subcore funcs.
    def field_decl(self, stmt):
        type_ = self.t(stmt.type_)
        utils.register_field(stmt.name, type_)
        yield astlib.Decl(stmt.decltype, stmt.name, type_, stmt.expr)

    def var_let_decl(self, stmt):
        type_, expr = self.te(stmt.type_, stmt.expr)
        utils.register_var_or_let(stmt.name, stmt.decltype, type_)
        yield astlib.Decl(stmt.decltype, stmt.name, type_, expr)

    def struct_func_decl(self, stmt):
        type_ = self.t(stmt.rettype)
        args = self.a(stmt.args)
        utils.register_func_as_child(stmt.parent, stmt.name, type_, args)
        +context.env
        utils.register_args(args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        -context.env

    def func_decl(self, stmt):
        type_ = self.t(stmt.rettype)
        args = self.a(stmt.args)
        utils.register_func(stmt.name, type_, args)
        +context.env
        utils.register_args(args)
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
            errors.unknown_stmt(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            yield from self.field_decl(stmt)
        elif stmt.decltype in (astlib.DeclT.var, astlib.DeclT.let):
            yield from self.var_let_decl(stmt)
        else:
            errors.unknown_stmt(stmt)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)
        elif stmt.decltype == astlib.DeclT.fun:
            yield from self.func_decl(stmt)
        else:
            errors.unknown_stmt(stmt)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register_data_decl(stmt.name, stmt.decltype, stmt.params)
        +context.env
        context.parent = stmt.name
        utils.register_params(stmt.params)
        if stmt.decltype == astlib.DeclT.adt:
            errors.not_now(errors.ADT)
        else:
            body = self.b(stmt.body)
        yield astlib.DataDecl(stmt.decltype, stmt.name, stmt.params, body)
        -context.env
