from .context import context
from . import astlib, layers, errors, defs, inference, utils
from .utils import A


def types_are_equal(type1, type2):
    if type1 in A(astlib.ParamedType) and type2 in A(astlib.ParamedType):
        return type1.base == type2.base
    elif type1 in A(astlib.Name) and type2 in A(astlib.Name):
        return type1 == type2
    return False


def get_adt_field_by_type(parent, type_):
    adt_type = context.env.get_variable_info(parent)["type_"]
    adt_type_info = context.env.get_type_info(adt_type)
    for field_name, field_info in adt_type_info["fields"].items():
        if types_are_equal(field_info["type_"], type_):
            return astlib.DataMember(
                astlib.DataT.adt, parent, astlib.Name(field_name))
    errors.no_adt_field(adt_type, type_)


def split_adt_usage(stmt):
    yield astlib.Decl(
        stmt.decltype, stmt.name, stmt.type_, astlib.Empty())
    yield astlib.Assignment(
        get_adt_field_by_type(
            stmt.name, inference.infer_type(stmt.expr)), "=", stmt.expr)


class Analyzer(layers.Layer):

    def __init__(self, f_count=None):
        self.f_count = f_count or 0
        self.b = layers.b(Analyzer, f_count=self.f_count)

    def update_b(self):
        self.b = layers.b(Analyzer, f_count=self.f_count)

    def make_adt_body(self, body):
        def _provide_name():
            name = astlib.Name(
                "".join([defs.F_STRING, str(self.f_count)]),
                is_user_name=False)
            self.f_count += 1
            return name
        return [
            astlib.Decl(
                astlib.DeclT.field, _provide_name(), type_, astlib.Empty)
            for type_ in body]

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
                and context.env.is_type(
                    context.env.get_node_type(callable_.name))):
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

    def e(self, expr):
        if expr in A(astlib.Name):
            variable_info = context.env.get_variable_info(expr)
            if context.env.is_adt(
                    context.env.get_node_type(variable_info["type_"])):
                return get_adt_field_by_type(
                    expr, inference.infer_type(variable_info["expr"]))
            return expr
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

    def field_decl(self, stmt):
        type_ = self.t(stmt.type_)
        utils.register(stmt, type_=type_)
        yield astlib.Decl(stmt.decltype, stmt.name, type_, stmt.expr)

    def var_let_decl(self, stmt):
        type_, expr = self.te(stmt.type_, stmt.expr)
        utils.register(stmt, type_=type_, expr=expr)
        result = astlib.Decl(stmt.decltype, stmt.name, type_, expr)
        if context.env.is_adt(context.env.get_node_type(type_)):
            yield from split_adt_usage(result)
        else:
            yield result

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        right = self.e(stmt.right)
        utils.register(astlib.Assignment(stmt.left, stmt.op, right))
        yield astlib.Assignment(
            self.e(stmt.left), stmt.op, right)

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
        type_ = self.t(stmt.rettype)
        args = self.a(stmt.args)
        utils.register(stmt, args=args, type_=type_)
        context.env.add_scope()
        utils.register_args(args)
        self.update_b()
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        context.env.remove_scope()

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register(stmt)
        context.env.add_scope()
        context.parent = stmt.name
        utils.register_params(stmt.params)
        body = stmt.body
        if stmt.decltype == astlib.DeclT.adt:
            body = self.make_adt_body(body)
        self.update_b()
        body = self.b(body)
        yield astlib.DataDecl(stmt.decltype, stmt.name, stmt.params, body)
        context.env.remove_scope()
