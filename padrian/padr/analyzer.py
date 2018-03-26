from . import astlib, defs, errors, inference, layers, typelib, utils
from .context import context
from .utils import A


def _get_adt_field_by_type(parent, type_):
    adt_type = context.env.get_variable_info(parent)["type_"]
    adt_type_info = context.env.get_type_info(adt_type)
    for field_name, field_info in adt_type_info["fields"].items():
        if typelib.types_are_equal(field_info["type_"], type_):
            return astlib.DataMember(
                astlib.DataT.adt, parent, astlib.Name(field_name))
    errors.no_adt_field(adt_type, type_)


def _split_adt_usage(stmt):
    yield astlib.Decl(
        stmt.decltype, stmt.name, stmt.type_, astlib.Empty())
    yield astlib.Assignment(
        _get_adt_field_by_type(
            stmt.name, inference.infer_type(stmt.expr)), "=", stmt.expr)


def _e_callable(expr: astlib.Callable):
    callable_type = expr.callabletype
    if expr.name == defs.REF and callable_type == astlib.CallableT.fun:
        args = _a(expr.args)
        if len(args) != 1:
            errors.wrong_number_of_args(len(args), 1)
        return astlib.Ref(args[0])
    if callable_type not in (
            astlib.CallableT.cfunc, astlib.CallableT.struct_func)\
            and context.env.is_type(context.env.get_node_type(expr.name)):
        callable_type = astlib.CallableT.struct
    return astlib.Callable(
        callable_type, expr.parent, expr.name, _a(expr.args))


def _e_data_member(expr: astlib.DataMember):
    if expr.datatype == astlib.DataT.struct:
        if expr.member in A(astlib.Callable):
            parent = _e(expr.parent)
            return astlib.Callable(
                astlib.CallableT.struct_func, inference.infer_type(parent),
                expr.member.name, [parent] + _a(expr.member.args)
            )
        return astlib.DataMember(expr.datatype, _e(expr.parent), expr.member)
    return expr


def _e(expr):
    if expr in A(astlib.Not):
        inner_expr = _e(expr.expr)
        return astlib.Callable(
            astlib.CallableT.struct_func, inference.infer_type(inner_expr),
            defs.NOT_METHOD, [inner_expr])
    if expr in A(astlib.Name):
        if expr == defs.TRUE:
            return defs.TRUE_TRANSLATION
        elif expr == defs.FALSE:
            return defs.FALSE_TRANSLATION
        variable_info = context.env.get_variable_info(expr)
        if context.env.is_adt(
                context.env.get_node_type(variable_info["type_"])):
            return _get_adt_field_by_type(
                expr, inference.infer_type(variable_info["expr"]))
    elif expr in A(astlib.Callable):
        return _e_callable(expr)
    elif expr in A(astlib.DataMember):
        return _e_data_member(expr)
    elif expr in A(astlib.Expr):
        left = _e(expr.left)
        right = _e(expr.right)
        return astlib.Callable(
            astlib.CallableT.struct_func, inference.infer_type(left),
            defs.OP_TO_METHOD[expr.op], [left, right]
        )
    return expr


def _a(args):
    if not args:
        return []
    elif args[0] in A(tuple):
        return [(name, _t(type_)) for name, type_ in args]
    return list(map(_e, args))


def _t(type_):
    if type_ in A(astlib.Name):
        if type_ == "Void":
            return astlib.Void()
        elif type_ == defs.BOOL:
            return defs.BOOL_TRANSLATION
    elif type_ in A(
            astlib.DataMember) and type_.datatype == astlib.DataT.module:
        if type_.parent != defs.CMODULE:
            errors.not_now(errors.MODULES)
    elif type_ in A(astlib.ParamedType):
        return astlib.ParamedType(_t(type_.base), list(map(_t, type_.params)))
    return type_


def _infer_unknown(type_, expr):
    if not type_:
        expr = _e(expr)
        return inference.infer_type(expr), expr
    elif not expr:
        type_ = _t(type_)
        return type_, inference.infer_expr(type_)
    return _t(type_), _e(expr)


class Analyzer(layers.Layer):

    def __init__(self, f_count=0):
        self.f_count = f_count
        self._update_b()

    def _update_b(self):
        self.b = layers.b(Analyzer, f_count=self.f_count)

    def _provide_name_for_adt_field(self):
        name = astlib.Name("".join([defs.F_STRING, str(self.f_count)]),
            is_user_name=False)
        self.f_count += 1
        return name

    def _make_adt_body(self, body):
        return [
            astlib.Decl(astlib.DeclT.field, self._provide_name_for_adt_field(),
                type_, astlib.Empty()) for type_ in body]

    @layers.register(astlib.Assignment)
    def translate_assignment(self, stmt):
        right = _e(stmt.right)
        utils.register(stmt, right=right)
        yield astlib.Assignment(_e(stmt.left), stmt.op, right)

    @layers.register(astlib.Return)
    def translate_return(self, stmt):
        yield astlib.Return(_e(stmt.expr))

    @layers.register(astlib.Callable)
    def translate_callable(self, stmt):
        yield _e_callable(stmt)

    @layers.register(astlib.DataMember)
    def translate_data_member(self, stmt):
        yield _e_data_member(stmt)

    def _if_stmt(self, stmt):
        context.env.add_scope()
        result = astlib.If(_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _elif_stmt(self, stmt):
        context.env.add_scope()
        result = astlib.ElseIf(_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_stmt = self._if_stmt(stmt.if_stmt)
        elifs = []
        for elif_ in stmt.else_ifs:
            elifs.append(self._elif_stmt(elif_))
        if stmt.else_ in A(list):
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_stmt, elifs, else_)

    @layers.register(astlib.Decl)
    def translate_declaration(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            type_ = _t(stmt.type_)
            utils.register(stmt, type_=type_)
            yield astlib.Decl(stmt.decltype, stmt.name, type_, stmt.expr)
        else:
            type_, expr = _infer_unknown(stmt.type_, stmt.expr)
            utils.register(stmt, type_=type_, expr=expr)
            result = astlib.Decl(stmt.decltype, stmt.name, type_, expr)
            if context.env.is_adt(context.env.get_node_type(type_)):
                yield from _split_adt_usage(result)
            else:
                yield result

    @layers.register(astlib.CallableDecl)
    def translate_callable_declaration(self, stmt):
        type_ = _t(stmt.rettype)
        args = _a(stmt.args)
        utils.register(stmt, args=args, type_=type_)
        context.env.add_scope()
        utils.register_args(args)
        self._update_b()
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        context.env.remove_scope()

    @layers.register(astlib.DataDecl)
    def translate_data_declaration(self, stmt):
        utils.register(stmt)
        context.env.add_scope()
        context.parent = stmt.name
        utils.register_params(stmt.params)
        body = stmt.body
        if stmt.decltype == astlib.DeclT.adt:
            body = self._make_adt_body(body)
        self._update_b()
        body = self.b(body)
        yield astlib.DataDecl(stmt.decltype, stmt.name, stmt.params, body)
        context.env.remove_scope()
