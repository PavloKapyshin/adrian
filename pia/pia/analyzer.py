from . import astlib, defs, errors, inference, layers, typelib, utils, env_api
from .context import context
from .utils import A


def scroll(t):
    if t in A(astlib.GenericType):
        return scroll(t.base)
    return t


def make_py_call(call):
    if call.name in A(astlib.PyType):
        return astlib.PyTypeCall(call.name.type_, [_e(arg) for arg in call.args])
    return astlib.PyFuncCall(call.name.name, [_e(arg) for arg in call.args])


def _get_adt_field_by_type(parent, type_):
    adt_type = env_api.variable_info(parent)["type_"]
    adt_type_info = env_api.type_info(adt_type)
    for field_name, field_info in adt_type_info["fields"].items():
        if typelib.types_are_equal(field_info["type_"], type_):
            return astlib.DataMember(
                astlib.DataT.adt, parent, astlib.Name(field_name))
    errors.type_mismatch(adt_type, type_)


def _get_adt_field_by_name(name, member):
    return astlib.DataMember(astlib.DataT.adt, name, member)


def adt_init(type_):
    return astlib.Callable(
        astlib.CallableT.struct_func, scroll(type_),
        astlib.Name(defs.INIT_METHOD), [])


def _split_adt_usage(type_, expr, name=None):
    expr_type = inference.infer_general_type(expr)
    if expr_type in A(astlib.Empty) or utils.is_adt(expr_type):
        return expr, None
    return adt_init(type_), astlib.Assignment(
        _get_adt_field_by_type(
            name, inference.infer_type(expr)), "=", expr)


def _e_callable(expr: astlib.Callable):
    if expr.name in A(astlib.PyObject):
        return make_py_call(expr)
    callable_type = expr.callabletype
    if expr.name == defs.REF and callable_type == astlib.CallableT.fun:
        args = _a(expr.args)
        if len(args) != 1:
            errors.args_number_mismatch(len(args), 1)
        return astlib.Ref(args[0])
    if (callable_type not in (
            astlib.CallableT.struct_func,) and
            utils.is_type(expr.name)):
        callable_type = astlib.CallableT.struct
    return astlib.Callable(
        callable_type, expr.parent, expr.name, _a(expr.args))


def _e_data_member(expr: astlib.DataMember):
    if expr.datatype == astlib.DataT.struct:
        if expr.parent in A(astlib.Name):
            if utils.is_adt(expr.parent):
                return astlib.AdtMember(
                    base=expr.parent, member=_e(expr.member))
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
            return astlib.PyConstant(defs.TRUE)
        elif expr == defs.FALSE:
            return astlib.PyConstant(defs.FALSE)
        return expr
    elif expr in A(astlib.Callable):
        return _e_callable(expr)
    elif expr in A(astlib.DataMember):
        return _e_data_member(expr)
    elif expr in A(astlib.Expr):
        left = _e(expr.left)
        right = _e(expr.right)
        return astlib.Callable(
            astlib.CallableT.struct_func, inference.infer_type(left),
            defs.OP_TO_METHOD[expr.op], [left, right])
    elif expr in A(astlib.Literal):
        if expr.type_ == astlib.LiteralT.vector:
            return astlib.Literal(
                expr.type_, [_e(arg) for arg in expr.literal])
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
            return astlib.PyType(defs.BOOL)
    elif type_ in A(astlib.GenericType):
        return astlib.GenericType(_t(type_.base), list(map(_t, type_.params)))
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

    def __init__(self):
        self.b = layers.b(Analyzer)

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_scope()
        yield astlib.While(_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()

    @layers.register(astlib.Assignment)
    def translate_assignment(self, stmt):
        right = _e(stmt.right)
        left = _e(stmt.left)
        env_api.register(stmt, right=right)
        yield astlib.Assignment(left, stmt.op, right)

    @layers.register(astlib.Return)
    def translate_return(self, stmt):
        yield astlib.Return(_e(stmt.expr))

    @layers.register(astlib.Callable)
    def translate_callable(self, stmt):
        yield _e_callable(stmt)

    @layers.register(astlib.DataMember)
    def translate_data_member(self, stmt):
        yield _e_data_member(stmt)

    def _if(self, stmt):
        context.env.add_scope()
        result = astlib.If(_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _elif(self, stmt):
        context.env.add_scope()
        result = astlib.Elif(_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_ = self._if(stmt.if_)
        elifs_ = []
        for elif_ in stmt.elifs_:
            elifs_.append(self._elif(elif_))
        if stmt.else_ in A(list):
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield astlib.Cond(if_, elifs_, else_)

    @layers.register(astlib.Decl)
    def translate_declaration(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            type_ = _t(stmt.type_)
            env_api.register(stmt, type_=type_)
            yield astlib.Decl(stmt.decltype, stmt.name, type_, stmt.expr)
        else:
            type_, expr = _infer_unknown(stmt.type_, stmt.expr)
            env_api.register(stmt, type_=type_, expr=expr)
            result = astlib.Decl(stmt.decltype, stmt.name, type_, expr)
            if utils.is_adt(type_):
                expr, assignments = _split_adt_usage(
                    type_, expr, name=stmt.name)
                yield astlib.Decl(stmt.decltype, stmt.name, type_, expr)
                if assignments is not None:
                    yield assignments
            else:
                yield result

    @layers.register(astlib.CallableDecl)
    def translate_callable_declaration(self, stmt):
        type_ = _t(stmt.rettype)
        args = _a(stmt.args)
        env_api.register(stmt, args=args, type_=type_)
        context.env.add_scope()
        env_api.register_args(args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            args, type_, self.b(stmt.body))
        context.env.remove_scope()

    @layers.register(astlib.DataDecl)
    def translate_data_declaration(self, stmt):
        env_api.register(stmt)
        context.env.add_scope()
        context.parent = stmt.name
        env_api.register_params(stmt.params)
        body = stmt.body
        body = self.b(body)
        yield astlib.DataDecl(stmt.decltype, stmt.name, stmt.params, body)
        context.env.remove_scope()
