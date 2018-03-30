from .utils import A
from .context import context
from . import astlib, layers, errors, inference, utils


def _scroll_to_parent(struct_field):
    if struct_field in A(astlib.DataMember):
        return _scroll_to_parent(struct_field.parent)
    return struct_field


def _check_name_is_variable(name):
    if name in A(astlib.Name):
        info = context.env[name]
        if not info:
            errors.unknown_name(name)
        if info["node_type"] != astlib.NodeT.var:
            errors.cannot_reassign_name(name)
    elif name in A(astlib.DataMember):
        # TODO: field exists in parent
        parent = _scroll_to_parent(name)
        _check_name_is_variable(parent)


def _check_you_can_declarate_name_for_variable(name):
    if name in context.env:
        errors.name_already_exists(name)


def _check_type_name(name):
    if name not in context.env:
        errors.unknown_name(name)


def _check_for_valid_usage_of_c_type(c_type):
    if c_type.member in A(astlib.Name):
        if c_type.member not in (
                "IntFast8", "IntFast16", "IntFast32", "IntFast64",
                "UIntFast8", "UIntFast16", "UIntFast32", "UIntFast64",
                "Char", "Array", "Pointer"):
            errors.no_such_module_member("c", c_type.member)
    else:
        errors.fatal_error("is not a type actually")


def _check_type(self, type_):
    if type_ in A(astlib.Name):
        if type_ not in ("Void", defs.BOOL):
            _check_type_name(type_)
    elif type_ in A(astlib.DataMember):
        if type_.datatype == astlib.DataT.module:
            # This layer is executed after linker,
            # so we have only c module here.
            if type_.parent != defs.CMODULE:
                errors.fatal_error("linker is broken")
            else:
                _check_for_valid_usage_of_c_type(type_)
        else:
            errors.fatal_error("you should store type only in modules")
    elif type_ in A(astlib.ParamedType):
        # TODO:
        #   * check you passed right params
        #   * check num of passed params and num of
        #       declarated params are equal
        #   * check base is a generic type
        _check_type(type_.base)
        map(_check_type, type_.params)
    elif type_ in A(astlib.Empty):
        pass
    else:
        errors.bad_type(type_)


def _check_type_and_type_of_expr_are_equal(name, type_, expr):
    expr_type = inference.infer_type(expr)
    if (typelib.types_are_equal(type_, expr_type) or
            typelib.is_supertype(type_, of=expr)):
        pass
    else:
        errors.type_of_name_and_expr_are_not_equal(name, type_, expr_type)


def _check_callable_exists(expr):
    if expr.callabletype == astlib.CallableT.struct_func:
        if expr.parent not in context.env:
            errors.no_such_type(expr.parent)
        parent_info = context.env.get_type_info(expr.parent)
        if expr.name not in parent_info["methods"]:
            errors.no_such_method(expr.parent, expr.name)
    elif expr.callabletype == astlib.CallableT.struct:
        if expr.name not in context.env:
            errors.no_such_type(expr.name)
    elif expr.callabletype == astlib.CallableT.cfunc:
        if expr.name not in (defs.MALLOC_FUNC, defs.SIZEOF, defs.FREE_FUNC):
            errors.no_such_module_member("c", expr.name)
    else:
        if expr.name not in context.env:
            errors.no_such_func(expr.name)


def _check_expr(name, expr):
    if expr in A(astlib.Name):
        if expr == name:
            errors.unknown_name(name)
    elif expr in A(astlib.DataMember):
        if expr.datatype in (astlib.DataT.struct, astlib.DataT.adt):
            _check_expr(expr.parent)
        else:
            errors.bad_expr(expr)
    elif expr in A(astlib.Callable):
        # TODO:
        #   * check you passed right args (number, types)
        _check_type(expr.parent)
        _check_callable_exists(expr)
        map(_check_expr, expr.args)
    elif expr in A(astlib.Ref):
        _check_expr(name, expr.expr)
    elif expr in A(astlib.StructScalar, astlib.Literal, astlib.Empty):
        pass
    else:
        errors.bad_expr(expr)


class Checker(layers.Layer):

    @layers.register(astlib.Decl)
    def _check_declaration(self, stmt):
        _check_you_can_declarate_name_for_variable(stmt.name)
        _check_type(stmt.type_)
        if stmt.decltype != astlib.DeclT.field:
            _check_type_and_type_of_expr_are_equal(
                stmt.name, stmt.type_, stmt.expr)
            _check_expr(stmt.name, stmt.expr)
        utils.register(stmt)
        yield stmt

    @layers.register(astlib.Assignment)
    def _check_assignment(self, stmt):
        _check_name_is_variable(stmt.left)
        _check_type_and_type_of_expr_are_equal(
            stmt.left, inference.infer_type(stmt.left), stmt.right)
        yield stmt

    @layers.register(astlib.CallableDecl)
    def _check_callable_declaration(self, stmt):
        utils.register(stmt)
        yield stmt

    @layers.register(astlib.DataDecl)
    def _check_data_declaration(self, stmt):
        utils.register(stmt)
        yield stmt
