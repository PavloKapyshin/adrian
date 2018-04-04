from .utils import A
from .context import context
from . import astlib, layers, errors, inference, utils, defs, env_api


def _check_fields_exist_in_parent(data_member):
    def _get_type_of_parent(expr):
        if expr in A(astlib.Name):
            # TODO: write using specific funcs
            found = env_api.variable_info(expr)
            return found["type_"]
        return _check_fields_exist_in_parent(expr)

    parent_type = _get_type_of_parent(data_member.parent)
    parent_type_info = env_api.type_info(parent_type)
    if data_member.member not in parent_type_info["fields"]:
        errors.no_such_field(parent_type, data_member.member)
    return parent_type


def _check_name_is_variable(name):
    if name in A(astlib.Name):
        info = context.env[name]
        if not info:
            errors.unknown_name(name)
        if info["node_type"] != astlib.NodeT.var and name != defs.SELF:
            errors.cannot_reassign_name(name)
    elif name in A(astlib.DataMember):
        if name.datatype == astlib.DataT.struct:
            _ = _check_fields_exist_in_parent(name)
            parent = utils.scroll_to_parent(name)
            _check_name_is_variable(parent)
        else:
            # TODO: write for adt
            pass


def _check_you_can_declarate_name_for_variable(name):
    if name in context.env:
        errors.name_already_exists(name)


def _check_type_name(name):
    if name not in context.env:
        errors.unknown_name(name)


def _check_for_valid_usage_of_c_type(c_type):
    AVAILABLE_CMEMBERS = (
        "IntFast8", "IntFast16", "IntFast32", "IntFast64",
        "UIntFast8", "UIntFast16", "UIntFast32", "UIntFast64",
        "Char", "Array", "Pointer")
    if c_type.member in A(astlib.Name) and c_type.member in AVAILABLE_CMEMBERS:
        return
    errors.no_such_module_member("c", c_type.member)


def _check_type(type_):
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
            errors.fatal_error("invalid type storage")
    elif type_ in A(astlib.GenericType):
        # TODO:
        #   * check you passed right params
        #   * check num of passed params and num of
        #       declarated params are equal
        #   * check base is a generic type
        _check_type(type_.base)
        map(_check_type, type_.params)
    elif type_ in A(astlib.Empty, astlib.LiteralType):
        pass
    else:
        errors.bad_type(type_)


def _check_callable_exists(expr):
    if expr.callabletype == astlib.CallableT.struct_func:
        if expr.parent not in context.env:
            errors.unknown_name(expr.parent)
        parent_info = env_api.type_info(expr.parent)
        if expr.name not in parent_info["methods"]:
            errors.no_such_method(expr.parent, expr.name)
    elif expr.callabletype == astlib.CallableT.struct:
        if expr.name not in context.env:
            errors.unknown_name(expr.name)
    elif expr.callabletype == astlib.CallableT.cfunc:
        if expr.name not in (defs.MALLOC_FUNC, defs.SIZEOF, defs.FREE_FUNC):
            errors.no_such_module_member("c", expr.name)
    elif expr.name not in context.env:
        errors.unknown_name(expr.name)


def _check_expr(name, expr):
    if expr in A(astlib.Name):
        if expr == name:
            errors.unknown_name(name)
    elif expr in A(astlib.DataMember):
        if expr.datatype in (astlib.DataT.struct, astlib.DataT.adt):
            _check_expr(name, expr.parent)
        else:
            errors.bad_expr(expr)
    elif expr in A(astlib.Callable):
        # TODO:
        #   * check you passed right args (number, types)
        _check_type(expr.parent)
        _check_callable_exists(expr)
        for arg in expr.args:
            _check_expr(name, arg)
    elif expr in A(astlib.Ref):
        _check_expr(name, expr.expr)
    elif expr in A(astlib.StructScalar, astlib.Literal, astlib.Empty):
        pass
    else:
        errors.bad_expr(expr)


class Checker(layers.Layer):

    def __init__(self):
        self.b = layers.b(Checker)

    @layers.register(astlib.Decl)
    def _check_declaration(self, stmt):
        _check_you_can_declarate_name_for_variable(stmt.name)
        _check_type(stmt.type_)
        if stmt.decltype != astlib.DeclT.field:
            _check_expr(stmt.name, stmt.expr)
        env_api.register(stmt)
        yield stmt

    @layers.register(astlib.Assignment)
    def _check_assignment(self, stmt):
        _check_name_is_variable(stmt.left)
        env_api.register(stmt)
        yield stmt

    @layers.register(astlib.CallableDecl)
    def _check_callable_declaration(self, stmt):
        env_api.register(stmt)
        +context.env
        env_api.register_args(stmt.args)
        _ = self.b(stmt.body)
        yield stmt
        -context.env

    @layers.register(astlib.DataDecl)
    def _check_data_declaration(self, stmt):
        env_api.register(stmt)
        +context.env
        context.parent = stmt.name
        env_api.register_params(stmt.params)
        _ = self.b(stmt.body)
        yield stmt
        -context.env
