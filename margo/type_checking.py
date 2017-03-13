from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def str_type(value, *, context):
    if isinstance(value, defs.NAME_TYPES):
        return value.type_
    # TODO: remove if not needed
    #if isinstance(value, str):
    #    return value
    return value.to_string()


def check_type_of_value(value, *, context):
    """Validate value (atom or a list of atoms) and return its type."""
    if isinstance(value, defs.NAME_TYPES):
        if not layers.name_exists(value, context=context):
            errors.non_existing_name(context.line, name=value)
        return str_type(value, context=context)
    elif isinstance(value, defs.ATOM_TYPES):
        return str_type(value, context=context)
    elif isinstance(value, list):
        return check_type_of_value.checker_reg[value[0]](
            check_type_of_value(value[1], context=context),
            check_type_of_value(value[2], context=context),
            context=context)


check_type_of_value.checker_reg = funcreg.NameRegistry()


@check_type_of_value.checker_reg.register("+")
def _check_plus(type1, type2, *, context):
    """Only Integer + Integer is supported."""
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line)


@check_type_of_value.checker_reg.register("-")
def _check_minus(type1, type2, *, context):
    """Only Integer - Integer is supported."""
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line)


@check_type_of_value.checker_reg.register("*")
def _check_multiply(type1, type2, *, context):
    """Only Integer * Integer is supported."""
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line)


@check_type_of_value.checker_reg.register("/")
def _check_divide(type1, type2, *, context):
    """Only Integer / Integer is supported.

    TODO: what type 1/2 has?
    """
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line)


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    context.line = pair.line
    type_of_value = check_type_of_value(stmt.value, context=context)
    if stmt.name.type_ != type_of_value:
        errors.type_of_name_and_type_of_value_are_not_equal(
            context.line, name=stmt.name.value,
            type_of_name=stmt.name.type_, type_of_value=type_of_value)
    return ast.Pair(
        pair.line,
        ast.Assignment(
            stmt.name.value, stmt.name.type_, stmt.value))


def check(pair, *, context):
    return _FUNCS[pair.stmt](pair, context=context)


def main(ast_):
    return [check(pair, context=ast.Context()) for pair in ast_]
