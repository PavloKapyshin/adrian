from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def name_exists(name, *, context):
    """Check for existence of a name."""
    return context.namespace.exists(name)


def type_exists(type_, *, context):
    """Check for existence of a type."""
    return type_ in defs.STANDARD_TYPES or context.typespace.exists(type_)


def func_exists(func, *, context):
    """Check for existence of a function."""
    return func in defs.STANDARD_FUNCS or context.funcspace.exists(func)


def check_value(value, *, context):
    """Check that value (atom or a list of atoms) is valid."""
    if isinstance(value, defs.NAME_TYPES):
        if not context.exists(value, space=context.namespace):
            errors.non_existing_name(context.line, name=value)
        return value
    elif isinstance(value, defs.ATOM_TYPES):
        return value
    elif isinstance(value, list):
        return [
            value[0],
            check_value(value[1], context=context),
            check_value(value[2], context=context)
        ]


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    name = pair.stmt.name
    context.line = pair.line
    # Check that builtins are not reassigned.
    if name.value in defs.STANDARD_FUNCS:
        errors.cant_reassign_builtin(pair.line, name=name.value)
    # Check existence of the type.
    if not type_exists(name.type_, context=context):
        errors.non_existing_name(pair.line, name=name.type_)
    value = check_value(pair.stmt.value, context=context)
    return ast.Assignment(name.value, name.type_, value)


def check(pair, *, context):
    return _FUNCS[pair.stmt](pair, context=context)


def main(ast_):
    return [check(pair, context=ast.Context()) for pair in ast_]
