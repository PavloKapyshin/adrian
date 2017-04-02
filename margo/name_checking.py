from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def check_value(value, *, context):
    """Check that value (atom or a list of atoms) is valid."""
    if isinstance(value, defs.NAME_TYPES):
        if not layers.name_exists(value.value, context=context):
            errors.non_existing_name(
                context.line, context.exit_on_error, name=value.value)
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
    if name.value in defs.STANDARD_FUNC_NAMES:
        errors.cant_reassign_builtin(
            context.line, context.exit_on_error, name=name.value)
    # Check existence of the type.
    if not layers.type_exists(name.type_, context=context):
        errors.non_existing_name(context.line, context.exit_on_error, name=name.type_)
    value = check_value(pair.stmt.value, context=context)
    context.namespace.add_name(name.value, {
        "node_type": defs.NodeType.variable,
        "type": name.type_,
        "value": value
    })
    return ast.Pair(pair.line, ast.Assignment(name.value, name.type_, value))


def check(ast_, *, context):
    return [_FUNCS[pair.stmt](pair, context=context) for pair in ast_]


def main(ast_, *, context=ast.Context(exit_on_error=True)):
    return check(ast_, context=context)
