from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def str_type(value, *, context):
    if isinstance(value, ast.Name):
        entry = context.namespace.get(value.value)
        if entry:
            return entry["type"]
    return value.to_string()


def get_type_of_value(value, *, context):
    if isinstance(value, defs.ATOM_TYPES):
        return str_type(value, context=context)
    elif isinstance(value, list):
        return get_type_of_value.getter_reg[value[0]](
            get_type_of_value(value[1], context=context),
            get_type_of_value(value[2], context=context),
            context=context)


get_type_of_value.getter_reg = funcreg.NameRegistry()


@get_type_of_value.getter_reg.register("+")
def _get_plus(type1, type2, *, context):
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line, context.exit_on_error)


@get_type_of_value.getter_reg.register("-")
def _get_plus(type1, type2, *, context):
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line, context.exit_on_error)


@get_type_of_value.getter_reg.register("*")
def _get_plus(type1, type2, *, context):
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line, context.exit_on_error)


@get_type_of_value.getter_reg.register("/")
def _get_plus(type1, type2, *, context):
    # TODO: what type 1/2 has?
    if type1 == ast.Integer.to_string() == type2:
        return ast.Integer.to_string()
    errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    context.line = pair.line
    type_of_value = get_type_of_value(stmt.value, context=context)
    if (isinstance(stmt.type_, ast.ModuleMember) and \
            stmt.type_.module_name == defs.CTYPES_MODULE_NAME):
        _d = {
            defs.CTYPES_INT32_STRING: ast.Integer.to_string(),
            defs.CTYPES_INT64_STRING: ast.Integer.to_string(),
            defs.CTYPES_CHAR_STRING: ast.String.to_string(),
        }
        if type_of_value != _d[stmt.type_.member.value]:
            errors.type_of_name_and_type_of_value_are_not_equal(
                context.line, context.exit_on_error,
                name=stmt.name.value, type_of_name=_d[stmt.type_.member.value],
                type_of_value=type_of_value)
    elif stmt.type_ != type_of_value and not type_of_value is None:
        errors.type_of_name_and_type_of_value_are_not_equal(
            context.line, context.exit_on_error, name=stmt.name.value,
            type_of_name=stmt.type_, type_of_value=type_of_value)
    context.namespace.add_name(stmt.name.value, {
        "node_type": defs.NodeType.variable,
        "type": stmt.type_,
        "value": stmt.value
    })


def check(ast_, *, context):
    for pair in ast_:
        _FUNCS[pair.stmt](pair, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    check(ast_, context=context)
