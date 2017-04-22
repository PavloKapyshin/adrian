from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def get_type_of_value(value, *, context):
    if isinstance(value, ast.Name):
        return context.namespace.get(value.value)["type"]
    elif isinstance(value, ast.MethodCall):
        if not value.method or value.method == "init":
            return value.struct
    elif isinstance(value, defs.ATOM_TYPES):
        return value.to_string()
    elif isinstance(value, list):
        return get_type_of_value.reg[value[0]](
            get_type_of_value(value[1], context=context),
            get_type_of_value(value[2], context=context),
            context=context)


get_type_of_value.reg = funcreg.NameRegistry()


@get_type_of_value.reg.register("+")
@get_type_of_value.reg.register("-")
@get_type_of_value.reg.register("*")
@get_type_of_value.reg.register("/")
def _basic_operators(type1, type2, *, context):
    if isinstance(type1, str) and isinstance(type2, str):
        if type1 == type2:
            return type1
        errors.not_implemented(context.line, context.exit_on_error)
    elif isinstance(type1, ast.Name) and isinstance(type2, ast.Name):
        if type1.value == type2.value:
            return type1.value
        errors.not_implemented(context.line, context.exit_on_error)
    elif (isinstance(type1, ast.ModuleMember) and \
            isinstance(type2, ast.ModuleMember)):
        type_ = _basic_operators(type1.member, type2.member, context=context)
        if type1.module_name == type2.module_name and type_:
            return type_
        errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_of_value = get_type_of_value(stmt.value, context=context)
    if isinstance(stmt.type_, ast.Name):
        type_ = stmt.type_.value
    elif isinstance(stmt.type_, ast.ModuleMember):
        type_ = stmt.type_.member.value
    if type_ is None:
        # Type inference.
        type_ = type_of_value
    elif type_ != type_of_value and not type_of_value is None:
        errors.type_of_name_and_type_of_value_are_not_equal(
            context.line, context.exit_on_error, name=stmt.name,
            type_of_name=type_, type_of_value=type_of_value)
    context.namespace.add_name(stmt.name, {
        "node_type": defs.NodeType.variable,
        "type": type_,
        "value": stmt.value
    })
    return ast.Decl(stmt.name, type_, stmt.value)


def check(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(_FUNCS[pair.stmt](pair.stmt, context=context))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    return check(ast_, context=context)