from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def type_from_alias(type_, *, context):
    if (isinstance(type_, ast.Name) and \
            type_.value in defs.STD_TYPE_NAMES):
        return ast.ModuleMember("std_types", type_)
    return type_


def translate_atom_to_init(atom):
    return ast.MethodCall(ast.StructElem(
        ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, ast.Name(atom.to_string())),
        ast.Name("init")), args=[atom.value])


def expr_from_aliases(expr, *, context):
    # TODO: translate `1 + 2` to
    # `std_types#Integer.add(std_types#Integer.init(1), ...)`
    if isinstance(expr, (ast.Name, ast.ModuleMember, ast.MethodCall)):
        return expr
    elif isinstance(expr, defs.ATOM_TYPES):
        return translate_atom_to_init(expr)
    errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_ = type_from_alias(stmt.type_, context=context)
    expr = expr_from_aliases(stmt.expr, context=context)
    return ast.Decl(stmt.name, type_, expr)


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(ast.Pair(
            pair.line, _FUNCS[pair.stmt](pair.stmt, context=context)))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    return translate(ast_, context=context)