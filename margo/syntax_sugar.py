"""Translates AST into more simple (for computer)."""

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def std_type_init_args_from_name(type_, value):
    # TODO: refactor
    if type_.value == "Integer":
        return ast.StructElem(value.value, "digits")


def std_type_init_args(type_, value):
    # TODO: refactor
    if type_.value == "Integer":
        return ast.CString(value.value)


def translate_std_type_value(type_, value):
    if isinstance(value, ast.Name):
        return ast.FuncCall(ast.StructElem(
            ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("__init__")), args=std_type_init_args_from_name(type_, value))
    elif isinstance(value, list):
        _d = {
            "+": "add",
            "-": "sub",
            "*": "mul",
            "/": "div"
        }
        return ast.FuncCall(ast.StructElem(
            ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("__" + _d[value[0]] + "__")), args=(
                translate_std_type_value(type_, value[1]),
                translate_std_type_value(type_, value[2])))
    return ast.FuncCall(ast.StructElem(
        ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
        ast.Name("__init__")), args=std_type_init_args(type_, value))


@_FUNCS.register(ast.Decl)
def decl(pair, *, context):
    stmt = pair.stmt
    type_ = stmt.type_
    value = stmt.value
    if (isinstance(stmt.type_, ast.Name) and \
            stmt.type_.value in defs.STANDARD_TYPE_NAMES):
        type_ = ast.ModuleMember(module_name="std_types", member=stmt.type_)
        value = translate_std_type_value(stmt.type_, stmt.value)
    return ast.Decl(stmt.name, type_, value)


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(_FUNCS[pair.stmt](pair, context=context))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    return translate(ast_, context=context)
