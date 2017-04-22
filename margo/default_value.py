from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def get_default_literals_std_type(type_):
    if type_ == ast.Integer.to_string():
        return [ast.Integer("0")]


def make_initialization_std_type(type_):
    return ast.MethodCall(
        method=ast.StructElem(type_, ast.Name("init")),
        args=get_default_literals_std_type(type_))


def get_default_literals_c_type(type_):
    if type_.value in (defs.C_INT32, defs.C_INT64):
        return [ast.Integer("0")]


def make_initialization_c_type(type_):
    return ast.MethodCall(
        method=ast.StructElem(type_, ast.Name("init")),
        args=get_default_literals_c_type(type_.member))


def get_default_expr(type_, *, context):
    if isinstance(type_, ast.Name):
        if type_.value in defs.STD_TYPE_NAMES:
            return make_initialization_std_type(type_.value)
    elif isinstance(type_, ast.ModuleMember):
        if type_.module_name == defs.C_MODULE_NAME:
            return make_initialization_c_type(type_)
    errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    expr = stmt.expr
    if not stmt.expr:
        expr = get_default_expr(stmt.type_, context=context)
    return ast.Decl(stmt.name, stmt.type_, expr)


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(_FUNCS[pair.stmt](pair.stmt, context=context))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    return translate(ast_, context=context)