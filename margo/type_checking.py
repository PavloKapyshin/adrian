from . import ast
from . import defs
from . import errors
from . import inference

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def check_types(type1, type2, name, *, context):
    if isinstance(type1, ast.Name) and isinstance(type2, ast.Name):
        if type1.value == type2.value:
            return
    elif (isinstance(type1, ast.ModuleMember) and \
            isinstance(type2, ast.ModuleMember)):
        if type1.module_name == type2.module_name:
            check_types(type1.member, type2.member, name, context=context)
            return
    errors.type_of_name_and_type_of_expr_are_not_equal(
        context.line, context.exit_on_error,
        name=name, type_of_name=type1, type_of_expr=type2)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_of_expr = inference.get_type_from_expr(stmt.expr, context=context)
    type_ = stmt.type_
    check_types(type_, type_of_expr, stmt.name.value, context=context)


def check(ast_, *, context):
    for pair in ast_:
        context.line = pair.line
        _FUNCS[pair.stmt](pair.stmt, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    check(ast_, context=context)