from . import ast
from . import defs
from . import errors
from . import inference

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_ = stmt.type_
    if not type_:
        type_ = inference.get_type_from_expr(stmt.expr, context=context)
    context.namespace.add_name(stmt.name.value, {"type": type_})
    return ast.Decl(stmt.name, type_, stmt.expr)


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