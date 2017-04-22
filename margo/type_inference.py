from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def get_type_from_method(method, *, context):
    # TODO: support other methods and structs
    if not isinstance(method, ast.StructElem):
        return method
    elif method.elem.value == "init":
        return method.struct
    errors.not_implemented(context.line, context.exit_on_error)


def get_type_from_expr(expr, *, context):
    if isinstance(expr, ast.Name):
        return context.namespace.get(expr.value)["type"]
    elif isinstance(expr, ast.MethodCall):
        if ((not isinstance(expr.method, ast.StructElem)) or \
                expr.method.elem.value == "init"):
            return get_type_from_method(expr.method, context=context)
        else:
            errors.not_implemented(context.line, context.exit_on_error)
    elif isinstance(expr, defs.ATOM_TYPES):
        return expr.to_string()
    elif isinstance(expr, list):
        # Expression contains only one type atoms
        return get_type_from_expr(expr[1], context=context)
    errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_ = stmt.type_
    if not type_:
        type_ = get_type_from_expr(stmt.expr, context=context)
    context.namespace.add_name(stmt.name.value, {"type": type_})
    return ast.Decl(stmt.name, type_, stmt.expr)


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(_FUNCS[pair.stmt](pair.stmt, context=context))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    return translate(ast_, context=context)