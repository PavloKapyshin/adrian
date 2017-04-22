from . import ast
from . import defs
from . import errors


def _get_type_from_method(method, *, context):
    # TODO: support other methods and structs
    if not isinstance(method, ast.StructElem):
        return method
    elif method.elem.value == "init":
        return method.struct
    errors.not_implemented(context.line, context.exit_on_error)


def get_type_from_expr(expr, *, context):
    if isinstance(expr, ast.Name):
        return ast.Name(context.namespace.get(expr.value)["type"])
    elif isinstance(expr, ast.MethodCall):
        if ((not isinstance(expr.method, ast.StructElem)) or \
                expr.method.elem.value == "init"):
            return _get_type_from_method(expr.method, context=context)
        else:
            errors.not_implemented(context.line, context.exit_on_error)
    elif isinstance(expr, defs.ATOM_TYPES):
        return ast.Name(expr.to_string())
    elif isinstance(expr, list):
        # Expression contains only one type atoms
        return get_type_from_expr(expr[1], context=context)
    errors.not_implemented(context.line, context.exit_on_error)