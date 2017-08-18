from . import astlib, errors
from .context import context, get
from .patterns import A


def get_field_entity(struct_elem):
    name_type = get(struct_elem.name)["type"]
    fields = get(name_type)["fields"]
    elem = struct_elem.elem
    while not elem in A(astlib.Name):
        name_type = fields[str(elem.name)]
        fields = get(name_type)["fields"]
        elem = elem.elem
    return fields[str(elem)]


def infer(expr):
    if expr in A(astlib.CTYPES):
        return expr.to_type()

    if expr in A(astlib.Name):
        entity = get(expr)
        if entity:
            return entity["type"]
        else:
            errors.non_existing_name(
                context.exit_on_error, name=str(expr))

    if expr in A(astlib.StructElem):
        return get_field_entity(expr)

    if expr in A(astlib.Expr):
        # +, -, * and / returns the value of the same type.
        return infer(expr.lexpr)

    if expr in A(astlib.FuncCall):
        entity = get(expr.name)
        if entity:
            return entity["type"]
        else:
            errors.non_existing_name(
                context.exit_on_error, name=str(expr.name))

    errors.not_implemented(
        context.exit_on_error,
        "can't infer type (expr {})".format(expr))