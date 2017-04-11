from . import ast
from . import defs

from vendor.paka import funcreg
from vendor.adrian import cgen.objects


_FUNCS = funcreg.TypeRegistry()


def _generate_value(value):
    if isinstance(value, defs.NAME_TYPES):
        return cgen.objects.Var(value.value)
    elif isinstance(value, defs.ATOM_TYPES):
        return cgen.objects.Val(value.value, )  # TODO: Type?
    elif isinstance(value, list):
        return cgen.objects.FuncCall( , _generate_value(value[1]),    # TODO: Operator?
            _generate_value(value[2]))


def generate_value(stmt):
    return generate_value.registry[stmt](stmt)


generate_value.registry = funcreg.TypeRegistry()


@generate_value.registry.register(ast.Assignment)
def _generate_assignment_value(stmt):
    return _generate_value(stmt.value)


@_FUNCS.register(ast.Assignment)
def assignment(pair):
    return cgen.objects.Decl(pair.stmt.name.value, generate_value(pair.stmt))


def generate(ast_, *, context):
    return [_FUNCS[pair.stmt](pair, context=context) for pair in ast_]


def main(ast_, *, context=ast.Context(exit_on_error=True):
    return generate(ast_, context=context)
