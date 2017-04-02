from . import ast
from . import defs

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def mangle(name, *, file_hash):
    if can_mangle(name):
        return "_".join([
            "adrian",
            file_hash,
            name
        ])
    return name


def can_mangle(name):
    return not (name in defs.STANDARD_FUNC_NAMES or name in defs.STANDARD_TYPE_NAMES)


def translate_value(value, *, file_hash):
    if isinstance(value, defs.NAME_TYPES):
        return mangle(value.value, file_hash=file_hash)
    elif isinstance(value, defs.ATOM_TYPES):
        return value
    elif isinstance(value, list):
        return [
            value[0],
            translate_value(value[1], file_hash=file_hash),
            translate_value(value[2], file_hash=file_hash)
        ]


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, file_hash):
    """var NAME: TYPE = 2 + NAME"""
    name = pair.stmt.name
    new_name = mangle(name.value, file_hash=file_hash)
    new_type = mangle(name.type_, file_hash=file_hash)
    new_value = None
    if pair.stmt.value:
        new_value = translate_value(pair.stmt.value, file_hash=file_hash)
    return ast.Pair(pair.line, ast.Assignment(new_name, new_type, new_value))


def translate(pair, *, file_hash):
    return _FUNCS[pair.stmt](pair, file_hash=file_hash)


def main(ast_, *, file_hash):
    return [translate(pair, file_hash=file_hash) for pair in ast_]
