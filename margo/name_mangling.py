from . import ast
from . import defs
from . import layers

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def mangle_name(name, *, file_hash):
    return "_".join(["adrian", file_hash, name])


def mangle(name, *, file_hash):
    if can_mangle(name):
        return layers.mangle_name(name, file_hash=file_hash)
    return name


def can_mangle(name):
    return (not (name in defs.STANDARD_FUNC_NAMES or \
            name in defs.STANDARD_TYPE_NAMES))


def translate_value(value, *, file_hash):
    if isinstance(value, ast.Name):
        return mangle(value.value, file_hash=file_hash)
    elif isinstance(value, defs.ATOM_TYPES):
        return value
    elif isinstance(value, ast.ModuleMember):
        if value.module_name == defs.CTYPES_MODULE_NAME:
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
    stmt = pair.stmt
    new_name = mangle(stmt.name.value, file_hash=file_hash)
    if (isinstance(stmt.type_, ast.ModuleMember) and \
            (stmt.type_.module_name == defs.CTYPES_MODULE_NAME)):
        new_type = stmt.type_
    else:
        new_type = mangle(stmt.type_, file_hash=file_hash)
    new_value = None
    if stmt.value:
        new_value = translate_value(stmt.value, file_hash=file_hash)
    return ast.Pair(pair.line, ast.Assignment(new_name, new_type, new_value))


def translate(pair, *, file_hash):
    return _FUNCS[pair.stmt](pair, file_hash=file_hash)


def main(ast_, *, file_hash):
    return [translate(pair, file_hash=file_hash) for pair in ast_]
