import re
import pathlib

from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def get_module_path(module_name, *, context):
    """
    TODO: look for a member in module
    """
    for path in context.module_paths:
        # TODO: if a directory
        for module in pathlib.Path(path).iterdir():
            if module_name == module.name:
                return module
    if module_name == "ctypes":
        return "ctypes"
    return False


def add_include(module_name, module_path, *, context):
    if module_name == "ctypes":
        return
    if not module_name in context.includes:
        context.includes[module_name] = layers.mangle_name(
            module_name, file_hash=context.file_hash)


def check_var_const_name(name, *, context):
    # TODO: very bad errors should be in this func
    entry = context.namespace.get(name)
    if entry and entry["node_type"] == defs.NodeType.variable:
        var_name = re.compile(r"[a-z_][a-zA-Z0-9]*")
        if var_name.fullmatch(name):
            return True
    elif entry and entry["node_type"] == defs.NodeType.constant:
        const_name = re.compile(r"[A-Z_][A-Z_0-9]*")
        if const_name.fullmatch(name):
            return True
    return False


def check_value(value, *, context):
    """Check that value (atom or a list of atoms) is valid."""
    if isinstance(value, ast.Name):
        if not layers.name_exists(value.value, context=context):
            errors.non_existing_name(
                context.line, context.exit_on_error, name=value.value)
        check_var_const_name(value.value, context=context)
        return value
    elif isinstance(value, defs.ATOM_TYPES):
        return value
    elif isinstance(value, ast.ModuleMember):
        module_path = get_module_path(value.module_name, context=context)
        if module_path:
            add_include(value.module_name, module_path, context=context)
        return check_value(value.member, context=context)
    elif isinstance(value, list):
        return [
            value[0],
            check_value(value[1], context=context),
            check_value(value[2], context=context)
        ]


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    context.line = pair.line
    if stmt.name.value in defs.STANDARD_FUNC_NAMES:
        errors.cant_reassign_builtin(
            context.line, context.exit_on_error, name=stmt.name.value)
    if isinstance(stmt.type_, ast.ModuleMember):
        module_path = get_module_path(stmt.type_.module_name, context=context)
        if module_path:
            add_include(stmt.type_.module_name, module_path, context=context)
        else:
            errors.non_existing_name_in_module(
                context.line, context.exit_on_error,
                name=stmt.type_.member, module=stmt.type_.module_name)
    elif not layers.type_exists(stmt.type_, context=context):
        errors.non_existing_name(context.line, context.exit_on_error, name=stmt.type_)
    value = check_value(stmt.value, context=context)
    context.namespace.add_name(stmt.name.value, {
        "node_type": defs.NodeType.variable,
        "type": stmt.type_,
        "value": value
    })
    return ast.Pair(pair.line, ast.Assignment(stmt.name.value, stmt.type_, value))


def check(ast_, *, context):
    return [_FUNCS[pair.stmt](pair, context=context) for pair in ast_]


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    return check(ast_, context=context)
