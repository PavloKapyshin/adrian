import re
import pathlib

from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def get_module_path(module_name, *, context):
    for path in context.module_paths:
        for module in pathlib.Path(path).iterdir():
            if module_name == module.name:
                return module
    if module_name == defs.CTYPES_MODULE_NAME:
        return defs.CTYPES_MODULE_NAME
    errors.non_existing_module(
        context.line, context.exit_on_error, module=module_name)


def add_include(module_name, module_path, *, context):
    if (not module_name in context.includes and \
            module_name != defs.CTYPES_MODULE_NAME):
        context.includes[module_name] = module_name


def check_name(name, node_type=None, *, context):
    entry = context.namespace.get(name)
    if node_type:
        regex_list = {
            defs.NodeType.variable: (defs.VARIABLE_REGEX, errors.bad_name_for_variable),
            defs.NodeType.constant: (defs.CONSTANT_REGEX, errors.bad_name_for_constant)
        }
        result = regex_list[node_type]
        if not result[0].fullmatch(name):
            result[1](context.line, context.exit_on_error, name=name)
    elif entry and entry["node_type"] == defs.NodeType.variable:
        if not defs.VARIABLE_REGEX.fullmatch(name):
            errors.bad_name_for_variable(
                context.line, context.exit_on_error, name=name)
    elif entry and entry["node_type"] == defs.NodeType.constant:
        if not defs.CONSTANT_REGEX.fullmatch(name):
            errors.bad_name_for_constant(
                context.line, context.exit_on_error, name=name)


def check_variable_name(name, *, context):
    check_name(name, defs.NodeType.variable, context=context)


def check_constant_name(name, *, context):
    check_name(name, defs.NodeType.constant, context=context)


def check_value(value, *, context):
    """Check that value (atom or a list of atoms) is valid."""
    if isinstance(value, ast.Name):
        if not layers.name.exists(value.value, context=context):
            errors.non_existing_name(
                context.line, context.exit_on_error, name=value.value)
        check_name(value.value, context=context)
    elif isinstance(value, defs.ATOM_TYPES):
        return value
    elif isinstance(value, ast.ModuleMember):
        module_path = get_module_path(value.module_name, context=context)
        if module_path:
            add_include(value.module_name, module_path, context=context)
        check_value(value.member, context=context)
    elif isinstance(value, list):
        check_value(value[1], context=context)
        check_value(value[2], context=context)


def _check_type_name(type_, *, context):
    if not defs.TYPE_REGEX.fullmatch(type_.value):
        errors.bad_name_for_type(
            context.line, context.exit_on_error, name=type_.value)


def check_type(type_, *, context):
    """Check that type is valid."""
    if isinstance(type_, ast.ModuleMember):
        module_path = get_module_path(type_.module_name, context=context)
        if module_path:
            add_include(type_.module_name, module_path, context=context)
            _check_type_name(type_.member, context=context)
        else:
            errors.non_existing_name_in_module(
                context.line, context.exit_on_error,
                name=type_.member, module=type_.module_name)
    elif not layers.type_exists(type_, context=context):
        errors.non_existing_name(
            context.line, context.exit_on_error, name=type_)
    else:
        _check_type_name(type_, context=context)


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    context.line = pair.line
    if stmt.name.value in defs.STANDARD_FUNC_NAMES:
        errors.cant_reassign_builtin(
            context.line, context.exit_on_error, name=stmt.name.value)
    check_variable_name(stmt.name.value, context=context)
    check_type(stmt.type_, context=context)
    check_value(stmt.value, context=context)
    context.namespace.add_name(stmt.name.value, {
        "node_type": defs.NodeType.variable,
        "type": stmt.type_,
        "value": stmt.value
    })


def check(ast_, *, context):
    for pair in ast_:
        _FUNCS[pair.stmt](pair, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    check(ast_, context=context)
