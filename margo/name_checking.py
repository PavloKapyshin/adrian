import re

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def name_exists(name, *, context):
    """Check for existence of a name."""
    return context.namespace.exists(name)


def type_exists(type_, *, context):
    """Check for existence of a type."""
    return (type_ in defs.STD_TYPE_NAMES or \
            context.typespace.exists(type_))


def func_exists(func, *, context):
    """Check for existence of a function."""
    return func in defs.STD_FUNCS or context.funcspace.exists(func)


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
        context.includes[module_name] = module_path


def _check_type_name(type_name, *, context):
    if not defs.TYPE_REGEX.fullmatch(type_name):
        errors.bad_name_for_type(
            context.line, context.exit_on_error, name=type_name)


def check_name(name, *, context):
    if name_exists(name, context=context):
        entry = context.namespace.get(name)
        if entry["node_type"] == defs.NodeType.variable:
            check_variable_name(name, context=context)
        elif entry["node_type"] == defs.NodeType.constant:
            check_constant_name(name, context=context)
    else:
        errors.non_existing_name(
            context.line, context.exit_on_error, name=name)


def check_value(value, *, context):
    if isinstance(value, ast.Name):
        if not name_exists(value.value, context=context):
            errors.non_existing_name(
                context.line, context.exit_on_error, name=value.value)
        check_name(value.value, context=context)
    elif isinstance(value, defs.ATOM_TYPES):
        # TODO: on funccall check name
        return value
    elif isinstance(value, ast.ModuleMember):
        module_path = get_module_path(value.module_name, context=context)
        add_include(value.module_name, module_path, context=context)
        check_value(value.member, context=context)
    elif isinstance(value, list):
        check_value(value[1], context=context)
        check_value(value[2], context=context)


def check_type(type_, *, context):
    if isinstance(type_, ast.ModuleMember):
        module_path = get_module_path(type_.module_name, context=context)
        add_include(type_.module_name, module_path, context=context)
        _check_type_name(type_.member, context=context)
    elif not type_exists(type_.value, context=context):
        errors.non_existing_name(
            context.line, context.exit_on_error, name=type_.value)
    else:
        _check_type_name(type_.value, context=context)


def check_variable_name(name, *, context):
    if not defs.VARIABLE_REGEX.fullmatch(name):
        errors.bad_name_for_variable(
            context.line, context.exit_on_error, name=name)


def check_constant_name(name, *, context):
    if not defs.CONSTANT_REGEX.fullmatch(name):
        errors.bad_name_for_constant(
            context.line, context.exit_on_error, name=name)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    if stmt.name in defs.STD_FUNCS:
        errors.cant_reassign_builtin(
            context.line, context.exit_on_error, name=stmt.name)
    check_variable_name(stmt.name, context=context)
    if stmt.type_:
        check_type(stmt.type_, context=context)
    if stmt.value:
        check_value(stmt.value, context=context)
    context.namespace.add_name(stmt.name, {
        "node_type": defs.NodeType.variable,
        "type": stmt.type_,
        "value": stmt.value,
    })


def check(ast_, *, context):
    for pair in ast_:
        context.line = pair.line
        _FUNCS[pair.stmt](pair.stmt, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    check(ast_, context=context)