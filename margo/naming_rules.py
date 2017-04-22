import re
import pathlib

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def _matches_name(name, regex):
    return regex.fullmatch(name)


def matches_variable_name(name):
    return _matches_name(name, defs.VARIABLE_REGEX)


def matches_type_name(name):
    return _matches_name(name, defs.TYPE_REGEX)


def matches_module_name(name):
    return _matches_name(name, defs.MODULE_REGEX)


def check_decl_name(name, *, context):
    if not matches_variable_name(name.value):
        errors.bad_name_for_variable(
            context.line, context.exit_on_error, name=name.value)


def check_module_name(name, *, context):
    if not matches_module_name(name):
        errors.bad_name_for_module(
            context.line, context.exit_on_error, name=name)


def check_type_name(name, *, context):
    if not matches_type_name(name.value):
        errors.bad_name_for_type(
            context.line, context.exit_on_error, name=name.value)


def check_decl_type(type_, *, context):
    if isinstance(type_, ast.Name):
        check_type_name(type_, context=context)
    elif isinstance(type_, ast.ModuleMember):
        check_module_name(type_.module_name, context=context)
        check_decl_type(type_.member, context=context)
    elif isinstance(type_, ast.MethodCall):
        check_decl_type(type_.method, context=context)
    elif isinstance(type_, ast.StructElem):
        check_decl_type(type_.struct, context=context)
        check_name(type_.elem, context=context)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


def check_call_args(args, *, context):
    for arg in args:
        check_expr(arg, context=context)


def check_name(expr, *, context):
    if isinstance(expr, ast.Name):
        if not matches_variable_name(expr.value):
            errors.bad_name_for_variable(
                context.line, context.exit_on_error, name=expr.value)
    elif isinstance(expr, ast.ModuleMember):
        check_module_name(expr.module_name, context=context)
        check_name(expr.member, context=context)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


def check_expr(expr, *, context):
    if isinstance(expr, (ast.Name, ast.ModuleMember)):
        check_name(expr, context=context)
    elif isinstance(expr, defs.ATOM_TYPES):
        pass
    elif isinstance(expr, list):
        check_expr(expr[1], context=context)
        check_expr(expr[2], context=context)
    elif isinstance(expr, ast.MethodCall):
        check_decl_type(expr.method, context=context)
        check_call_args(expr.args, context=context)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    check_decl_name(stmt.name, context=context)
    if stmt.type_:
        check_decl_type(stmt.type_, context=context)
    if stmt.expr:
        check_expr(stmt.expr, context=context)


def check(ast_, *, context):
    for pair in ast_:
        context.line = pair.line
        _FUNCS[pair.stmt](pair.stmt, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    check(ast_, context=context)
