import pathlib

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def name_in_module(name, module_name, *, context):
    # TODO: parse module, create a context for this module, search for name.
    return True


def get_module_path(module_name, *, context):
    for path in context.module_paths:
        for module in pathlib.Path(path).iterdir():
            if module_name == module.name:
                return module
    if module_name == defs.C_MODULE_NAME:
        return module_name
    elif module_name in defs.STD_MODULE_NAMES:
        return defs.STD_MODULES_PATH + module_name + defs.ADRIAN_FILE_EXTENSION
    errors.non_existing_module(
        context.line, context.exit_on_error, module=module_name)


def add_include(module_name, module_path, *, context):
    if (not module_name in context.includes and \
            module_name != defs.C_MODULE_NAME):
        context.includes[module_name] = module_path


def check_module_existence(module_name, *, context):
    path = get_module_path(module_name, context=context)
    add_include(module_name, path, context=context)


def check_type_existence(type_, *, context):
    if isinstance(type_, ast.Name):
        if not (type_.value in defs.STD_TYPE_NAMES or \
                context.typespace.exists(type_.value)):
            errors.non_existing_type(
                context.line, context.exit_on_error, name=type_.value)
    elif isinstance(type_, ast.ModuleMember):
        check_module_existence(type_.module_name, context=context)
        if not name_in_module(
                type_.member, type_.module_name, context=context):
            errors.cant_find_name_in_module(
                context.line, context.exit_on_error,
                name=type_.member, module_name=type_.module_name)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


def check_name_existence(name, *, context):
    if isinstance(name, ast.Name):
        if not context.namespace.exists(name.value):
            errors.non_existing_name(
                context.line, context.exit_on_error, name=name.value)
    elif isinstance(name, ast.ModuleMember):
        check_module_existence(name.module_name, context=context)
        if not name_in_module(
                name.member, name.module_name, context=context):
            errors.cant_find_name_in_module(
                context.line, context.exit_on_error,
                name=name.member, module_name=name.module_name)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


def check_call_args(args, *, context):
    for arg in args:
        check_expr(arg, context=context)


def check_expr(expr, *, context):
    if isinstance(expr, (ast.Name, ast.ModuleMember)):
        check_name_existence(expr, context=context)
    elif isinstance(expr, ast.MethodCall):
        check_type_existence(expr.method, context=context)
        check_call_args(expr.args, context=context)
    elif isinstance(expr, defs.ATOM_TYPES):
        pass
    elif isinstance(expr, list):
        check_expr(expr[1], context=context)
        check_expr(expr[2], context=context)
    else:
        errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    if stmt.name.value in defs.STD_FUNCS:
        errors.cant_reassign_builtin(
            context.line, context.exit_on_error, name=stmt.name.value)
    if stmt.type_:
        check_type_existence(stmt.type_, context=context)
    if stmt.expr:
        check_expr(stmt.expr, context=context)
    context.namespace.add_name(
        stmt.name.value, {"node_type": defs.NodeType.variable})


def check(ast_, *, context):
    for pair in ast_:
        context.line = pair.line
        _FUNCS[pair.stmt](pair.stmt, context=context)


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    check(ast_, context=context)
