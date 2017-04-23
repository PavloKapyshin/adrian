from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def type_from_alias(type_, *, context):
    if (isinstance(type_, ast.Name) and \
            type_.value in defs.STD_TYPE_NAMES):
        return ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_)
    return type_


def name_to_copy_method(name, *, context):
    if isinstance(name, ast.Name):
        type_ = context.namespace.get(name.value)["type"]
        return ast.MethodCall(
            ast.StructElem(type_, ast.Name("copy")), args=[name])
    errors.not_implemented(context.line, context.exit_on_error)


def method_call(call, *, context):
    type_ = call.method
    if not isinstance(type_, ast.StructElem):
        return ast.MethodCall(
            ast.StructElem(type_, ast.Name("init")), call.args)
    elif type_.elem.value == "init":
        # Bug can be here!
        return call
    errors.not_implemented(context.line, context.exit_on_error)


def std_type_init_args_from_alias(args, *, context):
    result = []
    for arg in args:
        if isinstance(arg, ast.Integer):
            # We usually can calculate length of an Integer when compiling.
            result.append(ast.MethodCall(
                ast.StructElem(ast.ModuleMember(
                    defs.C_MODULE_NAME, ast.Name(defs.C_CSTRING)), ast.Name("init")),
                [arg.value]))
        else:
            errors.not_implemented(context.line, context.exit_on_error)
    return result


def atom_to_init_method(atom, *, context):
    type_ = ast.Name(atom.to_string())
    args = std_type_init_args_from_alias([atom], context=context)
    return ast.MethodCall(ast.StructElem(
        ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
        ast.Name("init")), args)
    errors.not_implemented(context.line, context.exit_on_error)


def expr_from_alias(expr, *, context):
    if isinstance(expr, ast.Name):
        return name_to_copy_method(expr, context=context)
    elif isinstance(expr, ast.MethodCall):
        return method_call(expr, context=context)
    elif isinstance(expr, defs.ATOM_TYPES):
        return atom_to_init_method(expr, context=context)
    errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    type_ = type_from_alias(stmt.type_, context=context)
    context.namespace.add_name(stmt.name.value, {"type": type_})
    return ast.Decl(
        stmt.name, type_,
        expr_from_alias(stmt.expr, context=context))


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        context.line = pair.line
        result.append(ast.Pair(
            pair.line, _FUNCS[pair.stmt](pair.stmt, context=context)))
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=[defs.STD_MODULES_PATH])):
    return translate(ast_, context=context)