from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def translate_method(method, *, context):
    if isinstance(method, ast.StructElem):
        struct = translate_method(method.struct, context=context)
        elem = translate_method(method.elem, context=context)
        if isinstance(struct, ast.ModuleMember):
            struct.member = elem + struct.member
        else:
            struct = elem + struct
        return struct
    elif isinstance(method, ast.Name):
        return method.value
    elif isinstance(method, ast.ModuleMember):
        method.member = translate_method(method.member, context=context)
        return method
    elif isinstance(method, ast.MethodCall):
        method = translate_method(expr.method, context=context)
        args = translate_args(expr.args, context=context)
        return ast.FuncCall(method, args)
    else:
        print(method)
        errors.not_implemented(context.line, context.exit_on_error)


def translate_args(args, *, context):
    result = []
    for arg in args:
        if isinstance(arg, ast.MethodCall):
            result.append(ast.FuncCall(
                translate_method(arg.method, context=context),
                translate_args(arg.args, context=context)))
        else:
            result.append(arg)
    return result


def translate_expr(expr, *, context):
    if isinstance(expr, ast.MethodCall):
        method = translate_method(expr.method, context=context)
        args = translate_args(expr.args, context=context)
        return ast.FuncCall(method, args)
    else:
        print("expr")
        errors.not_implemented(context.line, context.exit_on_error)


@_FUNCS.register(ast.Decl)
def decl(stmt, *, context):
    expr = translate_expr(stmt.expr, context=context)
    return ast.Decl(stmt.name, stmt.type_, expr)


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