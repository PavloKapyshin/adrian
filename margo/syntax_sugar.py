"""Translates AST into more simple (for computer)."""

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def std_type_init_args_from_name(type_, value):
    # TODO: refactor
    if type_.value == "Integer":
        return ast.StructElem(value.value, "digits")


def std_type_init_args(type_, value):
    # TODO: refactor
    if type_.value == "Integer":
        return ast.CString(value.value)


def translate_std_type(type_):
    if type_ in defs.STD_TYPES_NAMES:
        return ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_)


def determine_std_type(value, *, context):
    if isinstance(value, list):
        return determine_std_type(value[1], context=context)
    elif isinstance(value, ast.Name):
        return context.namespace.get(value)["type_"]
    return ast.Name(value.to_string())


def translate_std_type_value(type_, value, *, context):
    if not type_:
        type_ = determine_std_type(value, context=context)
    if isinstance(value, ast.Name):
        return ast.FuncCall(ast.StructElem(
            ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("__init__")), args=std_type_init_args_from_name(type_, value))
    elif isinstance(value, list):
        _d = {
            "+": "add",
            "-": "sub",
            "*": "mul",
            "/": "div"
        }
        return ast.FuncCall(ast.StructElem(
            ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("__" + _d[value[0]] + "__")), args=(
                translate_std_type_value(type_, value[1]),
                translate_std_type_value(type_, value[2])))
    elif value:
        return ast.FuncCall(ast.StructElem(
            ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_),
            ast.Name("__init__")), args=std_type_init_args(type_, value))


def _translate_init_method(struct_name, method, *, context):
    """Adds declaration of self and return statement."""
    self_decl = ast.Pair(0, ast.Decl(ast.Name("self"), type_=struct_name,
        value=ast.FuncCall(ast.ModuleMember(ast.Name("c"), ast.Name("malloc")),
            args=(ast.FuncCall(
                ast.ModuleMember(ast.Name("c"), ast.Name("sizeof")),
                args=(ast.StructScalar(struct_name)))))))
    return_stmt = ast.Pair(0, ast.ReturnStmt(ast.Name("self")))
    method.body = [self_decl] + method.body + [return_stmt]
    method.name = "init" + struct_name
    return method


def translate_method(struct_name, method, *, context):
    # FuncDecl(name, args, type_, body)
    if method.name == "__init__":
        return _translate_init_method(struct_name, method, context=context)
    else:
        errors.not_implemented()


def struct_body(body, *, context):
    funcs = []
    new_body = []
    for pair in body:
        result = _FUNCS[pair.stmt](pair, context=context)
        if isinstance(result.stmt, ast.FuncDecl):
            funcs.append(result)
        else:
            new_body.append(result)
    return new_body, funcs


def func_args(args):
    return [new_args.append(ast.Arg(arg.name, translate_std_type(arg.type_)))
            for arg in args]


def func_body(body, *, context):
    return translate(body, context=context)


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    value = translate_std_type_value(None, stmt.value, context=context)
    return ast.Pair(pair.line, ast.Assignment(stmt.name, stmt.op, value))


@_FUNCS.register(ast.FuncDecl)
def func_decl(pair, *, context):
    stmt = pair.stmt
    args = func_args(stmt.args)
    body = func_body(stmt.body, context=context)
    new = ast.Pair(pair.line, ast.FuncDecl(stmt.name, args, stmt.type_, body))
    return new


@_FUNCS.register(ast.StructDecl)
def struct_decl(pair, *, context):
    stmt = pair.stmt
    body, funcs = struct_body(stmt.body, context=context)
    new_funcs = [
        ast.Pair(func.line, translate_method(
            stmt.name, func.stmt, context=context))
        for func in funcs]
    new = ast.Pair(pair.line, ast.StructDecl(stmt.name, body))
    return [new] + new_funcs


@_FUNCS.register(ast.Decl)
def decl(pair, *, context):
    stmt = pair.stmt
    type_ = stmt.type_
    value = stmt.value
    if (isinstance(stmt.type_, ast.Name) and \
            stmt.type_.value in defs.STANDARD_TYPE_NAMES):
        type_ = ast.ModuleMember(module_name="std_types", member=stmt.type_)
        value = translate_std_type_value(stmt.type_, stmt.value, context=context)
    return ast.Pair(pair.line, ast.Decl(stmt.name, type_, value))


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        res_ = _FUNCS[pair.stmt](pair, context=context)
        if isinstance(res_, list):
            result.extend(res_)
        else:
            result.append(res_)
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    return translate(ast_, context=context)
