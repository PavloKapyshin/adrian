"""Translates AST into more simple (for computer)."""

from . import ast
from . import defs
from . import errors

from vendor.paka import funcreg


_FUNCS = funcreg.TypeRegistry()


def std_type_init_args(type_, value):
    std_type_init_args.reg[type_.value](value)


std_type_init_args.reg = funcreg.NameRegistry()


@std_type_init_args.reg.register(ast.Integer.to_string())
def _init_args(value):
    return ast.CString(value.value)


def std_type_init_args_from_name(type_, value):
    if isinstance(type_, ast.ModuleMember):
        return std_type_init_args_from_name.reg[type_.member](value)
    return std_type_init_args_from_name.reg[type_.value](value)


std_type_init_args_from_name.reg = funcreg.NameRegistry()


@std_type_init_args_from_name.reg.register(ast.Integer.to_string())
def _init_args_from_name_integer(value):
    return ast.StructElem(value.value, "digits")

@std_type_init_args_from_name.reg.register(defs.CTYPES_INT32_STRING)
def _init_args_from_name_int32(value):
    return value


@std_type_init_args_from_name.reg.register(defs.CTYPES_INT64_STRING)
def _init_args_from_name_int64(value):
    return value


def determine_std_type(value, *, context):
    if isinstance(value, list):
        return determine_std_type(value[1], context=context)
    elif isinstance(value, ast.Name):
        return context.namespace.get(value.value)["type_"]
    return ast.Name(value.to_string())


def translate_std_type(type_):
    if (isinstance(type_, ast.Name) and \
            type_.value in defs.STD_TYPE_NAMES):
        return ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_.value)
    return type_


def translate_std_type_value(type_, value, *, context):
    if not type_:
        type_ = determine_std_type(value, context=context)
    base = ast.FuncCall(ast.StructElem(
        ast.ModuleMember(defs.STD_TYPES_MODULE_NAME, type_), elem=None), args=None)
    if isinstance(value, ast.Name):
        base.name.elem = ast.Name("init")
        base.args = std_type_init_args_from_name(type_, value)
    elif isinstance(value, list):
        _d = {
            "+": "add",
            "-": "sub",
            "*": "mul",
            "/": "div"
        }
        base.name.elem = ast.Name(_d[value[0]])
        base.args = (
            translate_std_type_value(type_, value[1]),
            translate_std_type_value(type_, value[2]))
    elif value:
        base.name.elem = ast.Name("init")
        base.args = std_type_init_args(type_, value)
    return base


def translate_method(struct_name, method, *, context):
    return translate_method.reg[method.name](
        struct_name, method, context=context)


translate_method.reg = funcreg.NameRegistry()


@translate_method.reg.register("init")
def _translate_init_method(struct_name, method, *, context):
    self_decl = ast.Pair(0, ast.Decl(ast.Name("self"), type_=ast.Name(struct_name),
        value=ast.FuncCall(ast.ModuleMember(ast.Name("c"), ast.Name("malloc")),
            args=(ast.FuncCall(
                ast.ModuleMember(ast.Name("c"), ast.Name("sizeof")),
                args=(ast.StructScalar(struct_name)))))))
    context.namespace.add_name("self", {
        "type_": self_decl.type_,
        "node_type": defs.NodeType.variable,
        "value": self_decl.value
    })
    return_stmt = ast.Pair(0, ast.ReturnStmt(ast.Name("self")))
    method.body = [self_decl] + method.body + [return_stmt]
    method.name = "init" + struct_name
    return method


def struct_body(body, *, context):
    methods = []
    new_body = []
    for pair in body:
        result = _FUNCS[pair.stmt](pair, context=context)
        if isinstance(result.stmt, ast.FuncDecl):
            methods.append(result)
        else:
            new_body.append(result)
    return new_body, methods


def func_body(body, *, context):
    return translate(body, context=context)


def func_args(args, *, context):
    result = []
    for arg in args:
        type_ = translate_std_type(arg.type_)
        context.namespace.add_name(arg.name, {
            "type_": type_,
            "node_type": defs.NodeType.variable,
            "value": None
        })
        result.append(ast.Arg(arg.name, arg.type_))
    return result


@_FUNCS.register(ast.Assignment)
def assignment(pair, *, context):
    stmt = pair.stmt
    value = translate_std_type_value(None, stmt.value, context=context)
    return ast.Pair(pair.line, ast.Assignment(stmt.name, stmt.op, value))


@_FUNCS.register(ast.FuncDecl)
def func_decl(pair, *, context):
    stmt = pair.stmt
    context.namespace.add_scope()
    args = func_args(stmt.args, context=context)
    type_ = translate_std_type(stmt.type_)
    context.funcspace.add_name(stmt.name, {
        "type_": type_,
        "args": args
    })
    body = func_body(stmt.body, context=context)
    context.namespace.del_scope()
    return ast.Pair(pair.line, ast.FuncDecl(stmt.name, args, type_, body))


@_FUNCS.register(ast.StructDecl)
def struct_decl(pair, *, context):
    stmt = pair.stmt
    context.namespace.add_scope()
    body, funcs = struct_body(stmt.body, context=context)
    new_funcs = [
        ast.Pair(func.line, translate_method(
            stmt.name, func.stmt, context=context))
        for func in funcs]
    new = ast.Pair(pair.line, ast.StructDecl(stmt.name, body))
    context.namespace.del_scope()
    return [new] + new_funcs


@_FUNCS.register(ast.Decl)
def decl(pair, *, context):
    stmt = pair.stmt
    type_ = stmt.type_
    value = stmt.value
    if (isinstance(stmt.type_, ast.Name) and \
            stmt.type_.value in defs.STD_TYPE_NAMES):
        type_ = ast.ModuleMember("std_types", stmt.type_.value)
        value = translate_std_type_value(stmt.type_, stmt.value, context=context)
    context.namespace.add_name(stmt.name, {
        "type_": type_,
        "node_type": defs.NodeType.variable,
        "value": value
    })
    return ast.Pair(pair.line, ast.Decl(stmt.name, type_, value))


def translate(ast_, *, context):
    result = []
    for pair in ast_:
        sub_result = _FUNCS[pair.stmt](pair, context=context)
        if isinstance(sub_result, list):
            result.extend(sub_result)
        else:
            result.append(sub_result)
    return result


def main(ast_, *, context=ast.Context(
        exit_on_error=True, module_paths=["std_modules/"])):
    return translate(ast_, context=context)
