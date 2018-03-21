from .utils import A
from .context import context
from . import astlib, errors, defs


def _get_arg(expr, index):
    if expr in A(astlib.Name):
        context.env[expr]


def _infer_params_from_args(struct_params, decl_args, call_args):
    # TODO: * add support of nested generic types
    mapping = {}
    i = 0
    for _, argument_type in decl_args:
        if argument_type in struct_params:
            if not argument_type in mapping:
                mapping[argument_type] = infer_type(call_args[i])
        i += 1
    result = [mapping[param] for param in struct_params]
    return result


def _infer_init_args(type_):
    info = context.env[type_]
    return [
        infer_expr(t) for _, t in info["methods"]["__init__"]["args"]]


def _for_env(type_):
    if type_ in A(astlib.ParamedType):
        return type_.type_
    return type_


def _infer_expr_literal(type_):
    if type_.type_ == astlib.LiteralT.integer:
        return astlib.Literal(astlib.LiteralT.integer, "0")


def _infer_expr_datamember(type_):
    if type_.datatype == astlib.DataT.module:
        return astlib.Callable(
            astlib.CallableT.struct, astlib.Empty(),
            type_, _infer_init_args(type_))


def infer_expr(type_):
    if type_ in A(astlib.LiteralType):
        return _infer_expr_literal(type_)
    elif type_ in A(astlib.DataMember):
        return _infer_expr_datamember(type_)
    elif type_ in A(astlib.Name):
        return astlib.Callable(
            astlib.CallableT.struct, astlib.Empty(),
            type_, _infer_init_args(type_))
    errors.not_now(errors.EXPR_INFERENCE)


def _infer_type_struct_func(expr):
    info = context.env[_for_env(expr.parent)]
    struct_params = info["params"]
    method = info["methods"][expr.name]
    if len(struct_params) == 0:
        return method["type_"]
    return astlib.ParamedType(
        expr.parent, _infer_params_from_args(
            struct_params, method["args"], expr.args))


def _infer_type_callable(expr):
    if expr.callabletype == astlib.CallableT.struct:
        return _infer_type_struct_func(
            astlib.Callable(
                astlib.CallableT.struct_func, expr.name,
                defs.INIT_METHOD, expr.args))
    elif expr.callabletype == astlib.CallableT.fun:
        return context.env[expr.name]["type_"]
    elif expr.callabletype == astlib.CallableT.struct_func:
        return _infer_type_struct_func(expr)


def _infer_type_datamember(expr):
    def check(result, params):
        if result in A(astlib.Name):
            return result in params
        return any([check(t, params) for t in result.params])

    def replaced(result, params, mapping):
        if result in A(astlib.Name):
            if check(result, params):
                return mapping[result]
            return result
        else:
            if check(result, params):
                return astlib.ParamedType(
                    result.type_,
                    [replaced(t, params, mapping) for t in result.params])

    info = context.env[_for_env(infer_type(expr.parent))]
    result = info["fields"][expr.member]["type_"]
    if "mapping" in context.env[expr.parent]:
        return replaced(
            result, info["params"], context.env[expr.parent]["mapping"])
    return result


def infer_type(expr):
    # TODO:
    #   * add more
    if expr in A(astlib.Callable):
        return _infer_type_callable(expr)
    elif expr in A(astlib.Name):
        return context.env[expr]["type_"]
    elif expr in A(astlib.DataMember):
        if expr.datatype == astlib.DataT.struct:
            return _infer_type_datamember(expr)
    elif expr in A(astlib.Ref):
        return infer_type(expr.expr)
    errors.not_now(errors.TYPE_INFERENCE)
