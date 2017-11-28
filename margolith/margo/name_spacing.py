from . import layers, astlib, errors, defs
from .patterns import A
from .context import context


def n(name):
    # adr_u_LOL
    is_tmp = False
    for_processing = name
    if name in A(astlib.Name):
        for_processing = str(name)
        is_tmp = name.is_tmp

    result = defs.ADR_PREFIX
    if not is_tmp:
        result += "_".join(["", defs.USER_PREFIX])

    if context.file_hash != "":
        result += "_".join(["", context.file_hash])
    result += "_".join(["", for_processing])
    return astlib.Name(result)


def struct_func_name(struct, name):
    for_processing = name
    struct_str = struct
    if name in A(astlib.Name):
        for_processing = str(name)
    if struct in A(astlib.Name):
        struct_str = str(struct)
    for_processing = "_".join([struct_str, for_processing])
    result = "_".join([defs.ADR_PREFIX, defs.USER_PREFIX])
    if context.file_hash != "":
        result += "_".join(["", context.file_hash])
    result += "_".join(["", for_processing])
    return astlib.Name(result)


def t(type_):
    if type_ in A(astlib.CType, astlib.CObject):
        return type_

    if type_ in A(astlib.Name):
        return n(type_)

    if type_ in A(astlib.ParameterizedType):
        return astlib.ParameterizedType(
            t(type_.type_), [t(p) for p in type_.parameters])

    errors.not_implemented(
        context.exit_on_error,
        "namespacing: type (type {})".format(type_))


def decl_args(args):
    result = []
    for arg in args:
        result.append(astlib.Arg(n(arg.name), t(arg.type_)))
    return result


def call_args(args):
    return list(map(e, args))


def e(expr):
    if expr in A(astlib.Name):
        return n(expr)

    if expr in A(astlib.Expr):
        return astlib.Expr(
            expr.op, e(expr.left_expr), e(expr.right_expr))

    if expr in A(astlib.CINT_TYPES):
        return expr

    if expr in A(astlib.CFuncCall):
        return astlib.CFuncCall(
            expr.name, call_args(expr.args))

    if expr in A(astlib.FuncCall):
        return astlib.FuncCall(
            n(expr.name), call_args(expr.args))

    if expr in A(astlib.StructFuncCall):
        return astlib.FuncCall(
            struct_func_name(expr.struct, expr.func_name),
            call_args(expr.args))

    if expr in A(astlib.StructCall):
        return astlib.FuncCall(
            struct_func_name(expr.name, defs.INIT_METHOD_NAME),
            call_args(expr.args))

    if expr in A(astlib.StructScalar):
        return astlib.StructScalar(t(expr.type_))

    if expr in A(astlib.Deref):
        return astlib.Deref(e(expr.expr))

    if expr in A(astlib.StructMember):
        return astlib.StructMember(e(expr.struct), e(expr.member))

    errors.not_implemented(
        context.exit_on_error,
        "namespacing: expr (expr {})".format(expr))


class NameSpacing(layers.Layer):

    def __init__(self, inlined_structs=None):
        self.inlined_structs = inlined_structs or []

    def body(self, body):
        reg = NameSpacing(inlined_structs=self.inlined_structs).get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body))

    def inlined(self, struct_func_decl):
        return str(struct_func_decl.struct) in self.inlined_structs

    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        yield astlib.VarDecl(
            n(declaration.name),
            t(declaration.type_),
            e(declaration.expr))

    @layers.register(astlib.LetDecl)
    def let_decl(self, declaration):
        yield astlib.LetDecl(
            n(declaration.name),
            t(declaration.type_),
            e(declaration.expr))

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield astlib.Assignment(
            e(stmt.variable), stmt.op,
            e(stmt.expr))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield astlib.CFuncCall(
            call.name, call_args(call.args))

    @layers.register(astlib.StructFuncCall)
    def struct_func_call(self, call):
        yield astlib.FuncCall(
            struct_func_name(call.struct, call.func_name),
            call_args(call.args))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield astlib.FuncCall(
            n(call.name), call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield astlib.Return(e(return_.expr))

    @layers.register(astlib.FuncDecl)
    def func(self, func):
        yield astlib.FuncDecl(
            n(func.name), decl_args(func.args),
            t(func.rettype), self.body(func.body))

    @layers.register(astlib.StructFuncDecl)
    def struct_func(self, stmt):
        if self.inlined(stmt):
            yield from []
        else:
            yield astlib.FuncDecl(
                struct_func_name(stmt.struct, stmt.func),
                decl_args(stmt.args),
                t(stmt.rettype), self.body(stmt.body))

    @layers.register(astlib.StructDecl)
    def struct(self, struct):
        if struct.var_types != []:
            self.inlined_structs.append(str(struct.name))
        yield astlib.StructDecl(
            n(struct.name), struct.var_types,
            self.body(struct.body))

    @layers.register(astlib.FieldDecl)
    def field(self, field):
        yield astlib.FieldDecl(
            n(field.name), t(field.type_))