from . import defs, layers, astlib, errors
from .context import context
from .patterns import A
from adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast16": "int_fast16",
    "IntFast32": "int_fast32",
    "IntFast64": "int_fast64",
    "UIntFast8": "uint_fast8",
    "UIntFast16": "uint_fast16",
    "UIntFast32": "uint_fast32",
    "UIntFast64": "uint_fast64",
}

TO_COP = {
    "+": cgen.COps.plus,
    "-": cgen.COps.minus,
    "*": cgen.COps.star,
    "/": cgen.COps.slash,
}


def cfunc_call(call):
    if call.name == defs.SIZEOF_FUNC_NAME:
        yield cgen.SizeOf(*call_args(call.args))
    elif call.name in defs.CFUNCS:
        yield getattr(defs, str(call.name).upper() + "_FUNC_DESCR")(
            *call_args(call.args))


def func_call(call):
    yield cgen.FuncCall(
        str(call.name), *call_args(call.args))


def t(type_):
    if type_ in A(astlib.CType):
        if str(type_) == "Void":
            return cgen.CTypes.void
        return cgen.CTypes.ptr(getattr(cgen.CTypes, TO_CTYPE[str(type_)]))

    if type_ in A(astlib.Name):
        return cgen.CTypes.ptr(cgen.StructType(str(type_)))
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t (type_ {})".format(type_))


def t_without_ptr(type_):
    if type_ in A(astlib.CType):
        return getattr(cgen.CTypes, TO_CTYPE[str(type_)])

    if type_ in A(astlib.Name):
        return str(type_)
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t_without_ptr (type_ {})".format(type_))


def e(expr):
    if expr in A(astlib.CINT_TYPES):
        return cgen.Val(
            literal=expr.literal,
            type_=getattr(
                cgen.CTypes, TO_CTYPE[str(expr.to_type())]))

    if expr in A(astlib.Deref):
        return cgen.DeRef(e(expr.expr))

    if expr in A(astlib.Name):
        return cgen.Var(str(expr))

    if expr in A(astlib.CFuncCall):
        return list(cfunc_call(expr))[0]

    if expr in A(astlib.FuncCall):
        return list(func_call(expr))[0]

    if expr in A(astlib.StructScalar):
        if expr.type_ in A(astlib.CType):
            return t_without_ptr(expr.type_)
        return cgen.StructType(t_without_ptr(expr.type_))

    if expr in A(astlib.StructMember):
        return cgen.StructElem(
            cgen.CTypes.ptr(e(expr.struct)), e(expr.member))

    if expr in A(astlib.Expr):
        return cgen.Expr(
            TO_COP[expr.op], e(expr.left_expr), e(expr.right_expr))

    errors.not_implemented(
        context.exit_on_error,
        "tocgen: e (expr {} {})".format(expr, type(expr)))


def decl_args(args):
    result = []
    for arg in args:
        result.append(
            cgen.Decl(str(arg.name), type_=t(arg.type_)))
    return result


def call_args(args):
    return list(map(e, args))


class ToCGen(layers.Layer):

    def b(self, body):
        reg = ToCGen().get_registry()
        return list(map(
            lambda stmt: list(
                layers.transform_node(stmt, registry=reg))[0],
            body))

    @layers.register(astlib.VarDecl)
    def decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=t(decl.type_),
            expr=e(decl.expr))

    @layers.register(astlib.LetDecl)
    def let_decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=t(decl.type_),
            expr=e(decl.expr))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        yield from cfunc_call(call)

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield from func_call(call)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield cgen.Assignment(
            name=e(assignment.variable),
            expr=e(assignment.expr))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield cgen.Return(e(return_.expr))

    @layers.register(astlib.FuncDecl)
    def func(self, func):
        yield cgen.Func(
            str(func.name), t(func.rettype),
            decl_args(func.args), self.b(func.body))

    @layers.register(astlib.StructDecl)
    def struct(self, struct):
        yield cgen.Struct(
            name=str(struct.name),
            body=self.b(struct.body))

    @layers.register(astlib.FieldDecl)
    def field(self, field):
        yield cgen.Decl(
            name=str(field.name),
            type_=t(field.type_))
