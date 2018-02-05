from . import defs, layers, astlib, errors
from .context import context
from .patterns import A
from adrian import cgen


def cfunc_call(call):
    if call.name == defs.SIZEOF_FUNC_NAME:
        yield cgen.SizeOf(*call_args(call.args))
    elif call.name in defs.CFUNCS:
        yield getattr(defs, str(call.name).upper() + "_FUNC_DESCR")(
            *call_args(call.args))


def func_call(call):
    name = call.name
    if name in A(astlib.ModuleMember):
        name = name.member
    yield cgen.FuncCall(
        str(name), *call_args(call.args))


def t(type_):
    if type_ in A(astlib.CVoid):
        return cgen.CTypes.void

    if type_ in A(astlib.ModuleMember):
        return t(type_.member)

    if type_ in A(astlib.Name):
        return cgen.CTypes.ptr(cgen.StructType(str(type_)))

    if type_ in A(astlib.ParameterizedType):
        return t(type_.type_)
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t (type_ {})".format(type_))


def t_without_ptr(type_):
    # if type_ in A(astlib.CType):
    #     return getattr(cgen.CTypes, TO_CTYPE[str(type_)])
    if type_ in A(astlib.Name):
        return str(type_)
    errors.not_implemented(
        context.exit_on_error,
        "tocgen: t_without_ptr (type_ {})".format(type_))


def e(expr):
    if expr in A(astlib.Deref):
        return cgen.DeRef(e(expr.expr))

    if expr in A(astlib.Ref):
        return e(expr.expr)

    if expr in A(astlib.CCast):
        return cgen.Cast(e(expr.expr), t(expr.to))

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

    if expr in A(astlib.IntLiteral):
        return cgen.Val(
            literal=expr.literal,
            # Any type, :D
            type_=cgen.CTypes.int_fast64)

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

    @layers.register(astlib.While)
    def while_(self, stmt):
        yield cgen.While(e(stmt.expr), self.b(stmt.body))

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

    @layers.register(astlib.ADTDecl)
    def adt(self, stmt):
        yield cgen.Union(
            name=str(stmt.name),
            body=self.b(stmt.body))

    @layers.register(astlib.FieldDecl)
    def field(self, field):
        yield cgen.Decl(
            name=str(field.name),
            type_=t(field.type_))

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        l = list(layers.transform_ast(ast_, registry=registry))
        for cheader in context.clibs_cinc:
            if cheader["type"] == "adr":
                yield cgen.Include(cheader["src"])
            else:
                yield cgen.CInclude(cheader["src"])
        yield from l