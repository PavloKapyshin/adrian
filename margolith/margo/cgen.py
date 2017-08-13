"""Generates CGen AST."""

from . import cdefs, layers, astlib, errors, defs
from .context import context
from adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast32": "int_fast32",
    "IntFast64": "int_fast64",
    "UIntFast8": "uint_fast8",
    "UIntFast32": "uint_fast32",
    "UIntFast64": "uint_fast64",
    "Void": "void",
}

TO_COP = {
    "+": cgen.COps.plus,
    "-": cgen.COps.minus,
    "*": cgen.COps.star,
    "/": cgen.COps.slash,
}


class CGen(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.CType):
            return getattr(cgen.CTypes, TO_CTYPE[str(type_)])
        elif isinstance(type_, astlib.Ref):
            return self.type_(type_.literal)
        elif isinstance(type_, astlib.Name):
            return cgen.CTypes.ptr(cgen.StructType(str(type_)))
        elif isinstance(type_, astlib.Empty):
            return None
        elif isinstance(type_, astlib.CPtr):
            return cgen.CTypes.ptr(self.type_(type_.literal))
        elif isinstance(type_, astlib.CVoid):
            return cgen.CTypes.void
        errors.not_implemented("type is not supported (cgen layer)")

    def expr(self, expr):
        if isinstance(expr, astlib.CINT_TYPES):
            return cgen.Val(
                literal=expr.literal,
                type_=getattr(cgen.CTypes, TO_CTYPE[str(expr.to_type())]))
        elif isinstance(expr, astlib.Ref):
            return self.expr(expr.literal)
        elif isinstance(expr, astlib.Unref):
            return self.expr(expr.literal)
        elif isinstance(expr, astlib.StructScalar):
            return cgen.StructType(str(expr.name))
        elif isinstance(expr, astlib.StructElem):
            return cgen.StructElem(
                struct_name=cgen.CTypes.ptr(self.expr(expr.name)),
                elem_name=self.expr(expr.elem))
        elif isinstance(expr, astlib.Name):
            return cgen.Var(str(expr))
        elif isinstance(expr, astlib.CFuncCall):
            if expr.name == cdefs.SIZEOF_FUNC_NAME:
                return cgen.SizeOf(*self.call_args(expr.args))
            elif expr.name in cdefs.CFUNCS:
                return getattr(cdefs, str(expr.name).upper() + "_FUNC_DESCR")(
                    *self.call_args(expr.args))
        elif isinstance(expr, astlib.Expr):
            return cgen.Expr(
                TO_COP[expr.op], self.expr(expr.lexpr),
                self.expr(expr.rexpr))
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        elif isinstance(expr, astlib.CCast):
            return cgen.Cast(self.expr(expr.expr), to=self.type_(expr.to))
        errors.not_implemented("expr is not supported (cgen layer)")

    def call_args(self, args):
        return [self.expr(arg) for arg in args]

    def args(self, args):
        return [cgen.Decl(str(arg.name), self.type_(arg.type_))
                for arg in args]

    def body(self, body):
        reg = CGen().get_registry()
        return [list(layers.transform_node(stmt, registry=reg))[0]
                for stmt in body]

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=self.type_(decl.type_),
            expr=self.expr(decl.expr))

    @layers.register(astlib.Func)
    def func(self, func):
        yield cgen.Func(
            name=str(func.name), rettype=self.type_(func.rettype),
            args=self.args(func.args), body=self.body(func.body))

    @layers.register(astlib.Struct)
    def struct(self, struct):
        yield cgen.Struct(
            name=str(struct.name), body=self.body(struct.body))

    @layers.register(astlib.CFuncCall)
    def cfunc_call(self, call):
        if call.name == cdefs.SIZEOF_FUNC_NAME:
            yield cgen.SizeOf(*self.call_args(call.args))
        elif call.name in cdefs.CFUNCS:
            yield getattr(cdefs, str(call.name).upper() + "_FUNC_DESCR")(
                *self.call_args(call.args))

    @layers.register(astlib.FuncCall)
    def func_call(self, call):
        yield cgen.FuncCall(str(call.name), *self.call_args(call.args))

    @layers.register(astlib.Return)
    def return_(self, return_):
        yield cgen.Return(self.expr(return_.expr))

    @layers.register(astlib.Field)
    def field(self, field):
        type_ = self.type_(field.type_)
        if isinstance(field.type_, astlib.Name):
            if defs.VAR_NAME_REGEX.fullmatch(
                str(field.type_.replace(
                    "_".join([defs.ADR_PREFIX, context.file_hash, ""]), ""))):
                type_ = cgen.CTypes.ptr(cgen.CTypes.void)
        yield cgen.Decl(str(field.name), type_=type_)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield cgen.Assignment(
            name=self.expr(assignment.var),
            expr=self.expr(assignment.expr))