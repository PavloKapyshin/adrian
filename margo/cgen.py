"""Generates CGen AST."""

from . import cdefs, layers, astlib, errors
from adrian import cgen


TO_CTYPE = {
    "IntFast8": "int_fast8",
    "IntFast32": "int_fast32",
    "UIntFast8": "uint_fast8",
    "UIntFast32": "uint_fast32",
    "Void": "void",
}


class CGen(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.CType):
            return getattr(cgen.CTypes, TO_CTYPE[str(type_)])
        elif isinstance(type_, astlib.TypeName):
            return cgen.CTypes.ptr(cgen.StructType(str(type_)))
        errors.not_implemented("type is not supported")

    def expr(self, expr):
        if isinstance(expr, (
                astlib.CIntFast8, astlib.CIntFast32,
                astlib.CUIntFast8, astlib.CUIntFast32)):
            return cgen.Val(
                literal=expr.literal,
                type_=getattr(cgen.CTypes, TO_CTYPE[str(expr.to_type())]))
        elif isinstance(expr, astlib.StructScalar):
            return cgen.StructType(str(expr.name))
        elif isinstance(expr, astlib.StructElem):
            return cgen.StructElem(
                struct_name=cgen.CTypes.ptr(self.expr(expr.name)),
                elem_name=self.expr(expr.elem))
        elif isinstance(expr, astlib.VariableName):
            return cgen.Var(str(expr))
        elif isinstance(expr, astlib.CFuncCall):
            if expr.name == cdefs.SIZEOF_FUNC_NAME:
                return cgen.SizeOf(*self.call_args(expr.args))
            elif expr.name in cdefs.CFUNCS:
                return getattr(cdefs, str(expr.name).upper() + "_FUNC_DESCR")(
                    *self.call_args(expr.args))
        elif isinstance(expr, astlib.FuncCall):
            return list(self.func_call(expr))[0]
        errors.not_implemented("expr is not supported")

    def call_args(self, args):
        result = []
        for arg in ([] if isinstance(args, astlib.Empty) else args.as_list()):
            result.append(self.expr(arg))
        return result

    def args(self, args):
        result = []
        for arg in ([] if isinstance(args, astlib.Empty) else args.as_list()):
            result.append(cgen.Decl(str(arg[0]), type_=self.type_(arg[1])))
        return result

    def body(self, body):
        reg = CGen().get_registry()
        result = []
        for stmt in ([] if isinstance(body, astlib.Empty) else body.as_list()):
            result.append(list(layers.transform_node(stmt, registry=reg))[0])
        return result

    @layers.register(astlib.Decl)
    def decl(self, decl):
        yield cgen.Decl(
            name=str(decl.name), type_=self.type_(decl.type_),
            expr=self.expr(decl.expr))

    @layers.register(astlib.Func)
    def func(self, func):
        yield cgen.Func(
            name=str(func.name), rettype=self.type_(func.type_),
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
        yield cgen.Decl(str(field.name), type_=self.type_(field.type_))

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        yield cgen.Assignment(
            name=self.expr(assignment.name),
            expr=self.expr(assignment.expr))
