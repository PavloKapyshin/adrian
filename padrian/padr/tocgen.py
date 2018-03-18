from . import astlib, layers, defs, errors, utils
from .utils import A

from adrian import cgen


class ToCgen(layers.Layer):

    def __init__(self):
        self.b = layers._b(ToCgen)

    # Misc.
    def struct_func_n(self, parent, name):
        name = self.n(name)
        if not name.startswith("_"):
            name = "".join(["_", name])
        return "".join([self.n(parent), name])

    def non_pointed_t(self, type_):
        if type_ in A(astlib.Name):
            return self.n(type_)

    def _e_callable(self, expr):
        if expr.callabletype == astlib.CallableT.cfunc:
            if expr.name == defs.SIZEOF:
                return cgen.SizeOf(*self.a(expr.args))
            elif expr.name in defs.CFUNCS:
                return getattr(
                    defs, str(expr.name).upper() + "_FUNC_DESCR")(
                        *self.a(expr.args))
        elif expr.callabletype == astlib.CallableT.fun:
            return cgen.FuncCall(
                self.n(expr.name), *self.a(expr.args))
        else:
            name = expr.name
            parent = expr.parent
            if expr.callabletype == astlib.CallableT.struct:
                parent = expr.name
                name = defs.INIT_METHOD
            return cgen.FuncCall(
                self.struct_func_n(parent, name),
                *self.a(expr.args))

    # Inner translation.
    def t(self, type_):
        if type_ in A(astlib.Void):
            return cgen.CTypes.void
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                return self.t(type_.member)
            errors.not_now(errors.LATER)
        if type_ in A(astlib.Name):
            return cgen.CTypes.ptr(cgen.StructType(self.n(type_)))
        if type_ in A(astlib.ParamedType):
            return self.t(type_.type_)

    def e(self, expr):
        if expr in A(astlib.Ref):
            return self.e(expr.expr)
        if expr in A(astlib.Name):
            return cgen.Var(self.n(expr))
        if expr in A(astlib.Callable):
            return self._e_callable(expr)
        if expr in A(astlib.StructScalar):
            translated = self.non_pointed_t(expr.type_)
            if expr.type_ in A(astlib.DataMember):
                if (expr.datatype == astlib.DataT.module and
                        expr.parent == defs.CMODULE):
                    return translated
                errors.not_now(errors.LATER)
            return cgen.StructType(translated)
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                return cgen.StructElem(
                    cgen.CTypes.ptr(self.e(expr.parent)),
                    self.e(expr.member))
        if expr in A(astlib.Literal):
            return cgen.Val(
                literal=expr.literal,
                type_=cgen.CTypes.int_fast64)

    def a(self, args):
        if args and isinstance(args[0], tuple):
            return [
                cgen.Decl(self.n(name), type_=self.t(type_))
                for name, type_ in args]
        return list(map(self.e, args))

    def n(self, name):
        if name in A(astlib.DataMember):
            if name.datatype == astlib.DataT.module:
                return self.n(name.member)
        return str(name)

    # Core.
    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        yield cgen.Assignment(
            name=self.e(stmt.left),
            expr=self.e(stmt.right))

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        yield cgen.Return(self.e(stmt.expr))

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        yield self.e(stmt)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        expr = None
        if stmt.decltype != astlib.DeclT.field:
            expr = self.e(stmt.expr)
        yield cgen.Decl(
            self.n(stmt.name), type_=self.t(stmt.type_), expr=expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.fun:
            yield cgen.Func(
                self.n(stmt.name), self.t(stmt.rettype),
                self.a(stmt.args), self.b(stmt.body))
        else:
            yield cgen.Func(
                self.struct_func_n(stmt.parent, stmt.name),
                self.t(stmt.rettype), self.a(stmt.args),
                self.b(stmt.body))

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.struct:
            fields, funcs = utils.split_body(stmt.body)
            yield cgen.Struct(self.n(stmt.name), self.b(fields))
            for func in funcs:
                yield from self.callable_decl(func)
        else:
            errors.not_now(errors.LATER)

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        ast_ = layers.transform_ast(ast_, registry=registry)
        main_func_body = []
        for node in ast_:
            if node in A(cgen.Decl, cgen.FuncCall, cgen.Assignment):
                main_func_body.append(node)
            else:
                yield node
        main_func_body.append(
            cgen.Return(cgen.Val("0", type_=cgen.CTypes.int)))
        yield cgen.make_main0(*main_func_body)
