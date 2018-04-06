from adrian import cgen
from . import astlib, defs, errors, layers, utils, env_api
from .context import context
from .utils import A


class ToCgen(layers.Layer):

    def __init__(self):
        self.b = layers.b(ToCgen)

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
    def f(self, fields, params):
        def check(type_):
            if type_ in A(astlib.Name):
                return type_ in params
            return any([check(t) for t in params])

        def gt(type_):
            if type_ in A(astlib.LiteralType):
                return cgen.CTypes.uint_fast64
            if check(type_):
                return cgen.CTypes.ptr(cgen.CTypes.void)
            return self.t(type_)

        fs = []
        for field in fields:
            fs.append(cgen.Decl(field.name, type_=gt(field.type_), expr=None))
        return fs

    def t(self, type_):
        if type_ in A(astlib.Void):
            return cgen.CTypes.void
        if type_ in A(astlib.DataMember):
            if type_.datatype == astlib.DataT.module:
                if type_.parent == defs.CMODULE:
                    return cgen.CTypes.ptr(
                        cgen.StructType(self.n(type_.member)))
        if type_ in A(astlib.Name):
            info = env_api.type_info(type_)
            if env_api.is_adt(info["node_type"]):
                return cgen.CTypes.ptr(cgen.UnionType(self.n(type_)))
            return cgen.CTypes.ptr(cgen.StructType(self.n(type_)))
        if type_ in A(astlib.GenericType):
            return self.t(type_.base)
        if type_ in A(astlib.LiteralType):
            if type_.type_ == astlib.LiteralT.uint_fast64_t:
                return cgen.CTypes.uint_fast64

    def bool_e(self, expr):
        if expr in A(astlib.CExpr):
            return cgen.Expr(
                cgen.COps.eq, self.e(expr.left), self.e(expr.right))
        return cgen.StructElem(
            cgen.CTypes.ptr(self.e(expr)),
            cgen.Var("literal"))

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
            if utils.is_adt(expr.type_):
                return cgen.UnionType(translated)
            return cgen.StructType(translated)
        if expr in A(astlib.DataMember):
            return cgen.StructElem(
                cgen.CTypes.ptr(self.e(expr.parent)),
                self.e(expr.member))
        if expr in A(astlib.Literal):
            if expr.type_ == astlib.LiteralT.integer:
                return cgen.Val(
                    literal=expr.literal,
                    type_=cgen.CTypes.int_fast64)
            return cgen.Val(
                literal=expr.literal,
                type_=cgen.CTypes.uint_fast64)
        if expr in A(astlib.Cast):
            type_ = self.t(expr.to)
            if type_ is None:
                return self.e(expr.expr)
            return cgen.Cast(self.e(expr.expr), type_)
        if expr in A(astlib.Empty):
            return None
        if expr in A(astlib.CExpr):
            return cgen.Expr(
                cgen.COps.neq, self.e(expr.left), self.e(expr.right))
        if expr in A(astlib.Null):
            return cgen.Null
        return cgen.Val(literal=expr, type_=cgen.CTypes.uint_fast64)

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
        if name in A(astlib.GenericType):
            return self.n(name.type_)
        return str(name)

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_scope()
        yield cgen.While(self.bool_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()

    def _if_stmt(self, stmt):
        context.env.add_scope()
        result = astlib.If(self.bool_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result.expr, result.body

    def _elif_stmt(self, stmt):
        context.env.add_scope()
        result = cgen.Elif(self.bool_e(stmt.expr), self.b(stmt.body))
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = cgen.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_stmt_expr, if_stmt_body = self._if_stmt(stmt.if_)
        elifs = []
        for elif_ in stmt.elifs_:
            elifs.append(self._elif_stmt(elif_))
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield cgen.If(
            if_stmt_expr, if_stmt_body, else_ifs=elifs, else_=else_)

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
        env_api.register(stmt)
        if stmt.decltype == astlib.DeclT.struct:
            fields, funcs = utils.split_body(stmt.body)
            if stmt.params:
                fields = self.f(fields, stmt.params)
                funcs = []
            else:
                fields = self.b(fields)
            yield cgen.Struct(self.n(stmt.name), fields)
            for func in funcs:
                yield from self.callable_decl(func)
        elif stmt.decltype == astlib.DeclT.adt:
            fields, other = utils.split_body(stmt.body)
            if stmt.params:
                # fields = self.f(fields, stmt.params)
                funcs = []
            # else:
            fields = self.b(fields)
            yield cgen.Union(self.n(stmt.name), fields)
            for func in funcs:
                yield from self.callable_decl(func)
        else:
            errors.later(errors.Version.v0m5.value)

    @layers.register(astlib.AST)
    def main(self, ast_, registry):
        ast_ = layers.transform_ast(ast_, registry=registry)
        yield from [
            (cgen.Include(info["header"])
             if info["type_"] == "adr" else
             cgen.CInclude(info["header"]))
            for _, info in context.clibs_includes.items()]
        main_func_body = []
        for node in ast_:
            if node in A(cgen.Decl, cgen.FuncCall, cgen.Assignment, cgen.If,
                    cgen.While):
                main_func_body.append(node)
            else:
                yield node
        main_func_body.append(
            cgen.Return(cgen.Val("0", type_=cgen.CTypes.int)))
        yield cgen.make_main0(*main_func_body)
