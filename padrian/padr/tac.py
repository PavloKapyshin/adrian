from . import astlib, layers, defs, utils, inference
from .context import context
from .utils import A


class TAC(layers.Layer):

    def __init__(self, tmp_count=0):
        self.tmp_count = tmp_count
        self.b = layers._b(TAC, tmp_count=self.tmp_count)

    # Misc.
    def inc_tmp_count(self):
        self.tmp_count += 1
        self.b = layers._b(TAC, tmp_count=self.tmp_count)

    def new_tmp(self, expr):
        name = astlib.Name(
            "".join([defs.T_STRING, str(self.tmp_count)]),
            is_user_name=False)
        type_ = inference.infer_type(expr)
        utils.register_var_or_let(name, astlib.DeclT.let, type_)
        self.inc_tmp_count()
        return name, [astlib.Decl(astlib.DeclT.let, name, type_, expr)]

    # Inner ast nodes translation.
    def _inner_e(self, expr):
        if expr in A(astlib.Callable):
            if (expr.callabletype in (astlib.CallableT.struct_func,
                    astlib.CallableT.struct, astlib.CallableT.fun)):
                args, decls = self.a(expr.args)
                tmp, decls_ = self.new_tmp(
                    astlib.Callable(
                        expr.callabletype, expr.parent,
                        expr.name, args))
                return tmp, decls + decls_
            return expr, []
        if expr in A(astlib.DataMember):
            if expr.datatype == astlib.DataT.struct:
                parent, decls = self._inner_e(expr.parent)
                return astlib.DataMember(
                    astlib.DataT.struct, parent, expr.member), decls
        if expr in A(
                astlib.Name, astlib.Literal,
                astlib.Ref, astlib.StructScalar):
            return expr, []
        return self.new_tmp(expr)

    def e(self, expr):
        if expr in A(astlib.Callable):
            if (expr.callabletype in (astlib.CallableT.struct_func,
                    astlib.CallableT.struct, astlib.CallableT.fun)):
                args, decls = self.a(expr.args)
                return astlib.Callable(
                    expr.callabletype, expr.parent,
                    expr.name, args), decls
        if (expr in A(astlib.DataMember) and
                expr.datatype == astlib.DataT.struct):
            return self._inner_e(expr)
        return expr, []

    def a(self, args):
        if len(args) == 0:
            return [], []
        elif isinstance(args[0], tuple):
            return args, []
        new_args, decls = [], []
        for arg in args:
            new_arg, new_decls = self._inner_e(arg)
            new_args.append(new_arg)
            decls.extend(new_decls)
        return new_args, decls

    # Subcore funcs.
    def fun_decl(self, stmt):
        utils.register_func(stmt.name, stmt.rettype, stmt.args)
        +context.env
        utils.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    def struct_func_decl(self, stmt):
        utils.register_func_as_child(
            stmt.parent, stmt.name, stmt.rettype, stmt.args)
        +context.env
        utils.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    # Core funcs.
    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr, decls = self.e(stmt.right)
        yield from decls
        yield astlib.Assignment(stmt.left, stmt.op, expr)

    @layers.register(astlib.Return)
    def return_stmt(self, stmt):
        expr, decls = self.e(stmt.expr)
        yield from decls
        yield astlib.Return(expr)

    @layers.register(astlib.Callable)
    def callable_stmt(self, stmt):
        args, decls = self.a(stmt.args)
        yield from decls
        yield astlib.Callable(stmt.callabletype, stmt.parent, stmt.name, args)

    @layers.register(astlib.Decl)
    def decl(self, stmt):
        if stmt.decltype == astlib.DeclT.field:
            utils.register_field(stmt.name, stmt.type_)
            yield stmt
        else:
            expr, decls = self.e(stmt.expr)
            utils.register_var_or_let(stmt.name, stmt.decltype, stmt.type_)
            yield from decls
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        if stmt.decltype == astlib.DeclT.fun:
            yield from self.fun_decl(stmt)
        if stmt.decltype == astlib.DeclT.struct_func:
            yield from self.struct_func_decl(stmt)
        if stmt.decltype == astlib.DeclT.protocol_func:
            errors.not_now(errors.LATER)

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register_data_decl(stmt.name, stmt.decltype, stmt.params)
        +context.env
        context.parent = stmt.name
        utils.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
