from . import astlib, defs, inference, layers, utils
from .context import context
from .utils import A, scroll_to_parent


class TAC(layers.Layer):

    def __init__(self, tmp_count=0):
        self.tmp_count = tmp_count
        self.b = layers.b(TAC, tmp_count=self.tmp_count)

    def inc_tmp_count(self):
        self.tmp_count += 1
        self.b = layers.b(TAC, tmp_count=self.tmp_count)

    def new_tmp(self, expr):
        name = astlib.Name(
            "".join([defs.T_STRING, str(self.tmp_count)]),
            is_user_name=False)
        type_ = inference.infer_type(expr)
        result = astlib.Decl(astlib.DeclT.let, name, type_, expr)
        utils.register(result)
        self.inc_tmp_count()
        return name, [result]

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

    @layers.register(astlib.While)
    def while_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self._inner_e(stmt.expr)
        body = self.b(stmt.body)
        for decl in decls:
            body.append(astlib.Assignment(decl.name, "=", decl.expr))
        yield from decls
        yield astlib.While(expr, body)
        context.env.remove_scope()

    def _if_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self._inner_e(stmt.expr)
        result = (astlib.If(expr, self.b(stmt.body)), decls)
        context.env.remove_scope()
        return result

    def _elif_stmt(self, stmt):
        context.env.add_scope()
        expr, decls = self._inner_e(stmt.expr)
        result = (astlib.ElseIf(expr, self.b(stmt.body)), decls)
        context.env.remove_scope()
        return result

    def _else(self, stmt):
        context.env.add_scope()
        result = astlib.Else(self.b(stmt.body))
        context.env.remove_scope()
        return result

    @layers.register(astlib.Cond)
    def translate_cond(self, stmt: astlib.Cond):
        if_stmt, main_decls = self._if_stmt(stmt.if_stmt)
        elifs = []
        for elif_ in stmt.else_ifs:
            res = self._elif_stmt(elif_)
            elifs.append(res[0])
            main_decls.extend(res[1])
        if stmt.else_ is None:
            else_ = None
        else:
            else_ = self._else(stmt.else_)
        yield from main_decls
        yield astlib.Cond(if_stmt, elifs, else_)

    def replace_ass_a(self, what, with_, args):
        return [self.replace_ass_e(what, with_, a) for a in args]

    def replace_ass_e(self, what, with_, expr):
        if expr in A(astlib.Name):
            if expr == what:
                return with_
        if expr in A(astlib.DataMember):
            return astlib.DataMember(
                self.replace_ass_e(what, with_, expr.parent), expr.member)
        if expr in A(astlib.Callable):
            return astlib.Callable(
                expr.callabletype, expr.parent, expr.name,
                self.replace_ass_a(what, with_, expr.args))
        return expr

    def _a_tail_pointer(self, name, args):
        return any(self._tail_pointer(name, a) for a in args)

    def _tail_pointer(self, name, expr):
        if expr in A(astlib.Name):
            return expr == name
        if expr in A(astlib.DataMember):
            expr_p = scroll_to_parent(expr)
            return expr_p == name
        if expr in A(astlib.Callable):
            return self._a_tail_pointer(name, expr.args)
        return False

    def _ass_e(self, name, expr):
        if name in A(astlib.DataMember):
            name = scroll_to_parent(name)
        if self._tail_pointer(name, expr):
            tmp_name, decl = self.new_tmp(name)
            return self.replace_ass_e(name, tmp_name, expr), decl
        return expr, None

    @layers.register(astlib.Assignment)
    def assignment(self, stmt):
        expr_, decls_ = self._ass_e(stmt.left, stmt.right)
        if decls_:
            for decl in decls_:
                yield from self.decl(decl)
        expr, decls = self.e(expr_)
        utils.register(stmt, right=expr)
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
            utils.register(stmt)
            yield stmt
        else:
            expr, decls = self.e(stmt.expr)
            utils.register(stmt, expr=expr)
            yield from decls
            yield astlib.Decl(stmt.decltype, stmt.name, stmt.type_, expr)

    @layers.register(astlib.CallableDecl)
    def callable_decl(self, stmt):
        utils.register(stmt)
        +context.env
        utils.register_args(stmt.args)
        yield astlib.CallableDecl(
            stmt.decltype, stmt.parent, stmt.name,
            stmt.args, stmt.rettype, self.b(stmt.body))
        -context.env

    @layers.register(astlib.DataDecl)
    def data_decl(self, stmt):
        utils.register(stmt)
        +context.env
        context.parent = stmt.name
        utils.register_params(stmt.params)
        yield astlib.DataDecl(
            stmt.decltype, stmt.name, stmt.params, self.b(stmt.body))
        -context.env
