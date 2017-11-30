import itertools

from . import astlib, layers, inference, defs, errors
from .context import context, add_to_env, add_scope, del_scope
from .patterns import A


def new_tmp(expr):
    tmp_name = astlib.Name(
        "".join([defs.T_STRING, str(context.tmp_count)]),
        is_tmp=True)
    type_ = inference.infer(expr)
    context.tmp_count += 1
    node = astlib.VarDecl(tmp_name, type_, expr)
    add_to_env(node)
    return tmp_name, [node]


def inner_expr(expr):
    if expr in A(astlib.Expr):
        expr_, decls_ = e(expr)
        tmp, decls = new_tmp(expr_)
        return tmp, decls_ + decls

    if expr in A(astlib.FuncCall, astlib.StructCall):
        new_args, decls_ = call_args(expr.args)
        tmp, decls = new_tmp(type(expr)(expr.name, new_args))
        return tmp, decls_ + decls

    if expr in A(astlib.StructFuncCall):
        new_args, decls_ = call_args(expr.args)
        tmp, decls = new_tmp(
            astlib.StructFuncCall(
                expr.struct, expr.func_name, new_args))
        return tmp, decls_ + decls

    if expr in A(astlib.StructMember):
        tmp, decls = inner_expr(expr.struct)
        main_tmp, main_decls = new_tmp(astlib.StructMember(tmp, expr.member))
        return main_tmp, decls + main_decls

    if expr in A(astlib.Name, astlib.Ref):
        return expr, []

    return new_tmp(expr)


def e(expr):
    if expr in A(astlib.Expr):
        left_tmp, left_decls = inner_expr(expr.left_expr)
        right_tmp, right_decls = inner_expr(expr.right_expr)
        tmp_decls = left_decls + right_decls
        return astlib.Expr(
            expr.op, left_tmp, right_tmp), tmp_decls

    if expr in A(astlib.FuncCall, astlib.StructCall):
        new_args, decls = call_args(expr.args)
        return type(expr)(expr.name, new_args), decls

    if expr in A(astlib.StructFuncCall):
        new_args, decls = call_args(expr.args)
        return astlib.StructFuncCall(
            expr.struct, expr.func_name, new_args), decls

    if expr in A(astlib.StructMember):
        return inner_expr(expr)

    return expr, []


def call_args(args):
    new_args, decls = [], []
    for arg in args:
        arg_, decls_ = inner_expr(arg)
        decls.extend(decls_)
        new_args.append(arg_)
    return new_args, decls


class TAC(layers.Layer):

    def body(self, body):
        reg = TAC().get_registry()
        return list(itertools.chain.from_iterable(
            map(lambda stmt: list(
                    layers.transform_node(stmt, registry=reg)),
                body)))


    @layers.register(astlib.VarDecl)
    def var_decl(self, declaration):
        new_expr, tmp_decls = e(declaration.expr)
        add_to_env(declaration)
        yield from tmp_decls
        yield astlib.VarDecl(
            declaration.name, declaration.type_, new_expr)

    @layers.register(astlib.LetDecl)
    def let_decl(self, declaration):
        new_expr, tmp_decls = e(declaration.expr)
        add_to_env(declaration)
        yield from tmp_decls
        yield astlib.LetDecl(
            declaration.name, declaration.type_, new_expr)

    @layers.register(astlib.AssignmentAndAlloc)
    def assignment_and_alloc(self, stmt):
        new_expr, tmp_decls = e(stmt.expr)
        yield from tmp_decls
        yield astlib.AssignmentAndAlloc(
            stmt.name, stmt.type_, new_expr)

    @layers.register(astlib.Assignment)
    def assignment(self, assignment):
        new_expr, tmp_decls = e(assignment.expr)
        yield from tmp_decls
        yield astlib.Assignment(
            assignment.variable, assignment.op, new_expr)

    @layers.register(astlib.Return)
    def return_(self, return_):
        new_expr, tmp_decls = inner_expr(return_.expr)
        yield from tmp_decls
        yield astlib.Return(new_expr)

    @layers.register(astlib.FuncDecl)
    def func_decl(self, declaration):
        add_to_env(declaration)
        add_scope()
        yield astlib.FuncDecl(
            declaration.name, declaration.args,
            declaration.rettype, self.body(declaration.body))
        del_scope()

    @layers.register(astlib.StructFuncDecl)
    def struct_func(self, declaration):
        add_to_env(declaration)
        add_scope()
        yield astlib.StructFuncDecl(
            declaration.struct, declaration.func,
            declaration.args, declaration.rettype,
            self.body(declaration.body))
        del_scope()

    @layers.register(astlib.StructDecl)
    def struct_decl(self, declaration):
        add_to_env(declaration)
        add_scope()
        yield astlib.StructDecl(
            declaration.name, declaration.var_types,
            self.body(declaration.body))
        del_scope()