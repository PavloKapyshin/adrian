from . import astlib, layers, inference, errors
from .context import context
from .patterns import A


# Temporary variable prefix.
T_STRING = "t"


def new_tmp(expr_):
    tmp_name = astlib.Name(
        "".join([T_STRING, str(context.tmp_count)]), is_tmp=True)
    context.tmp_count += 1
    return tmp_name, [astlib.Decl(tmp_name, inference.infer(expr_), expr_)]


def inner_expr(e):
    if e in A(astlib.Expr):
        return new_tmp(expr(e))
    return new_tmp(e)


def expr(e):
    if e in A(astlib.Expr):
        lexpr_tmp, lexpr_decls = inner_expr(e.lexpr)
        rexpr_tmp, rexpr_decls = inner_expr(e.rexpr)
        tmp_decls = lexpr_decls + rexpr_decls
        return astlib.Expr(e.op, lexpr_tmp, rexpr_tmp), tmp_decls

    if e in A(astlib.CTYPES + (astlib.Name, )):
        return e, []

    errors.not_implemented("tac: expr (expr {})".format(e))


class TAC(layers.Layer):

    @layers.register(astlib.Decl)
    def decl(self, decl):
        context.env.add(str(decl.name), {
             "type": decl.type_
        })
        new_expr, tmp_decls = expr(decl.expr)
        yield from tmp_decls
        yield astlib.Decl(decl.name, decl.type_, new_expr)