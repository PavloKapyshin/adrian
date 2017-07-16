"""Analyzes names and translates them into more specific."""

from . import layers, astlib, errors
from .context import context


class Analyzer(layers.Layer):

    def _type(self, type_):
        # Only c module is supported, for now.
        if isinstance(type_, astlib.ModuleMember):
            return layers.create_with(
                type_, name=astlib.ModuleName(str(type_.name)),
                member=astlib.TypeName(str(type_.member)))
        elif isinstance(type_, astlib.Empty):
            return type_
        errors.not_implemented()

    def _expr(self, expr):
        # Only c module is supported, for now.
        if (isinstance(expr, astlib.FuncCall) and \
                (isinstance(expr.name, astlib.ModuleMember))):
            module = expr.name
            # args = (
            #     [] if isinstance(expr, astlib.Empty)
            #     else expr.args.as_list())
            args = expr.args
            return getattr(astlib, "C" + str(module.member))(
                args.arg.literal)
        elif isinstance(expr, astlib.Name):
            return astlib.VariableName(str(expr))
        elif isinstance(expr, astlib.SExpr):
            return layers.create_with(
                expr, expr1=self._expr(expr.expr1),
                expr2=self._expr(expr.expr2))
        return expr

    @layers.preregister(astlib.Decl)
    def _decl(self, decl):
        # Maybe ns.add(decl.name)?
        yield layers.create_with(
            decl, name=astlib.VariableName(str(decl.name)),
            type_=self._type(decl.type_), expr=self._expr(decl.expr))

    def _func_decl_args(self, args):
        arg_list = []
        for arg in ([] if isinstance(args, astlib.Empty) else args.as_list()):
            arg_list.append(
                (astlib.VariableName(str(arg[0])), self._type(arg[1])))
        return arg_list

    def _func_decl_body(self, body):
        # TODO: iterate through stmt_list and collect results
        stmt_list = ([] if isinstance(body, astlib.Empty) else body.as_list())
        return stmt_list

    @layers.preregister(astlib.FuncDecl)
    def _func_decl(self, func_decl):
        yield layers.create_with(
            func_decl, name=astlib.FunctionName(str(func_decl.name)),
            args=self._func_decl_args(func_decl.args),
            type_=self._type(func_decl.type_),
            body=self._func_decl_body(func_decl.body))
