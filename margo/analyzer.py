"""Analyzes names and translates them into more specific."""

from . import layers
from . import astlib
from . import errors


class Analyzer(layers.Layer):

    def _type(self, type_):
        # Only c module is supported, for now.
        if isinstance(type_, astlib.ModuleMember):
            return layers.create_with(
                type_, name=astlib.ModuleName(str(type_.name)),
                member=astlib.TypeName(str(type_.member)))
        elif isinstance(type_, astlib.Empty):
            return type_
        errors.not_implemented(context.position, context.exit_on_error)

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
