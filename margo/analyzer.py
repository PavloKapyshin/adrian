"""Analyzes names and translates them into more specific."""

from . import layers, astlib, errors, cdefs
from .context import context


class Analyzer(layers.Layer):

    def type_(self, type_):
        if isinstance(type_, astlib.ModuleMember):
            if type_.module_name == cdefs.CMODULE_NAME:
                return astlib.CType(str(type_.member))
        elif isinstance(type_, astlib.Empty):
            return astlib.Empty()
        errors.not_implemented("type is not supported")

    def expr(self, expr):
        if isinstance(expr, astlib.Name):
            #name_type = context.ns.get(str(expr))["name_type"]
            name_type = astlib.VariableName
            return name_type(str(expr))
        elif isinstance(expr, astlib.SExpr):
            return astlib.SExpr(
                op=expr.op, expr1=self.expr(expr.expr1),
                expr2=self.expr(expr.expr2))
        elif (isinstance(expr, astlib.FuncCall) and \
                isinstance(expr.name, astlib.ModuleMember) and \
                expr.name.module_name == cdefs.CMODULE_NAME):
            module = expr.name
            length = (
                len(expr.args) if isinstance(expr.args, astlib.CallArgs)
                else 0)
            if length != 1:
                errors.wrong_number_of_args(
                    context.exit_on_error, expected=1, got=length)
            arg = expr.args.arg
            return getattr(astlib, "C" + str(module.member))(arg.literal)
        errors.not_implemented("expr is not supported")

    @layers.register(astlib.Decl)
    def decl(self, decl):
        # Adding to namespace.
        #context.ns.add(str(decl.name), {
        #    "name_type": astlib.VariableName
        #})
        yield astlib.Decl(
            astlib.VariableName(str(decl.name)),
            type_=self.type_(decl.type_), expr=self.expr(decl.expr))