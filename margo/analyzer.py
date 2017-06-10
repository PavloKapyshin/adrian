"""This layer search for CTypes expression (e. g. `c#IntFast8(0)`)
in the expression and translates it into AST nodes."""

from . import ast, cdefs, errors, layers

from vendor.paka import funcreg


class Analyzer(layers.Layer):
    _ctype_string_to_ast = {
        "IntFast8": ast.CIntFast8,
        "IntFast32": ast.CIntFast32,
        "UIntFast8": ast.CUIntFast8,
        "UIntFast32": ast.CUIntFast32,
        "Char": ast.CChar,
        "String": ast.CString
    }

    def _expr(self, expr):
        if isinstance(expr, ast.Empty):
            return ast.Empty()
        return self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.TypeName)
    @_expr.registry.register(ast.ModuleName)
    @_expr.registry.register(ast.FunctionName)
    def _expr_pass(self, atom):
        errors.not_implemented(self.position, self.exit_on_error)

    @_expr.registry.register(ast.VariableName)
    @_expr.registry.register(ast.Integer)
    @_expr.registry.register(ast.String)
    def _expr_atom(self, atom):
        return atom

    @_expr.registry.register(ast.FuncCall)
    def _expr_func_call(self, call):
        if isinstance(call.name, ast.ModuleMember):
            module = call.name
            if module.name == cdefs.CMODULE_NAME:
                member = module.member
                if isinstance(member, ast.TypeName):
                    # Only int types are supported, for now :D
                    return self._to_catom(member, call.args)
                elif isinstance(member, ast.FunctionName):
                    return ast.FuncCall(
                        name=module, args=call.args)
        errors.not_implemented(self.position, self.exit_on_error)

    @_expr.registry.register(ast.MethodCall)
    def _expr_method_call(self, call):
        errors.not_implemented(self.position, self.exit_on_error)

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        return ast.SExpr(
            sexpr.op, self._expr(sexpr.expr1),
            self._expr(sexpr.expr2))

    def _to_catom(self, type_, args):
        if not type_ in self._ctype_string_to_ast:
            errors.not_implemented(self.position, self.exit_on_error)
        catom_type = self._ctype_string_to_ast[type_]
        if len(args) == 1:
            return catom_type(args[0].literal)
        errors.wrong_number_of_args(
            self.position, self.exit_on_error,
            expected="1", got=str(len(args)))

    def decl(self, stmt):
        expr = self._expr(stmt.expr)
        return ast.Decl(stmt.name, stmt.type_, expr)

    def funccall(self, stmt):
        args = [self._expr(arg) for arg in stmt.args]
        return ast.FuncCall(stmt.name, args)
