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
        if expr is None:
            return None
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    @_expr.reg.register(ast.StructElem)
    @_expr.reg.register(ast.Integer)
    @_expr.reg.register(ast.String)
    def _expr_pass(self, atom):
        return atom

    @_expr.reg.register(list)
    def _sexpr(self, lst):
        return [
            lst[0],
            self._expr(lst[1]),
            self._expr(lst[2])
        ]

    @_expr.reg.register(ast.FuncCall)
    def _expr_func_call(self, call):
        if isinstance(call.name, ast.ModuleMember):
            module = call.name
            if module.name == cdefs.CMODULE_NAME:
                type_ = module.member
                if len(call.args) == 1:
                    return self._to_catom(type_, call.args[0])
                errors.wrong_number_of_args(
                    self.context.line,
                    self.context.exit_on_error,
                    expected="1", got=str(len(call.args)))
        # `if isinstance(call.name, ast.Name)` need to
        # implement when funcs will be added in Adrian.
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_expr.reg.register(ast.MethodCall)
    def _expr_method_call(self, call):
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _to_catom(self, type_, arg):
        if not type_ in self._ctype_string_to_ast:
            return arg
        catom_type = self._ctype_string_to_ast[type_]
        return catom_type(arg.literal)

    def decl(self, stmt):
        expr = self._expr(stmt.expr)
        return ast.Decl(stmt.name, stmt.type_, expr)

    def funccall(self, stmt):
        args = [self._expr(arg) for arg in stmt.args]
        return ast.FuncCall(stmt.name, args)
