from . import ast, defs, layers, errors

from vendor.paka import funcreg


class OOP(layers.Layer):

    def _method(self, struct, method):
        if isinstance(struct, ast.VariableName):
            type_ = self.namespace.get(struct)["type"]
        elif isinstance(struct, ast.FuncCall):
            type_ = struct.name
        if isinstance(type_, ast.ModuleMember):
            return ast.ModuleMember(
                name=type_.name,
                member=method + type_.member)
        return ast.FunctionName(method + type_)

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def _expr(self, expr):
        return self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.VariableName)
    def _expr_name(self, name):
        return name.copy()

    @_expr.registry.register(ast.FuncCall)
    def _expr_func_call(self, call):
        if isinstance(call.name, ast.ModuleMember):
            if isinstance(call.name.member, ast.TypeName):
                return ast.Instance(call.name, call.args)
        elif isinstance(call.name, ast.TypeName):
            return ast.Instance(call.name, call.args)
        return call.copy()

    @_expr.registry.register(ast.MethodCall)
    def _expr_method_call(self, call):
        return ast.FuncCall(
            name=self._method(call.struct, call.method),
            args=self._call_args([call.struct] + call.args))

    @_expr.registry.register(ast.CIntFast8)
    @_expr.registry.register(ast.CIntFast32)
    @_expr.registry.register(ast.CUIntFast8)
    @_expr.registry.register(ast.CUIntFast32)
    @_expr.registry.register(ast.CString)
    @_expr.registry.register(ast.CChar)
    def _expr_catom(self, catom):
        return catom

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        return ast.SExpr(
            sexpr.op,
            self._expr(sexpr.expr1),
            self._expr(sexpr.expr2))

    def decl(self, stmt):
        expr = self._expr(stmt.expr)
        self.namespace.add(stmt.name, {
            "type": stmt.type_
        })
        return ast.Decl(stmt.name, stmt.type_, expr)

    def funccall(self, stmt):
        return ast.FuncCall(
            stmt.name, [self._expr(arg) for arg in stmt.args])
