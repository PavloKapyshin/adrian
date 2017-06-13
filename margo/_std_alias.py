from . import ast, defs, cdefs, layers, errors

from vendor.paka import funcreg


class StdAlias(layers.Layer):

    def _type(self, type_):
        if (isinstance(type_, ast.TypeName) and \
                type_ in defs.STD_TYPES_NAMES):
            return ast.ModuleMember(
                name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                member=type_)
        return type_

    def _expr(self, expr):
        return self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.VariableName)
    def _expr_name(self, name):
        type_ = self.namespace.get(name)["type"]
        if (isinstance(type_, ast.ModuleMember) and \
                type_.name == cdefs.CMODULE_NAME):
            return name.copy()
        return ast.MethodCall(
            struct=name,
            method=ast.MethodName("__copy__"),
            args=[])

    @_expr.registry.register(ast.MethodCall)
    def _expr_method_call(self, call):
        errors.not_implemented(self.position, self.exit_on_error)

    @_expr.registry.register(ast.Integer)
    @_expr.registry.register(ast.String)
    def _expr_std_atom(self, atom):
        type_ = atom.to_type()
        args = [ast.CString(atom.literal)]
        return ast.FuncCall(
            name=ast.ModuleMember(
                name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                member=type_),
            args=args)

    @_expr.registry.register(ast.CIntFast8)
    @_expr.registry.register(ast.CUIntFast8)
    @_expr.registry.register(ast.CIntFast32)
    @_expr.registry.register(ast.CUIntFast32)
    @_expr.registry.register(ast.CChar)
    def _expr_catom(self, catom):
        return catom

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        if isinstance(sexpr.expr1, ast.CTYPES):
            return ast.SExpr(
                sexpr.op,
                sexpr.expr1,
                sexpr.expr2)
        if isinstance(sexpr.expr1, ast.VariableName):
            type_ = self.namespace.get(sexpr.expr1)["type"]
        else:
            type_ = ast.ModuleMember(
                name=ast.ModuleName(defs.STD_TYPES_MODULE_NAME),
                member=sexpr.expr1.to_type())
        return ast.MethodCall(
            struct=self._expr(sexpr.expr1),
            method=ast.MethodName(self._op_to_func(sexpr.op)),
            args=[self._expr(sexpr.expr2)])

    def _op_to_func(self, op):
        if op in defs.OP_TO_FUNC:
            return defs.OP_TO_FUNC[op]
        errors.not_implemented(self.position, self.exit_on_error)

    def decl(self, stmt):
        type_ = self._type(stmt.type_)
        expr = self._expr(stmt.expr)
        self.namespace.add(stmt.name, {
            "type": type_
        })
        return ast.Decl(stmt.name, type_, expr)

    def funccall(self, stmt):
        args = [self._expr(arg) for arg in stmt.args]
        name = stmt.name
        if (isinstance(name, ast.ModuleMember) and \
                (name.name == cdefs.CMODULE_NAME) and \
                (name.member == "tpr")):
            name = ast.ModuleMember(
                name=name.name,
                member=ast.FunctionName("puts"))
            args = [ast.CString("Hello, world!")]
        return ast.FuncCall(name, args)
