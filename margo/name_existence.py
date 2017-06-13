from . import ast, defs, cdefs, errors, layers

from vendor.paka import funcreg


class NameExistence(layers.CheckingLayer):

    def _std_type(self, type_):
        if not type_ in defs.STD_TYPES_NAMES:
            errors.non_existing_type(
                self.position, self.exit_on_error, type_)

    def _c_type(self, type_):
        if not type_ in ast.CTYPES_NAMES:
            errors.non_existing_type(
                self.position, self.exit_on_error, type_)

    def _decl_type(self, type_):
        self._decl_type.registry[type_](self, type_)

    _decl_type.registry = funcreg.TypeRegistry()

    @_decl_type.registry.register(ast.TypeName)
    def _decl_type_name(self, name):
        if not self.typespace.exists(name):
            errors.non_existing_type(
                self.position, self.exit_on_error, name)

    @_decl_type.registry.register(ast.ModuleMember)
    def _decl_type_module(self, module):
        # Checking existence of module.member.
        if module.name == defs.STD_TYPES_MODULE_NAME:
            self._std_type(module.member)
        elif module.name == cdefs.CMODULE_NAME:
            self._c_type(module.member)
        # TODO: support user modules.
        else:
            errors.not_implemented(self.position, self.exit_on_error)

    @_decl_type.registry.register(ast.StructElem)
    def _decl_type_struct_elem(self, struct):
        errors.not_implemented(self.position, self.exit_on_error)

    def _expr(self, expr):
        if not isinstance(expr, (
                ast.Integer, ast.String,
                ast.CIntFast8, ast.CUIntFast8,
                ast.CIntFast32, ast.CUIntFast32,
                ast.CChar, ast.CString)):
            self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.VariableName)
    def _expr_name(self, name):
        if not self.namespace.exists(name):
            errors.non_existing_name(
                self.position, self.exit_on_error, name)

    @_expr.registry.register(ast.FuncCall)
    def _expr_funccall(self, call):
        self._func_name(call.name)
        self._call_args(call.args)

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        self._expr(sexpr.expr1)
        self._expr(sexpr.expr2)

    @_expr.registry.register(ast.Instance)
    def _expr_instance(self, instance):
        self._decl_type(instance.struct)
        self._call_args(instance.args)

    def _func_name(self, name):
        if isinstance(name, ast.FunctionName):
            if name in defs.STD_TYPES_FUNC_SIGNATURES:
                return
        elif name.member in defs.STD_TYPES_FUNC_SIGNATURES:
            return
        errors.not_implemented(self.position, self.exit_on_error)

    def _call_args(self, args):
        for arg in args:
            self._expr(arg)

    def _decl_name(self, name):
        # Function and variable names are named using the same regex.
        if name in defs.STD_FUNCS:
            errors.cant_reassign_builtin(
                self.position, self.exit_on_error, name)

    def decl(self, stmt):
        self._decl_name(stmt.name)
        self._decl_type(stmt.type_)
        self._expr(stmt.expr)
        # Register here to avoid linking in expression to itself.
        self.namespace.add(stmt.name, {
            "node_type": defs.NodeType.variable
        })