from . import ast, defs, cdefs, errors, layers

from vendor.paka import funcreg


def _matches_maker(regex):
    return lambda name: regex.fullmatch(str(name))


_matches_var_name = _matches_maker(defs.VARIABLE_REGEX)
_matches_func_name = _matches_maker(defs.FUNCTION_REGEX)
_matches_type_name = _matches_maker(defs.TYPE_REGEX)
_matches_module_name = _matches_maker(defs.MODULE_REGEX)
_matches_spec_struct_elem = _matches_maker(
    defs.SPEC_STRUCT_ELEM_REGEX)


class NamingRules(layers.Layer):

    def _expr(self, expr):
        if expr:
            self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        self._var_name(name)

    @_expr.reg.register(ast.Integer)
    @_expr.reg.register(ast.String)
    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    @_expr.reg.register(ast.CChar)
    @_expr.reg.register(ast.CString)
    def _expr_pass(self, atom):
        pass

    @_expr.reg.register(ast.FuncCall)
    def _expr_func_call(self, call):
        self._call_args(call.args)
        self._func_name(call.name)

    @_expr.reg.register(ast.MethodCall)
    @_expr.reg.register(ast.StructElem)
    def _expr_not_implemented(self, atom):
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _func_name(self, name):
        if isinstance(name, ast.ModuleMember):
            module = name
            self._module_name(module.name)
            if module.name == cdefs.CMODULE_NAME:
                if not _matches_func_name(module.member):
                    errors.bad_name_for_function(
                        self.context.line,
                        self.context.exit_on_error,
                        module.member)
        else:
            errors.not_implemented(
                self.context.line, self.context.exit_on_error)

    def _call_args(self, args):
        for arg in args:
            self._expr(arg)

    def _type(self, type_):
        if type_:
            self._type.reg[type_](self, type_)

    _type.reg = funcreg.TypeRegistry()

    @_type.reg.register(ast.ModuleMember)
    def _type_module_member(self, module):
        self._module_name(module.name)
        self._type(module.member)

    @_type.reg.register(ast.StructElem)
    def _type_struct_elem(self, struct):
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_type.reg.register(ast.Name)
    def _type_name(self, name):
        if not _matches_type_name(name):
            errors.bad_name_for_type(
                self.context.line, self.context.exit_on_error,
                name)

    def _var_name(self, name):
        if not _matches_var_name(name):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error,
                name)

    def _module_name(self, name):
        if not _matches_module_name(name):
            errors.bad_name_for_module(
                self.context.line, self.context.exit_on_error,
                name)

    def decl(self, stmt):
        self._var_name(stmt.name)
        self._type(stmt.type_)
        self._expr(stmt.expr)
        # TODO: add name to context ({"node_type": Variable}).
        return ast.Decl(stmt.name, stmt.type_, stmt.expr)

    def funccall(self, stmt):
        self._func_name(stmt.name)
        self._call_args(stmt.args)
        return ast.FuncCall(stmt.name, stmt.args)