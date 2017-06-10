"""This layer analyzes and checks names."""

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
        if isinstance(expr, ast.Empty):
            return ast.Empty()
        return self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.Name)
    def _expr_name(self, name):
        # Only variable name is allowed, for now :D
        return self._var_name(name)

    @_expr.registry.register(ast.Integer)
    @_expr.registry.register(ast.String)
    def _expr_atom(self, atom):
        return atom

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        return ast.SExpr(
            sexpr.op, self._expr(sexpr.expr1),
            self._expr(sexpr.expr2))

    @_expr.registry.register(ast.FuncCall)
    def _expr_funccall(self, call):
        return ast.FuncCall(
            name=self._func_name(call.name),
            args=self._call_args(call.args))

    @_expr.registry.register(ast.MethodCall)
    @_expr.registry.register(ast.StructElem)
    def _expr_not_implemented(self, atom):
        errors.not_implemented(self.position, self.exit_on_error)

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def _type(self, type_):
        if isinstance(type_, ast.Empty):
            return ast.Empty()
        return self._type.registry[type_](self, type_)

    _type.registry = funcreg.TypeRegistry()

    @_type.registry.register(ast.ModuleMember)
    def _type_module_member(self, module):
        return ast.ModuleMember(
            name=self._module_name(module.name),
            member=self._type(module.member))

    @_type.registry.register(ast.StructElem)
    def _type_struct_eleme(self, struct):
        errors.not_implemented(self.position, self.exit_on_error)

    @_type.registry.register(ast.Name)
    def _type_name(self, name):
        if not _matches_type_name(name):
            errors.bad_name_for_type(
                self.position, self.exit_on_error, name)
        return ast.TypeName(str(name))

    def _var_name(self, name):
        if not _matches_var_name(name):
            errors.bad_name_for_variable(
                self.position, self.exit_on_error, name)
        return ast.VariableName(str(name))

    def _module_name(self, name):
        if not _matches_module_name(name):
            errors.bad_name_for_module(
                self.position, self.exit_on_error, name)
        return ast.ModuleName(str(name))

    def _func_name(self, name):
        if isinstance(name, ast.ModuleMember):
            module = name
            if module.name == cdefs.CMODULE_NAME:
                if not _matches_func_name(module.member):
                    if not _matches_type_name(module.member):
                        errors.bad_name_for_type(
                            self.position, self.exit_on_error,
                            module.member)
                    return ast.ModuleMember(
                        name=self._module_name(module.name),
                        member=self._type(module.member))
                return ast.ModuleMember(
                    name=self._module_name(module.name),
                    member=ast.FunctionName(str(module.member)))
        errors.not_implemented(self.position, self.exit_on_error)

    def decl(self, stmt):
        # We know that this is varaible name, so
        # we dont need information about it.
        name = ast.Name(str(self._var_name(stmt.name)))
        type_ = self._type(stmt.type_)
        expr = self._expr(stmt.expr)
        return ast.Decl(name, type_, expr)