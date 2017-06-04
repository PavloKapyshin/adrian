from . import ast
from . import defs
from . import layers
from . import errors

from vendor.paka import funcreg


class NamingRules(layers.Layer):

    def _matches_variable_name(self, text):
        """Checks text for matching variable naming rules."""
        return defs.VARIABLE_REGEX.fullmatch(text)

    def _matches_type_name(self, text):
        """Checks text for matching type naming rules."""
        return defs.TYPE_REGEX.fullmatch(text)

    def _matches_module_name(self, text):
        """Checks text for matching module naming rules."""
        return defs.MODULE_REGEX.fullmatch(text)

    def _matches_special_struct_elem(self, text):
        """Checks text for matching special elem rules."""
        return defs.SPECIAL_STRUCT_ELEM_REGEX.fullmatch(text)

    def _check_decl_name(self, name):
        """Checks name according to all naming rules."""
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _check_module_name(self, module_name):
        """Checks text according to all naming rules."""
        if not self._matches_module_name(module_name.value):
            errors.bad_name_for_module(
                self.context.line, self.context.exit_on_error,
                module_name.value)
        return module_name

    def _check_struct_elem_name(self, name):
        # TODO: think how to refactor this.
        # Maybe self._check_decl_type()?
        if (not (self._matches_special_struct_elem(name.value) or\
                self._matches_variable_name(name.value) or\
                self._matches_type_name(name.value))):
            errors.bad_name_in_expr(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _check_struct_elem(self, struct_elem):
        if isinstance(struct_elem, ast.Name):
            return self._check_struct_elem_name(struct_elem)
        return self._check_decl_type(struct_elem)

    def _check_decl_type(self, type_):
        """Checks type_ according to all naming rules."""
        return self._check_decl_type.reg[type_](self, type_)

    _check_decl_type.reg = funcreg.TypeRegistry()

    @_check_decl_type.reg.register(ast.Name)
    def _check_decl_type_name(self, name):
        """Checks name according to all naming rules."""
        if not self._matches_type_name(name.value):
            errors.bad_name_for_type(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    @_check_decl_type.reg.register(ast.ModuleMember)
    def _check_decl_type_module_member(self, module):
        """Checks module according to all naming rules."""
        return ast.ModuleMember(
            name=self._check_module_name(module.name),
            member=self._check_decl_type(module.member))

    @_check_decl_type.reg.register(ast.StructElem)
    def _check_decl_type_struct_elem(self, struct):
        """Checks struct according to all naming rules."""
        return ast.StructElem(
            name=self._check_decl_type(struct.name),
            elem=self._check_struct_elem(struct.elem))

    def _check_expr(self, expr):
        """Checks expr according to all naming rules."""
        return self._check_expr.reg[expr](self, expr)

    _check_expr.reg = funcreg.TypeRegistry()

    @_check_expr.reg.register(ast.Name)
    def _check_expr_name(self, name):
        """Checks name according to all naming rules.

        TODO: name can be a constant.
        Needed to check node type in context.
        """
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    @_check_expr.reg.register(ast.MethodCall)
    def _check_expr_method_call(self, method):
        """Checks method according to all naming rules.

        method.method must named according to the type naming rules.
        """
        return ast.MethodCall(
            method=self._check_decl_type(method.method),
            args=self._check_call_args(method.args))

    @_check_expr.reg.register(ast.StructElem)
    def _check_expr_struct_elem(self, struct):
        return ast.StructElem(
            name=self._check_struct_name(struct.name),
            elem=self._check_struct_elem(struct.elem))

    @_check_expr.reg.register(ast.Integer)
    @_check_expr.reg.register(ast.String)
    def _check_expr_atom(self, atom):
        # Nothing to do here.
        return atom

    @_check_expr.reg.register(list)
    def _check_expr_list(self, lst):
        return [
            lst[0],
            self._check_expr(lst[1]),
            self._check_expr(lst[2])
        ]

    def _check_struct_name(self, name):
        if isinstance(name, ast.Name):
            if (not (self._matches_variable_name(name.value) or\
                    self._matches_type_name(name.value))):
                errors.bad_name_in_expr(
                    self.context.line, self.context.exit_on_error,
                    name.value)
            return name
        return self._check_decl_type(name)

    def _check_call_args(self, args):
        """Checks args according to the naming rules."""
        return [self._check_expr(arg) for arg in args]

    def decl(self, stmt):
        name = self._check_decl_name(stmt.name)
        type_ = stmt.type_
        if type_:
            type_ = self._check_decl_type(type_)
        expr = stmt.expr
        if expr:
            expr = self._check_expr(expr)
        return ast.Decl(name, type_, expr)