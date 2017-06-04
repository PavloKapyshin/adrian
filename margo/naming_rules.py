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

    def _decl_name(self, name):
        """Checks name according to all naming rules."""
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _module_name(self, module_name):
        """Checks text according to all naming rules."""
        if not self._matches_module_name(module_name.value):
            errors.bad_name_for_module(
                self.context.line, self.context.exit_on_error,
                module_name.value)
        return module_name

    def _struct_elem_name(self, name):
        # TODO: think how to refactor this.
        # Maybe self._decl_type()?
        if (not (self._matches_special_struct_elem(name.value) or \
                self._matches_variable_name(name.value) or \
                self._matches_type_name(name.value))):
            errors.bad_name_in_expr(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _struct_elem(self, struct_elem):
        if isinstance(struct_elem, ast.Name):
            return self._struct_elem_name(struct_elem)
        return self._decl_type(struct_elem)

    def _decl_type(self, type_):
        """Checks type_ according to all naming rules."""
        return self._decl_type.reg[type_](self, type_)

    _decl_type.reg = funcreg.TypeRegistry()

    @_decl_type.reg.register(ast.Name)
    def _decl_type_name(self, name):
        """Checks name according to all naming rules."""
        if not self._matches_type_name(name.value):
            errors.bad_name_for_type(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    @_decl_type.reg.register(ast.ModuleMember)
    def _decl_type_module_member(self, module):
        """Checks module according to all naming rules."""
        return ast.ModuleMember(
            name=self._module_name(module.name),
            member=self._decl_type(module.member))

    @_decl_type.reg.register(ast.StructElem)
    def _decl_type_struct_elem(self, struct):
        """Checks struct according to all naming rules."""
        return ast.StructElem(
            name=self._decl_type(struct.name),
            elem=self._struct_elem(struct.elem))

    def _expr(self, expr):
        """Checks expr according to all naming rules."""
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        """Checks name according to all naming rules.

        TODO: name can be a constant.
        Needed to node type in context.
        """
        if not self._matches_variable_name(name.value):
            errors.bad_name_for_variable(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    @_expr.reg.register(ast.MethodCall)
    def _expr_method_call(self, method):
        """Checks method according to all naming rules.

        method.method must named according to the type naming rules.
        """
        return ast.MethodCall(
            method=self._decl_type(method.method),
            args=self._call_args(method.args))

    @_expr.reg.register(ast.StructElem)
    def _expr_struct_elem(self, struct):
        return ast.StructElem(
            name=self._struct_name(struct.name),
            elem=self._struct_elem(struct.elem))

    @_expr.reg.register(ast.Integer)
    @_expr.reg.register(ast.String)
    def _expr_atom(self, atom):
        # Nothing to do here.
        return atom

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        return [
            lst[0],
            self._expr(lst[1]),
            self._expr(lst[2])
        ]

    def _struct_name(self, name):
        if isinstance(name, ast.Name):
            if (not (self._matches_variable_name(name.value) or \
                    self._matches_type_name(name.value))):
                errors.bad_name_in_expr(
                    self.context.line, self.context.exit_on_error,
                    name.value)
            return name
        return self._decl_type(name)

    def _call_args(self, args):
        """Checks args according to the naming rules."""
        return [self._expr(arg) for arg in args]

    def decl(self, stmt):
        name = self._decl_name(stmt.name)
        type_ = stmt.type_
        if type_:
            type_ = self._decl_type(type_)
        expr = stmt.expr
        if expr:
            expr = self._expr(expr)
        return ast.Decl(name, type_, expr)