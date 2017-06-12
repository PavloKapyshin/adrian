from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


class NameExistence(layers.Layer):

    def _type_exists(self, text):
        return (self.context.typespace.exists(text))

    def _module_exists(self, text):
        # TODO: search for module in the fs.
        return (text in defs.STD_MODULE_NAMES)

    def _name_exists(self, text):
        return (self.context.namespace.exists(text))

    def _module_name(self, name):
        if not self._module_exists(name.value):
            errors.non_existing_module(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _std_type(self, type_):
        if type_.value in defs.STD_TYPES_NAMES:
            return type_
        errors.non_existing_type(
            self.context.line, self.context.exit_on_error,
            type_.value)

    def _c_type(self, type_):
        if type_.value in defs.C_TYPES_NAMES:
            return type_
        errors.non_existing_type(
            self.context.line, self.context.exit_on_error,
            type_.value)

    def _decl_type(self, type_):
        return self._decl_type.reg[type_](self, type_)

    _decl_type.reg = funcreg.TypeRegistry()

    @_decl_type.reg.register(ast.Name)
    def _decl_type_name(self, name):
        if not self._type_exists(name.value):
            errors.non_existing_type(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    @_decl_type.reg.register(ast.ModuleMember)
    def _decl_type_module(self, module):
        # TODO: check existence of type in module.
        # Maybe it fixed?
        if module.name.value == defs.STD_TYPES_MODULE_NAME:
            return ast.ModuleMember(
                name=module.name,
                member=self._std_type(module.member))
        elif module.name.value == defs.C_MODULE_NAME:
            return ast.ModuleMember(
                name=module.name,
                member=self._c_type(module.member))
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_decl_type.reg.register(ast.StructElem)
    def _decl_type_struct(self, struct):
        # TODO: only ast.Name in struct.elem for now. :(
        return ast.StructElem(
            name=self._decl_type(struct.name),
            elem=self._name(struct.elem))

    def _decl_name(self, name):
        if name.value in defs.STD_FUNCS:
            errors.cant_reassign_builtin(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _name(self, name):
        # TODO: support not only ast.Name
        if not self._name_exists(name.value):
            errors.non_existing_name(
                self.context.line, self.context.exit_on_error,
                name.value)
        return name

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        return self._name(name)

    @_expr.reg.register(ast.FuncCall)
    def _expr_funccall(self, call):
        if isinstance(call.name, ast.StructElem):
            # TODO: check for existence struct.elem.
            return ast.FuncCall(
                name=ast.StructElem(
                    name=self._decl_type(call.name.name),
                    elem=call.name.elem),   # Check here.
                args=self._call_args(call.args))
        return ast.FuncCall(
            name=self._func_name(call.name),
            args=self._call_args(call.args))

    @_expr.reg.register(ast.Integer)
    @_expr.reg.register(ast.String)
    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    @_expr.reg.register(ast.CChar)
    @_expr.reg.register(ast.CString)
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

    def _func_name(self, name):
        if isinstance(name, ast.Name):
            if (name.value in defs.STD_TYPES_FUNC_SIGNATURES or \
                    name.value in defs.C_FUNC_SIGNATURES):
                return name
        if (name.member.value in defs.STD_TYPES_FUNC_SIGNATURES or \
                name.member.value in defs.C_FUNC_SIGNATURES):
            return name

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def decl(self, stmt):
        name = self._decl_name(stmt.name)
        type_ = self._decl_type(stmt.type_)
        expr = self._expr(stmt.expr)
        # Register here to avoid linking in expression to itself.
        self.context.namespace.add_name(name.value, {
            "node_type": defs.NodeType.variable
        })
        return ast.Decl(name, type_, expr)
