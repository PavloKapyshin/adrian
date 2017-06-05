from . import ast
from . import defs
from . import layers
from . import errors

from vendor.paka import funcreg


class StdAlias(layers.Layer):

    def _type_from_alias(self, type_):
        if (isinstance(type_, ast.Name) and \
                type_.value in defs.STD_TYPES_NAMES):
            return ast.ModuleMember(
                name=ast.Name(defs.STD_TYPES_MODULE_NAME),
                member=type_)
        return type_

    def _expr_from_alias(self, expr):
        return self._expr_from_alias.reg[expr](self, expr)

    _expr_from_alias.reg = funcreg.TypeRegistry()

    @_expr_from_alias.reg.register(ast.Name)
    def _expr_from_alias_name(self, name):
        type_ = self.context.namespace.get(name.value)["type_"]
        if (isinstance(type_, ast.ModuleMember) and \
                type_.name.value == defs.C_MODULE_NAME):
            return name.copy()
        return ast.MethodCall(
            method=ast.StructElem(
                name=type_,
                elem=ast.Name("__copy__")),
            args=[name])

    @_expr_from_alias.reg.register(ast.MethodCall)
    def _expr_from_alias_method_call(self, call):
        type_ = call.method
        if not isinstance(type_, ast.StructElem):
            return ast.MethodCall(
                method=ast.StructElem(
                    name=type_.copy(),
                    elem=ast.Name("__init__")),
                args=call.args)
        elif type_.elem.value == "__init__":
            return call
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_expr_from_alias.reg.register(ast.Integer)
    @_expr_from_alias.reg.register(ast.String)
    def _expr_from_alias_atom(self, atom):
        type_ = atom.to_type()
        args = self._std_type_init_args_from_alias([atom])
        return ast.MethodCall(
            method=ast.StructElem(
                name=ast.ModuleMember(
                    name=ast.Name(defs.STD_TYPES_MODULE_NAME),
                    member=type_),
                elem=ast.Name("__init__")),
            args=args)

    @_expr_from_alias.reg.register(ast.CIntFast8)
    @_expr_from_alias.reg.register(ast.CUIntFast8)
    @_expr_from_alias.reg.register(ast.CIntFast32)
    @_expr_from_alias.reg.register(ast.CUIntFast32)
    @_expr_from_alias.reg.register(ast.CChar)
    def _expr_from_alias_catom(self, catom):
        return catom

    @_expr_from_alias.reg.register(list)
    def _expr_from_alias_list(self, lst):
        if isinstance(lst[1], defs.C_TYPES):
            return [
                lst[0],
                self._expr_from_alias(lst[1]),
                self._expr_from_alias(lst[2])
            ]
        if isinstance(lst[1], ast.Name):
            type_ = self.context.namespace.get(lst[1].value)["type_"]
        else:
            type_ = ast.ModuleMember(
                name=ast.Name(defs.STD_TYPES_MODULE_NAME),
                member=lst[1].to_type())
        return ast.MethodCall(
            method=ast.StructElem(
                name=type_,
                elem=ast.Name(self._op_to_func(lst[0]))),
            args=[
                self._expr_from_alias(lst[1]),
                self._expr_from_alias(lst[2])
            ])

    def _op_to_func(self, op):
        if op in defs.OP_TO_FUNC:
            return defs.OP_TO_FUNC[op]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _std_type_init_args_from_alias(self, args):
        return [self._std_type_init_args_from_alias.reg[arg](
                    self, arg)
                for arg in args]

    _std_type_init_args_from_alias.reg = funcreg.TypeRegistry()

    @_std_type_init_args_from_alias.reg.register(ast.Integer)
    def _std_type_init_args_from_alias_integer(self, atom):
        return ast.CString(atom.value)

    def decl(self, stmt):
        name = stmt.name
        type_ = self._type_from_alias(stmt.type_)
        expr = self._expr_from_alias(stmt.expr)
        self.context.namespace.add_name(name.value, {
            "type_": type_
        })
        return ast.Decl(name, type_, expr)