from . import ast
from . import defs
from . import layers
from . import errors

from vendor.paka import funcreg


class OOP(layers.Layer):

    def _method(self, method):
        return self._method.reg[method](self, method)

    _method.reg = funcreg.TypeRegistry()

    @_method.reg.register(ast.Name)
    def _method_name(self, name):
        return name.copy()

    @_method.reg.register(ast.ModuleMember)
    def _method_module_member(self, module):
        return ast.ModuleMember(
            name=module.name,
            member=self._method(module.member))

    @_method.reg.register(ast.StructElem)
    def _method_struct_elem(self, struct):
        sct = self._method(struct.name)
        elem = self._method(struct.elem)
        if isinstance(sct, ast.ModuleMember):
            sct.member.value = elem.value + sct.member.value
        else:
            sct.value = elem.value + sct.value
        return sct

    @_method.reg.register(ast.MethodCall)
    def _method_call(self, call):
        return self._expr_method_call(call)

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.MethodCall)
    def _expr_method_call(self, call):
        return ast.FuncCall(
            name=self._method(call.method),
            args=self._call_args(call.args))

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        return ast.Name(name.value)

    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    @_expr.reg.register(ast.CChar)
    @_expr.reg.register(ast.CString)
    def _expr_catom(self, catom):
        return catom

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        return [
            lst[0],
            self._expr(lst[1]),
            self._expr(lst[2])
        ]

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = self._expr(stmt.expr)
        return ast.Decl(name, type_, expr)
