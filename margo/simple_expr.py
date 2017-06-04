from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


class SimpleExpr(layers.Layer):
    tmp_count = 0
    tmp_string = "tmp"

    operator_to_func = {
        "+": "__add__",
        "-": "__sub__",
        "*": "__mul__",
        "/": "__div__",
    }

    def _get_info_from_call(self, name):
        if isinstance(name, ast.ModuleMember):
            if name.name.value == defs.C_MODULE_NAME:
                return defs.C_FUNC_SIGNATURES[name.member.value]
            elif name.name.value == defs.STD_TYPES_MODULE_NAME:
                return defs.STD_TYPES_FUNC_SIGNATURES[name.member.value]
        return self.context.funcspace.get(name.value)

    def _tmp(self, type_, expr):
        self.tmp_count += 1
        tmp_name = self.tmp_string + str(self.tmp_count)
        return ast.Decl(ast.Name(tmp_name), type_, expr)

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        return name, []

    @_expr.reg.register(ast.FuncCall)
    def _expr_funccall(self, call):
        info = self._get_info_from_call(call.name)
        type_ = info["rettype"]
        result = self.funccall(call)
        # maybe without that?
        tmp_decl_stmt = self._tmp(type_, result[-1])
        tmp_name = tmp_decl_stmt.name
        return tmp_name, result[:-1] + [tmp_decl_stmt]

    @_expr.reg.register(ast.CString)
    @_expr.reg.register(ast.CChar)
    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    def _expr_catom(self, catom):
        return catom, []

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        tmp_decls = []
        expr1_tmp, expr1_decls = self._expr(lst[1])
        expr2_tmp, expr2_decls = self._expr(lst[2])
        tmp_decls.extend(expr1_decls)
        tmp_decls.extend(expr2_decls)
        return [
            lst[0],
            expr1_tmp,
            expr2_tmp
        ], tmp_decls

    def _call_args(self, args):
        tmp_decls = []
        tmp_names = []
        for arg in args:
            tmp_name, lst = self._expr(arg)
            tmp_names.append(ast.Name(tmp_name))
            tmp_decls.extend(lst)
        return tmp_names, tmp_decls

    def funccall(self, stmt):
        # TODO: write code for builtins.
        tmp_names, tmp_decls = self._call_args(stmt.args)
        return tmp_decls + [ast.FuncCall(
            name=stmt.name,
            args=tmp_names)]

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        self.context.namespace.add_name(name.value, {
            "type_": type_
        })
        tmp, lst = self._expr(stmt.expr)
        return lst + [ast.Decl(name, type_, tmp)]