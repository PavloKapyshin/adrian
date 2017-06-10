from . import ast, defs, errors, layers

from vendor.paka import funcreg


class SimpleExpr(layers.Layer):
    tmp_count = 0
    tmp_string = "tmp"

    def _get_info_from_call(self, name):
        if isinstance(name, ast.ModuleMember):
            if name.name == defs.STD_TYPES_MODULE_NAME:
                return defs.STD_TYPES_FUNC_SIGNATURES[name.member]
        return self.funcspace.get(name)

    def _tmp(self, type_, expr):
        self.tmp_count += 1
        tmp_name = self.tmp_string + str(self.tmp_count)
        return ast.Pair(
            self.position.line, self.position.column,
            ast.Decl(ast.VariableName(tmp_name), type_, expr))

    def _expr(self, expr):
        return self._expr.registry[expr](self, expr)

    _expr.registry = funcreg.TypeRegistry()

    @_expr.registry.register(ast.VariableName)
    def _expr_name(self, name):
        return name.copy(), []

    @_expr.registry.register(ast.Instance)
    def _expr_instance(self, call):
        type_ = call.struct
        result = self.instance(call)
        result = result[:-1] + [result[-1].stmt]
        tmp_decl_stmt = self._tmp(type_, result[-1])
        tmp_name = tmp_decl_stmt.stmt.name
        return tmp_name, result[:-1] + [tmp_decl_stmt]

    @_expr.registry.register(ast.FuncCall)
    def _expr_func_call(self, call):
        info = self._get_info_from_call(call.name)
        type_ = info["rettype"]
        result = self.funccall(call)
        result = result[:-1] + [result[-1].stmt]
        tmp_decl_stmt = self._tmp(type_, result[-1])
        tmp_name = tmp_decl_stmt.stmt.name
        return tmp_name, result[:-1] + [tmp_decl_stmt]

    @_expr.registry.register(ast.CString)
    @_expr.registry.register(ast.CChar)
    @_expr.registry.register(ast.CIntFast8)
    @_expr.registry.register(ast.CUIntFast8)
    @_expr.registry.register(ast.CIntFast32)
    @_expr.registry.register(ast.CUIntFast32)
    def _expr_catom(self, catom):
        return catom, []

    @_expr.registry.register(ast.SExpr)
    def _sexpr(self, sexpr):
        tmp_decls = []
        expr1_tmp, expr1_decls = self._expr(sexpr.expr1)
        expr2_tmp, expr2_decls = self._expr(sexpr.expr2)
        tmp_decls.extend(expr1_decls)
        tmp_decls.extend(expr2_decls)
        return ast.SExpr(
            sexpr.op, expr1_tmp,
            expr2_tmp), tmp_decls

    def _call_args(self, args):
        tmp_names, tmp_decls = [], []
        for arg in args:
            tmp_name, list_ = self._expr(arg)
            if isinstance(tmp_name, str):
                tmp_names.append(ast.VariableName(tmp_name))
            else:
                tmp_names.append(tmp_name)
            tmp_decls.extend(list_)
        return tmp_names, tmp_decls

    def decl(self, stmt):
        self.namespace.add(stmt.name, {
            "type": stmt.type_
        })
        tmp, list_ = self._expr(stmt.expr)
        return list_ + [ast.Pair(
            self.position.line, self.position.column,
            ast.Decl(stmt.name, stmt.type_, tmp))]

    def funccall(self, stmt):
        tmp_names, tmp_decls = self._call_args(stmt.args)
        return tmp_decls + [ast.Pair(
            self.position.line, self.position.column,
            ast.FuncCall(stmt.name, tmp_names))]

    def instance(self, stmt):
        tmp_names, tmp_decls = self._call_args(stmt.args)
        return tmp_decls + [ast.Pair(
            self.position.line, self.position.column,
            ast.Instance(stmt.struct, tmp_names))]
