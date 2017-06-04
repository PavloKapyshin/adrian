from . import ast
from . import defs
from . import errors
from . import layers


class SimpleExpr(layers.Layer):
    tmp_number = 0
    tmp_string = "tmp"

    def _temp(self, type_, expr):
        self.tmp_number += 1
        tmp_name = self.tmp_string + str(self.tmp_number)
        return ast.Decl(tmp_name, type_, expr)

    def _make_temps_funccall(self, expr):
        # var s = c#initInt32(1)
        # var s = myFun()
        if isinstance(expr.name, ast.ModuleMember):
            if expr.name.module_name == defs.C_MODULE_NAME:
                # TODO: check for func existence.
                info = defs.C_FUNC_SIGNATURES[expr.name.member]
            elif expr.name.module_name == defs.STD_TYPES_MODULE_NAME:
                # TODO: check for func existence.
                info = defs.STD_TYPES_FUNC_SIGNATURES[expr.name.member]
        else:
            info = self.context.funcspace.get(expr)
        type_ = info["rettype"]
        result = self.funccall(expr)
        tmp_decl_stmt = self._temp(type_, result[-1])
        tmp_name = tmp_decl_stmt.name
        return tmp_name, [tmp_decl_stmt]

    def _expr(self, expr):
        if isinstance(expr, ast.FuncCall):
            return self._make_temps_funccall(expr)
        errors.not_implemented(self.context.line, self.context.exit_on_error)

    def _call_args(self, args):
        new_args = []
        for arg in args:
            tmp_name, lst = self._expr(arg)
            new_args.append((tmp_name, lst))
        return new_args

    def funccall(self, stmt):
        # TODO: write code for builtins.
        args = self._call_args(stmt.args)
        # Collect declarations and names of temp vars from args.
        tmp_decls = []
        tmp_names = []
        for tmp_name, lst in args:
            tmp_names.append(tmp_name)
            tmp_decls.extend(lst)
        return tmp_decls + [ast.FuncCall(stmt.name, tmp_names)]

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        self.context.namespace.add_name(name.value, {
            "type": type_
        })
        expr, lst = self._expr(stmt.expr)
        return lst + [ast.Decl(name, type_, ast.Name(expr))]