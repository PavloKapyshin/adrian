import pathlib

from vendor.adrian import cgen
from vendor.paka import funcreg

from . import ast, defs, cdefs, layers, errors, structs


def search_module(module_name, module_paths):
    for module_path in module_paths.iterdir():
        if module_path.name == module_name:
            return module_path


class CGen(layers.Layer):
    includes = set()

     _to_cop_dict = {
        "+": cgen.COps.plus,
        "-": cgen.COps.minus,
        "*": cgen.COps.star,
        "/": cgen.COps.slash,
    }

    _to_ctype_dict = {
        ast.CIntFast8: cgen.CTypes.int_fast8,
        ast.CUIntFast8: cgen.CTypes.uint_fast8,
        ast.CIntFast32: cgen.CTypes.int_fast32,
        ast.CUIntFast32: cgen.CTypes.uint_fast32,
        ast.CString: cgen.CTypes.ptr(cgen.CTypes.char),
        ast.CChar: cgen.CTypes.char,
    }

    def _to_cop(self, op):
        if op in _to_cop_dict:
            return _to_cop_dict[op]
        errors.not_implemented(self.position, self.exit_on_error)

    def _to_ctype(self, type_):
        return self._to_ctype.reg[type_](self, type_)

    _to_ctype.reg = funcreg.TypeRegistry()

    @_to_ctype.reg.register(ast.CIntFast8)
    @_to_ctype.reg.register(ast.CUIntFast8)
    @_to_ctype.reg.register(ast.CIntFast32)
    @_to_ctype.reg.register(ast.CUIntFast32)
    @_to_ctype.reg.register(ast.CString)
    @_to_ctype.reg.register(ast.CChar)
    def _to_ctype_catom(self, catom):
        if type(catom) in _to_ctype_dict:
            return _to_ctype_dict[type(catom)]
        errors.not_implemented(self.position, self.exit_on_error)

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.FuncCall)
    def _expr_call(self, call):
        return self.funccall(call)

    @_expr.reg.register(ast.VariableName)
    def _expr_name(self, name):
        return cgen.Var(str(name))

    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    @_expr.reg.register(ast.CString)
    @_expr.reg.register(ast.CChar)
    def _expr_catom(self, catom):
        return cgen.Val(catom.literal, self._to_ctype(catom))

    @_expr.reg.register(ast.SExpr)
    def _sexpr(self, sexpr):
        return cgen.Expr(
            self._to_cop(sexpr.op),
            self._expr(sexpr.expr1),
            self._expr(sexpr.expr2))

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def decl(self, stmt):
        type_ = self._type(stmt.type_)
        expr = self._expr(stmt.expr)
        return cgen.Decl(str(stmt.name), type_, expr)

    def funccall(self, stmt):
        if isinstance(stmt.name, ast.ModuleMember):
            module = stmt.name
            if module.name == cdefs.CMODULE_NAME:
                return cdefs.CFUNC_SIGNATURES[module.member](
                    *self._call_args(stmt.args))
        errors.not_implemented(self.position, self.exit_on_error)

    def main(self, ast_, *, exit_on_error, module_paths):
        modules = [
            search_module(module, module_paths)
            for module in self.includes]
        return modules, super().main(
            ast_, exit_on_error=exit_on_error)
