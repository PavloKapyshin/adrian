from . import ast
from . import defs
from . import layers
from . import errors

from vendor.adrian import cgen
from vendor.paka import funcreg


class CGen(layers.Layer):

    def _to_cop(self, op):
        d = {
            "+": cgen.COps.plus,
            "-": cgen.COps.minus,
            "*": cgen.COps.star,
            "/": cgen.COps.slash,
        }
        if op in d:
            return d[op]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

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
        d = {
            ast.CIntFast8: cgen.CTypes.int_fast8,
            ast.CUIntFast8: cgen.CTypes.uint_fast8,
            ast.CIntFast32: cgen.CTypes.int_fast32,
            ast.CUIntFast32: cgen.CTypes.uint_fast32,
            ast.CString: cgen.CTypes.ptr(cgen.CTypes.char),
            ast.CChar: cgen.CTypes.char,
        }
        if type(catom) in d:
            return d[type(catom)]
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _call_args(self, args):
        return [self._expr(arg) for arg in args]

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.FuncCall)
    def _expr_call(self, call):
        return self.funccall(call)

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        return cgen.Var(name.value)

    @_expr.reg.register(ast.CIntFast8)
    @_expr.reg.register(ast.CUIntFast8)
    @_expr.reg.register(ast.CIntFast32)
    @_expr.reg.register(ast.CUIntFast32)
    @_expr.reg.register(ast.CString)
    @_expr.reg.register(ast.CChar)
    def _expr_catom(self, catom):
        return cgen.Val(catom.value, self._to_ctype(catom))

    @_expr.reg.register(list)
    def _expr_list(self, lst):
        return cgen.Expr(
            self._to_cop(lst[0]),
            self._expr(lst[1]),
            self._expr(lst[2]))

    def decl(self, stmt):
        name = stmt.name
        expr = self._expr(stmt.expr)
        return cgen.Decl(name.value, expr)

    def funccall(self, stmt):
        # TODO: refactor.
        if isinstance(stmt.name, ast.ModuleMember):
            m_name = stmt.name.name
            m_member = stmt.name.member
            if isinstance(m_member, ast.Name):
                m_member = m_member.value
            if isinstance(m_name, ast.Name):
                m_name = m_name.value
            if not m_name in self.context.includes:
                self.context.includes[m_name] = {
                    "__INCLUDE__": cgen.Include(m_name)
                }
            self.context.includes[m_name][m_member] = cgen.CFuncDescr(
                m_member,
                rettype=defs.STD_TYPES_FUNC_SIGNATURES[m_member]["rettype"],
                args=defs.STD_TYPES_FUNC_SIGNATURES[m_member]["args"],
                includes=[self.context.includes[m_name]["__INCLUDE__"]])
            return self.context.includes[m_name][m_member](
                *self._call_args(stmt.args))
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def main(self, ast_):
        return self.context.includes.get_cgen_ast() + super().main(
            ast_)