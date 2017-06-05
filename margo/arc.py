from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


class ARC(layers.Layer):

    def _mark_freed(self, text):
        entry = self.context.namespace.get(text)
        expr = entry["expr"]
        text_ = text
        while True:
            self.context.namespace.update(text_, {
                "freed": True
            })
            expr = self.context.namespace.get(text_)["expr"]
            if isinstance(expr, ast.Name):
                text_ = expr.value
            else:
                break

    def _free(self, text, entry):
        if (entry["refs"] == 0) and (not "freed" in entry):
            return self._free.reg[entry["expr"]](self, text, entry)

    _free.reg = funcreg.TypeRegistry()

    @_free.reg.register(ast.Name)
    @_free.reg.register(ast.FuncCall)
    def _free_name(self, text, entry):
        type_ = entry["type_"]
        self._mark_freed(text)
        if isinstance(type_, ast.ModuleMember):
            return ast.FuncCall(
                name=ast.ModuleMember(
                    name=defs.STD_TYPES_MODULE_NAME,
                    member="__free__" + type_.member.value),
                args=[ast.Name(text)])
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _arc_element(self, key, value):
        if not "freed" in value:
            return self._arc_element.reg[value["expr"]](
                self, key, value)

    _arc_element.reg = funcreg.TypeRegistry()

    @_arc_element.reg.register(ast.Name)
    def _arc_element_name(self, key, value):
        # var some = s
        expr_entry = self.context.namespace.get(value["expr"].value)
        self.context.namespace.update(value["expr"].value, {
            "refs": expr_entry["refs"] - 1
        })
        if (isinstance(expr_entry["type_"], ast.ModuleMember) and \
                expr_entry["type_"].name.value == defs.C_MODULE_NAME):
            return None
        return self._free(key, value)

    @_arc_element.reg.register(ast.FuncCall)
    def _arc_element_call(self, key, value):
        # var some = std_types#__init__Integer(0)
        expr = value["expr"]
        if isinstance(expr.name, ast.ModuleMember):
            self._arc_element_call_args(expr.args)
            print(key)
            return self._free(key, value)
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_arc_element.reg.register(ast.CIntFast8)
    @_arc_element.reg.register(ast.CUIntFast8)
    @_arc_element.reg.register(ast.CIntFast32)
    @_arc_element.reg.register(ast.CUIntFast32)
    @_arc_element.reg.register(ast.CChar)
    @_arc_element.reg.register(ast.CString)
    def _arc_element_catom(self, key, value):
        return None

    @_arc_element.reg.register(list)
    def _arc_element_list(self, key, lst):
        return None

    def _arc_element_call_args(self, args):
        for arg in args:
            if isinstance(arg, ast.Name):
                entry = self.context.namespace.get(arg.value)
                self.context.namespace.update(arg.value, {
                    "refs": entry["refs"] - 1
                })

    def _arc(self):
        result = []
        scope = self.context.namespace.scope
        for key, value in self.context.namespace.space()[scope].items():
            subres = self._arc_element(key, value)
            if isinstance(subres, list):
                result.extend(subres)
            elif subres:
                result.append(subres)
        return result

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        entry = self.context.namespace.get(name.value)
        self.context.namespace.update(name.value, {
            "refs": entry["refs"] + 1,
        })
        return name

    @_expr.reg.register(ast.FuncCall)
    def _expr_call(self, call):
        args = self._expr_call_args(call.args)
        return ast.FuncCall(
            name=call.name,
            args=args)

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

    def _expr_call_args(self, args):
        return [self._expr(arg) for arg in args]

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = self._expr(stmt.expr)
        self.context.namespace.add_name(name.value, {
            "expr": expr,
            "refs": 0,
            "type_": type_
        })
        return ast.Decl(name, type_, expr)

    def main(self, ast_):
        result = super().main(ast_)
        arc_ast = self._arc()
        while arc_ast:
            result += arc_ast
            arc_ast = self._arc()
        return result