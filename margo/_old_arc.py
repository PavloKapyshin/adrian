from . import ast
from . import defs
from . import errors
from . import layers

from vendor.paka import funcreg


class ARC(layers.Layer):

    def _free(self, text, entry):
        if entry["refs"] == 0:
            return self._free.reg[entry["expr"]](self, text, entry)

    _free.reg = funcreg.TypeRegistry()

    @_free.reg.register(ast.Name)
    def _free_name(self, text, entry):
        type_ = entry["type_"]
        if isinstance(type_, ast.ModuleMember):
            return ast.FuncCall(
                name=ast.ModuleMember(
                    name=defs.STD_TYPES_MODULE_NAME,
                    member="__free__" + type_.member.value),
                args=[ast.Name(text)])
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    @_free.reg.register(ast.FuncCall)
    def _free_call(self, text, call):
        type_ = call["type_"]
        if isinstance(type_, ast.ModuleMember):
            return ast.FuncCall(
                name=ast.ModuleMember(
                    name=defs.STD_TYPES_MODULE_NAME,
                    member="__free__" + type_.member.value),
                args=[ast.Name(text)])
        errors.not_implemented(
            self.context.line, self.context.exit_on_error)

    def _arc_element(self, key, value):
        if value is None:
            return None
        if not "freed" in value:
            return self._arc_element.reg[value["expr"]](
                self, key, value)

    _arc_element.reg = funcreg.TypeRegistry()

    def _arc(self):
        result = []
        scope = self.context.namespace.scope
        for key, value in self.context.namespace.space()[scope].items():
            print("-------------------")
            print(key, value)
            print("-------------------")
            if not "freed" in value:
                subres = self._arc_element(key, value)
                if isinstance(subres, list):
                    result.extend(subres)
                elif subres:
                    result.append(subres)
        return result

    @_arc_element.reg.register(ast.Name)
    def _arc_name(self, key, value):
        entry = self.context.namespace.get(value["expr"].value)
        self.context.namespace.update(value["expr"].value, {
            "refs": entry["refs"] - 1,
            "freed": True
        })
        if (isinstance(entry["type_"], ast.ModuleMember) and \
                entry["type_"].name.value == defs.C_MODULE_NAME):
            return None
        return [self._free(key, value)] + [self._arc_element(
            value["expr"].value,
            self.context.namespace.get(value["expr"].value))]

    @_arc_element.reg.register(ast.FuncCall)
    def _arc_call(self, key, value):
        return self._arc_call.reg[value["expr"].name](
            self, key, value)

    @_arc_element.reg.register(ast.CIntFast8)
    @_arc_element.reg.register(ast.CUIntFast8)
    @_arc_element.reg.register(ast.CIntFast32)
    @_arc_element.reg.register(ast.CUIntFast32)
    @_arc_element.reg.register(ast.CChar)
    @_arc_element.reg.register(ast.CString)
    def _arc_element_catom(self, key, value):
        return None

    _arc_call.reg = funcreg.TypeRegistry()

    @_arc_call.reg.register(ast.ModuleMember)
    def _arc_call_module(self, key, call):
        args_decls = []
        for arg in call["expr"].args:
            subres = self._arc_element(
                arg.value, self.context.namespace.get(arg.value))
            if subres:
                args_decls.append(subres)
        return [self._free(
            key, call)] + args_decls

    def _expr(self, expr):
        return self._expr.reg[expr](self, expr)

    _expr.reg = funcreg.TypeRegistry()

    @_expr.reg.register(ast.Name)
    def _expr_name(self, name):
        entry = self.context.namespace.get(name.value)
        self.context.namespace.update(name.value, {
            "refs": entry["refs"] + 1
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
        return super().main(ast_) + self._arc()