from . import ast, defs, cdefs, errors, layers, arclib

from vendor.paka import funcreg


def _make_arc_projection(ns):
    def _call_args_to_deps(args):
        # if len(args) == 1 we actually creating a copy, so
        # a new memory region occurs.
        if isinstance(args[0], ast.VariableName) and len(args) == 2:
            return [args[0], args[1]]
        return []

    def _expr_to_deps(expr):
        if isinstance(expr, ast.VariableName):
            return [expr]
        elif isinstance(expr, ast.FuncCall):
            return _call_args_to_deps(expr.args)
        # Bug can be here.
        return []

    return {
        var_name: _expr_to_deps(entry["expr"])
        for var_name, entry in ns.items()
    }


class ARC(layers.Layer):

    var_types = {}

    def _free(self, var_name):
        type_ = self.var_types[var_name]
        if isinstance(type_, ast.TypeName):
            free_func = ast.FunctionName(
                "__free__" + type_)
        elif isinstance(type_, ast.ModuleMember):
            if type_.name == cdefs.CMODULE_NAME:
                return
            type_name = type_.member
            free_func = ast.ModuleMember(
                ast.ModuleName(type_.name),
                ast.FunctionName("__free__" + type_name))
        return ast.FuncCall(
            name=free_func,
            args=[ast.VariableName(var_name)])

    def _arc(self):
        to_free = set(arclib.arc(
            _make_arc_projection(
                self.namespace.space(
                    )[self.namespace.scope])))
        free_stmts = []
        for var_name in to_free:
            subres = self._free(var_name)
            if subres:
                free_stmts.append(subres)
        # TODO: make ast; use self.var_types[to_free[N]]
        return free_stmts

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        self.namespace.add(str(name), {
            "expr": expr,
            "type": type_
        })
        self.var_types[str(name)] = type_
        return ast.Decl(name, type_, expr)

    def funccall(self, stmt):
        # Only c#Void functions are supported.
        return ast.FuncCall(stmt.name, stmt.args)

    def main(self, ast_, *, exit_on_error):
        return super().main(ast_, exit_on_error=exit_on_error) + self._arc()
