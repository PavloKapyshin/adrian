from . import ast, defs, errors, layers, arclib

from vendor.paka import funcreg


def _make_arc_projection(ns):
    def _call_args_to_deps(args):
        # if len(args) == 1 we actually creating a copy, so
        # a new memory region occurs.
        if isinstance(args[0], ast.Name) and len(args) == 2:
            return [args[0].value, args[1].value]
        return []

    def _expr_to_deps(expr):
        if isinstance(expr, ast.Name):
            return [expr.value]
        elif isinstance(expr, ast.FuncCall):
            return _call_args_to_deps(expr.args)
        # Bug can be here.
        return []

    return {
        var_name: _expr_to_deps(entry["expr"])
        for var_name, entry in ns.items()
    }


class ARC(layers.Layer):

    def _free(self, var_name):
        type_ = self.context.var_types[var_name]
        if isinstance(type_, ast.Name):
            type_name = type_.value
            free_func = ast.Name(
                "__free__" + type_name)
        elif isinstance(type_, ast.ModuleMember):
            if type_.name.value == defs.C_MODULE_NAME:
                return
            type_name = type_.member.value
            free_func = ast.ModuleMember(
                type_.name,
                "__free__" + type_name)
        return ast.FuncCall(
            name=free_func,
            args=[ast.Name(var_name)])

    def _arc(self):
        to_free = set(arclib.arc(
            _make_arc_projection(
                self.context.namespace.space(
                    )[self.context.namespace.scope])))
        free_stmts = []
        for var_name in to_free:
            subres = self._free(var_name)
            if subres:
                free_stmts.append(subres)
        # TODO: make ast; use self.context.var_types[to_free[N]]
        return free_stmts

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = stmt.expr
        self.context.namespace.add_name(name.value, {
            "expr": expr,
            "type_": type_
        })
        self.context.var_types[name.value] = type_
        return ast.Decl(name, type_, expr)

    def main(self, ast_):
        return super().main(ast_) + self._arc()
