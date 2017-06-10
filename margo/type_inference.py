from . import ast, defs, layers, errors

from vendor.paka import funcreg


class TypeInference(layers.TypeLayer):

    def decl(self, stmt):
        type_ = stmt.type_
        if isinstance(type_, ast.Empty):
            type_ = self.get_type_from_expr(stmt.expr)
        self.namespace.add(stmt.name, {
            "type": type_
        })
        return ast.Decl(stmt.name, type_, stmt.expr)

    def funccall(self, stmt):
        return ast.FuncCall(stmt.name, stmt.args)
