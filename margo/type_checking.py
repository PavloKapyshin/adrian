from . import ast, layers, errors


class TypeChecking(layers.TypeLayer):

    def decl(self, stmt):
        type_of_expr = self.get_type_from_expr(stmt.expr)
        if not self.types_equal(stmt.type_, type_of_expr):
            errors.types_are_not_equal(
                self.position, self.exit_on_error,
                stmt.type_, type_of_expr)
        self.namespace.add(stmt.name, {
            "type": stmt.type_
        })
        return ast.Decl(stmt.name, stmt.type_, stmt.expr)