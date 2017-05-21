import ast
import defs
import layers
import errors

class OOP(layers.Layer):

    def _expr(self, expr):
        # TODO: write this function
        pass

    def decl(self, stmt):
        name = stmt.name
        type_ = stmt.type_
        expr = self._expr(stmt.expr)
        return ast.Decl(name, type_, expr)