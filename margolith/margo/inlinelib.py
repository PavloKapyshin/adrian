"""InlineLib"""

from . import astlib, mappinglib


class Inliner:

    def __init__(self):
        self.mapping = None

    def need_to_inline(self, declaration):
        # TODO: search for param type dependencies.
        return True

    def inline(self, call, *, declaration, type_mapping, expr_mapping):
        self.mapping = mappinglib.Mapping(
            expr_mapping=expr_mapping, type_mapping=type_mapping)
        if isinstance(call, astlib.MethodCall):
            return self.inline_method(call, declaration)
        elif isinstance(call, astlib.Instance):
            return self.inline_instance(call, declaration)
        return self.inline_func(call, declaration)

    def inline_instance(self, instance, declaration):
        body = self.mapping.apply(declaration.body)
        return_expr, inlined = None, []
        for stmt in body:
            if isinstance(stmt, astlib.Decl):
                inlined.append(
                    astlib.Decl(stmt.name, stmt.type_, stmt.expr))
            elif isinstance(stmt, astlib.Return):
                return_expr = stmt.expr
        return return_expr, inlined

    def inline_method(self, method, declaration):
        body = self.mapping.apply(declaration.body)
        return_expr, inlined = None, []
        for stmt in body:
            if isinstance(stmt, astlib.CFuncCall):
                inlined.append(
                    astlib.CFuncCall(stmt.name, stmt.args))
            elif isinstance(stmt, astlib.Return):
                return_expr = stmt.expr
        return return_expr, inlined

    def inline_func(self, func, declaration):
        pass
