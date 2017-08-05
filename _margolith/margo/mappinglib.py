"""MappingLib"""

from . import astlib, defs


class Mapping:

    def __init__(self, expr_mapping=None, type_mapping=None):
        self.expr_mapping = expr_mapping or {}
        self.type_mapping = type_mapping or {}

    def fill_expr_mapping(self):
        pass

    def fill_type_mapping(self):
        pass

    def map_name_type(self, type_):
        if isinstance(type_, astlib.Name):
            if str(type_) in self.type_mapping:
                return self.type_mapping[str(type_)]
        return type_

    def map_type(self, type_):
        if isinstance(type_, astlib.ParamedType):
            params = []
            for param in type_.params:
                params.append(self.map_name_type(param))
            return astlib.ParamedType(type_.base, params)
        return type_

    def apply_type_mapping(self, ast_):
        for stmt in ast_:
            if isinstance(stmt, astlib.Decl):
                yield astlib.Decl(
                    stmt.name, self.map_type(stmt.type_), stmt.expr)
            else:
                yield stmt

    def map_expr(self, expr):
        if isinstance(expr, astlib.Name):
            if str(expr) in self.expr_mapping:
                return self.expr_mapping[str(expr)]
        elif isinstance(expr, astlib.StructElem):
            return astlib.StructElem(self.map_expr(expr.name), expr.elem)
        elif isinstance(expr, astlib.MethodCall):
            base = self.map_expr(expr.base)
            if isinstance(base, astlib.CINT_TYPES):
                if str(expr.method) == defs.COPY_METHOD_NAME:
                    return base
            return astlib.MethodCall(
                base, expr.method,
                self.apply_expr_mapping_on_args(expr.args))
        return expr

    def apply_expr_mapping_on_args(self, args):
        for arg in args:
            yield self.map_expr(arg)

    def apply_expr_mapping(self, ast_):
        for stmt in ast_:
            if isinstance(stmt, astlib.CFuncCall):
                yield astlib.CFuncCall(
                    stmt.name, list(self.apply_expr_mapping_on_args(stmt.args)))
            elif isinstance(stmt, astlib.Assignment):
                yield astlib.Assignment(
                    self.map_expr(stmt.var), stmt.op, self.map_expr(stmt.expr))
            elif isinstance(stmt, astlib.MethodCall):
                base = self.map_expr(stmt.base)
                if isinstance(base, astlib.CINT_TYPES):
                    if str(stmt.method) == defs.COPY_METHOD_NAME:
                        yield base
                    #elif str(stmt.method) == defs.DEINIT_METHOD_NAME:
                        #continue
                yield astlib.MethodCall(
                    base, stmt.method,
                    list(self.apply_expr_mapping_on_args(stmt.args)))
            else:
                yield stmt

    def apply(self, ast_):
        mapped_by_types = list(self.apply_type_mapping(ast_))
        return list(self.apply_expr_mapping(mapped_by_types))
