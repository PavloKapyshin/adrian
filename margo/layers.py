"""Framework for creating layers for margolith."""
from . import ast, defs, errors

from vendor.paka import funcreg


class Layer:

    def __init__(self, context=None):
        self.namespace = None
        self.funcspace = None
        self.typespace = None
        if context:
            self.namespace = context.namespace
            self.funcspace = context.funcspace
            self.typespace = context.typespace

    def decl(self, stmt):
        """Overrides in subclass."""

    def struct_decl(self, stmt):
        """Overrides in subclass."""

    def func_decl(self, stmt):
        """Overrides in subclass."""

    def funccall(self, stmt):
        """Overrides in subclass."""

    def _make_funcs_dict(self):
        return {
            ast.Decl: self.decl,
            ast.StructDecl: self.struct_decl,
            ast.FuncDecl: self.func_decl,
            ast.FuncCall: self.funccall,
        }

    def main(self, ast_, *, exit_on_error=True):
        self.exit_on_error = exit_on_error
        funcs = self._make_funcs_dict()
        result = []
        for pair in ast_:
            self.position = ast.Position(pair.line, pair.column)
            result.append(
                ast.Pair(pair.line, pair.column,
                    funcs[type(pair.stmt)](pair.stmt)))
        return result
        # self.funcs = self._make_funcs_dict()
        # result = []
        # for pair in ast_:
        #     if isinstance(pair, ast.Pair):
        #         self.position = ast.Position(pair.line, pair.column)
        #         res = self.funcs[type(pair.stmt)](pair.stmt)
        #         if isinstance(res, list):
        #             for elem in res:
        #                 result.append(ast.Pair(
        #                     pair.line, pair.column, elem))
        #         else:
        #             result.append(ast.Pair(
        #                 pair.line, pair.column, res))
        #     else:
        #         res = self.funcs[type(pair)](pair)
        #         if isinstance(res, list):
        #             for elem in res:
        #                 result.append(ast.Pair(
        #                     self.position.line, self.position.column,
        #                     elem))
        #         else:
        #             result.append(ast.Pair(
        #                 self.position.line, self.position.column, res))
        # return result

class TypeLayer(Layer):

    def types_equal(self, type1, type2):
        return self.types_equal.registry[type1](self, type1, type2)

    types_equal.registry = funcreg.TypeRegistry()

    @types_equal.registry.register(ast.TypeName)
    def types_equal_name(self, type1, type2):
        if isinstance(type2, ast.TypeName):
            return type1 == type2
        errors.types_are_not_equal(
            self.position, self.exit_on_error, type1, type2)

    @types_equal.registry.register(ast.ModuleMember)
    def types_equal_name(self, type1, type2):
        if isinstance(type2, ast.ModuleMember):
            return (type1.name == type2.name and \
                    self.types_equal(type1.member, type2.member))
        errors.types_are_not_equal(
            self.position, self.exit_on_error, type1, type2)

    def get_type_from_expr(self, expr):
        return self.get_type_from_expr.registry[expr](self, expr)

    get_type_from_expr.registry = funcreg.TypeRegistry()

    @get_type_from_expr.registry.register(ast.VariableName)
    def get_type_from_name(self, name):
        return self.namespace.get(name)["type"]

    @get_type_from_expr.registry.register(ast.FuncCall)
    def get_type_from_func_call(self, call):
        if isinstance(call.name, ast.ModuleMember):
            module = call.name
            if isinstance(module.member, ast.TypeName):
                errors.not_implemented(self.position, self.exit_on_error)
            elif isinstance(module.member, ast.FunctionName):
                # Only std types are supported, for now :D
                func_info = defs.STD_TYPES_FUNC_SIGNATURES[module.member]
                return func_info["rettype"]
        errors.not_implemented(self.position, self.exit_on_error)

    @get_type_from_expr.registry.register(ast.Integer)
    @get_type_from_expr.registry.register(ast.String)
    @get_type_from_expr.registry.register(ast.CIntFast8)
    @get_type_from_expr.registry.register(ast.CUIntFast8)
    @get_type_from_expr.registry.register(ast.CIntFast32)
    @get_type_from_expr.registry.register(ast.CUIntFast32)
    @get_type_from_expr.registry.register(ast.CChar)
    @get_type_from_expr.registry.register(ast.CString)
    def _get_type_from_atom(self, atom):
        return atom.to_type()

    @get_type_from_expr.registry.register(ast.SExpr)
    def get_type_from_sexpr(self, sexpr):
        expr1_type = self.get_type_from_expr(sexpr.expr1)
        expr2_type = self.get_type_from_expr(sexpr.expr2)
        if not self.types_equal(expr1_type, expr2_type):
            errors.types_are_not_equal(
                self.position, self.exit_on_error,
                expr1_type, expr2_type)
        return expr1_type