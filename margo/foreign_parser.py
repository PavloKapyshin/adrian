from . import ast, parser_ast

from vendor.paka import funcreg


class ForeignParser:

    def tr_call_args(self, args):
        return [self.tr_expr(arg) for arg in args]

    def tr_type(self, type_):
        if type_ == parser_ast.EMPTY:
            return ast.Empty()
        elif parser_ast.isinstance_(type_, parser_ast.NAME):
            return ast.Name(type_[1])
        elif parser_ast.isinstance_(type_, parser_ast.MODULE_MEMBER):
            return ast.ModuleMember(
                type_[1], self.tr_type(type_[2]))
        elif parser_ast.isinstance_(type_, parser_ast_.STRUCT_ELEM):
            return ast.StructElem(
                self.tr_type(type_[1]), self.tr_type(type_[2]))
        errors.not_implemented(self.position, self.exit_on_error)


    def tr_expr(self, expr):
        if expr == parser_ast.EMPTY:
            return ast.Empty()
        elif parser_ast.isinstance_(expr, parser_ast.SEXPR):
            return ast.SExpr(
                expr[1], self.tr_expr(expr[2]),
                self.tr_expr(expr[3]))
        elif parser_ast.isinstance_(expr, parser_ast.NAME):
            return ast.Name(expr[1])
        elif parser_ast.isinstance_(expr, parser_ast.MODULE_MEMBER):
            return ast.ModuleMember(expr[1], self.tr_expr(expr[2]))
        elif parser_ast.isinstance_(expr, parser_ast.STRUCT_ELEM):
            return ast.StructElem(
                self.tr_expr(expr[1]), self.tr_expr(expr[2]))
        elif parser_ast.isinstance_(expr, parser_ast.INTEGER):
            return ast.Integer(expr[1])
        elif parser_ast.isinstance_(expr, parser_ast.STRING):
            return ast.String(expr[1])
        elif parser_ast.isinstance_(expr, parser_ast.FUNC_CALL):
            return ast.FuncCall(
                self.tr_expr(expr[1]), self.tr_call_args(expr[2]))
        elif parser_ast.isinstance_(expr, parser_ast.METHOD_CALL):
            return ast.MethodCall(
                self.tr_expr(expr[1]), self.tr_expr(expr[2]),
                self.tr_call_args(expr[3]))
        errors.not_implemented(self.position, self.exit_on_error)


    registry = funcreg.NameRegistry()


    @registry.register(parser_ast.DECL)
    def decl(self, stmt):
        return ast.Decl(
            stmt[1], self.tr_type(stmt[2]), self.tr_expr(stmt[3]))


    @registry.register(parser_ast.FUNC_CALL)
    def funccall(self, stmt):
        return ast.FuncCall(
            self.tr_expr(stmt[1]), self.tr_call_args(stmt[2]))


    def main(self, parser_ast_, *, exit_on_error):
        self.exit_on_error = exit_on_error
        result = []
        for pair in parser_ast_:
            line = pair[1]
            column = pair[2]
            self.position = ast.Position(line, column)
            stmt = pair[3]
            result.append(
                ast.Pair(
                    line, column,
                    self.registry[parser_ast.type_(stmt)](
                        self, stmt)))
        return result