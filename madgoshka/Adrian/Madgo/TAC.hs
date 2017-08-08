-- Three address code conversion.

module Adrian.Madgo.TAC where


import qualified Adrian.Madgo.AST as AST


translateNode :: AST.Node -> AST.Node
translateNode node =
    case node of
        AST.VariableDeclaration name type_ expr ->
            AST.VariableDeclaration name type_ expr


translateAST :: AST.AST -> AST.AST
translateAST ast = map translateNode ast
