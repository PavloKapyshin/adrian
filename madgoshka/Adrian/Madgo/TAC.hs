-- What:
-- This layer translates expressions that are composed of several smaller ones
-- to three address code.
--
-- Why:
-- Because every variable must be freed. When we create an variable like
-- this: `var lol: c#IntFast32 = c#IntFast32(0) + c#IntFast32(1)`
-- we have no variables that point to `c#IntFast32(0)` and `c#IntFast32(1)`.
-- Also these transformations will help us to improve output code.
--
-- Example:
-- input:  var someVariable: c#IntFast8 = c#IntFast8(2) + c#IntFast8(1)
-- output: var t1: c#IntFast8 = c#IntFast8(2)
--         var t2: c#IntFast8 = c#IntFast8(1)
--         var someVariable: c#IntFast8 = t1 + t2

module Adrian.Madgo.TAC where


import qualified Adrian.Madgo.AST as AST


createTempVariable :: AST.Expr -> AST.Node
createTempVariable expr =
    AST.VariableDeclaration (AST.Name "some_tmp") (AST.Type "some_type") expr


-- Applies three address code tranformation to expression.
-- Returns a pair where first element is a new expression you need to put
-- instead of previous expression and second element is a list of
-- declarations of temporary variables.
translateExpr :: AST.Expr -> (AST.Expr, AST.AST)
translateExpr expr =
    case expr of
        AST.SExpr operator lexpr rexpr -> (
            AST.SExpr operator (AST.NameInExpr "some_") (AST.NameInExpr "some_"),
            [(createTempVariable lexpr), (createTempVariable rexpr)]
            )
        otherwise -> (expr, [])


translateVariableDeclaration :: AST.Name -> AST.Type -> AST.Expr -> AST.AST
translateVariableDeclaration name type_ expr =
    let (newExpr, tempDeclarations) = translateExpr expr in
    -- Declarations of temporary variables must be first, because we use them
    -- in new expression.
    tempDeclarations ++ [AST.VariableDeclaration name type_ newExpr]


translateNode :: AST.Node -> AST.AST
translateNode node =
    case node of
        AST.VariableDeclaration name type_ expr ->
            translateVariableDeclaration name type_ expr


-- Main function that applies three address code tranformation.
translateAST :: AST.AST -> AST.AST
translateAST ast = concat $ map translateNode ast
