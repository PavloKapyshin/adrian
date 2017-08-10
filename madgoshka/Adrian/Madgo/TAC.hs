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


import Data.Map ((!), update)

import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.SymbolTable as STable


newContext :: STable.Context
newContext = ([STable.emptyTable], STable.fromList [("temp_count", "0")])


createTempVariable :: AST.Expr -> STable.Context -> (String, AST.Node, STable.Context)
createTempVariable expr (tables, compilerOptions) =
    let nameString = 't':(compilerOptions ! "temp_count")
        tempDecl name =
            AST.VarDecl (AST.Name name) (AST.Type "some_type") expr
        updatedCompilerOptions =
            update (\n -> Just $ show ((read n :: Integer) + 1)) "temp_count" compilerOptions
    in (nameString, tempDecl nameString, (tables, updatedCompilerOptions))


-- Applies three address code tranformation to expression.
-- Returns a pair where first element is a new expression you need to put
-- instead of previous expression and second element is a list of
-- declarations of temporary variables.
translateExpr :: AST.Expr -> STable.Context -> (AST.Expr, AST.AST, STable.Context)
translateExpr expr context = case expr of
    AST.SExpr op left right ->
        let (tempName1, tempDecl1, context1) = createTempVariable left context in
        let (tempName2, tempDecl2, context2) = createTempVariable right context1 in
        (AST.SExpr op (AST.NameInExpr tempName1) (AST.NameInExpr tempName2),
          [tempDecl1, tempDecl2], context2)
    _ -> (expr, [], context)


translateNode :: AST.Node -> STable.Context -> AST.AST
translateNode AST.VarDecl {AST.varName = name, AST.varType = type_, AST.varExpr = expr} context =
    let (newExpr, tempDeclarations, _) = translateExpr expr context in
    -- Declarations of temporary variables must be first, because we use them
    -- in new expression.
    tempDeclarations ++ [AST.VarDecl {
        AST.varName = name,
        AST.varType = type_,
        AST.varExpr = newExpr}]


-- Main function that applies three address code tranformation.
translateAST :: AST.AST -> AST.AST
translateAST ast = concat $ map (`translateNode` newContext) ast
