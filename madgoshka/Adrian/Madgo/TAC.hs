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

import qualified Adrian.Madgo.Env as Env
import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.Inference as Inference


newContext :: Env.Context
newContext = ([Env.emptyEnv], Env.fromList [("temp_count", "0")])


createTempVariable :: AST.Expr -> Env.Context -> (String, AST.Node, Env.Context)
createTempVariable expr (envs, options) =
    let tempNameString = 't':(options ! "temp_count")
        tempDecl name =
            AST.VarDecl (AST.Name name) (Inference.inferTypeFromExpr expr) expr
        modifiedContext =
            (envs, update (\n -> Just $ show ((read n :: Integer) + 1)) "temp_count" options)
    in (tempNameString, tempDecl tempNameString, modifiedContext)


-- Applies three address code tranformation to expression.
-- Returns a tuple where first element is a new expression you need to put
-- instead of previous expression, second element is a list of
-- declarations of temporary variables and third element is a
-- modified context.
translateExpr :: AST.Expr -> Env.Context -> (AST.Expr, AST.AST, Env.Context)
translateExpr (AST.SExpr op left right) context =
    (newExpr, decls, modifiedContext)
    where
        (nameOfTemp1, declOfTemp1, _modifiedContext) = createTempVariable left context
        (nameOfTemp2, declOfTemp2, modifiedContext) = createTempVariable right _modifiedContext
        newExpr = AST.SExpr op (AST.NameInExpr nameOfTemp1) (AST.NameInExpr nameOfTemp2)
        decls = [declOfTemp1, declOfTemp2]
translateExpr expr context = (expr, [], context)


translateNode :: AST.Node -> Env.Context -> AST.AST
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
