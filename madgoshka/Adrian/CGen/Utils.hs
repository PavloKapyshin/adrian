module Adrian.CGen.Utils where

import Adrian.CGen.AST


main0 :: AST -> Node
main0 body = Func {funcName = "main", funcRetType = Int,
    funcArgs = [], funcBody = body}


plus :: Expr -> Expr -> Expr
plus = Expr Plus

minus :: Expr -> Expr -> Expr
minus = Expr Minus

star :: Expr -> Expr -> Expr
star = Expr Star

slash :: Expr -> Expr -> Expr
slash = Expr Slash
