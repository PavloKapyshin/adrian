module Adrian.CGen.AST where

data Op = Plus | Minus | Slash | Star
data Type = IntFast8 | Int | Char | Ptr Type
data Expr = Expr Op Expr Expr | Val String Type | Var String
data FuncArg = FuncArg String Type
data Node = Return Expr
          | Func {
                funcName :: String,
                funcRetType :: Type,
                funcArgs :: [FuncArg],
                funcBody :: AST}
          | FuncCall String [Expr]
          | Decl String Type
          | DeclE String Type Expr
type AST = [Node]
