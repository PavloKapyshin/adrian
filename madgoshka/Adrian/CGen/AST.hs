module Adrian.CGen.AST where

data Op = Plus | Minus | Slash | Star
data Type = IntFast8 | Int | Char | Size | Void | Ptr Type
data Expr = Expr Op Expr Expr
          | Val String Type
          | Var String
          | FuncCall String [Expr]
          | FuncDescrCall FuncDescr [Expr]
data Include = Include String deriving (Eq, Ord)
data FuncDescr = FuncDescr {
    funcDescrName :: String,
    funcDescrRetType :: Type,
    funcDescrArgs :: [Type],
    funcDescrIncludes :: [Include]}
data FuncArg = FuncArg String Type
data Node = Return Expr
          | Func {
                funcName :: String,
                funcRetType :: Type,
                funcArgs :: [FuncArg],
                funcBody :: AST}
          | Decl String Type
          | DeclE String Type Expr
          | StmtE Expr
type AST = [Node]
