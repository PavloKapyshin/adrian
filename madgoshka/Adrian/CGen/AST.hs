module Adrian.CGen.AST where

data Op = Plus | Minus | Slash | Star
data ArraySize = ArraySize Integer | ArrayNoSize
data Type = UIntFast8
          | IntFast8
          | Int
          | Char
          | Size
          | Void
          | Ptr Type
          | Array Type ArraySize
data Expr = Expr Op Expr Expr
          | Val String Type
          | Var String
          | FuncCall String [Expr]
          | FuncDescrCall FuncDescr [Expr]
          | Cast Expr Type
          | Ref Expr
          | DeRef Expr
          | SizeOf Type
          | InitList [Expr]
          | ArrayElem Expr Expr
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
          | Assignment Expr Expr
type AST = [Node]
