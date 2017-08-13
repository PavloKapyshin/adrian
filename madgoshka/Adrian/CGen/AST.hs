module Adrian.CGen.AST where

data Op = Plus | Minus | Slash | Star
data ArraySize = ArraySize Integer | ArrayNoSize
data Type = UIntFast8
          | IntFast8
          | UIntFast32
          | IntFast32
          | Int
          | Char
          | Size
          | Void
          | Ptr Type
          | Array Type ArraySize
          | Struct String
data Expr = Expr Op Expr Expr
          | Val String Type
          | Var String
          | FuncCall String [Expr]
          | CallableDescrCall CallableDescr [Expr]
          | Cast Expr Type
          | Ref Expr
          | DeRef Expr
          | SizeOf Type
          | InitList [Expr]
          | ArrayElem Expr Expr
          | StructPtrElem Expr Expr
data Include = Include String deriving (Eq, Ord)
data CallableDescr = CallableDescr {
    callableDescrName :: String,
    callableDescrRetType :: Type,
    callableDescrArgs :: [Type],
    callableDescrIncludes :: [Include]}
data FuncArg = FuncArg String Type
data StructMemberDecl = StructMemberDecl String Type
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
          | StructDecl String [StructMemberDecl]
type AST = [Node]
