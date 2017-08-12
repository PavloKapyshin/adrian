module Adrian.Madgo.CoreAST where


type ModuleName = String

data Name =
    Name String
  | NameFromModule ModuleName Name
    deriving (Show, Eq)

data Type =
    Type String
  | TypeFromModule ModuleName Type
  | StructTypePtr Type
    deriving (Show, Eq)

type Args = [Expr]
type Operator = String
data Expr =
    IntegerLiteral String
  | StructFuncCall Type Name Args
  | SExpr Operator Expr Expr
  | Dereference Expr
  | Parentheses Expr
  | NameInExpr String
  | ExprT Type
  | CFuncCall {
        cfuncName :: Name,
        cfuncArgs :: Args}
    deriving (Show, Eq)

data Node =
    VarDecl {
        varName :: Name,
        varType :: Type,
        varExpr :: Expr}
  | Assignment Expr Operator Expr
  | NodeE Expr
    deriving (Show, Eq)

type AST = [Node]