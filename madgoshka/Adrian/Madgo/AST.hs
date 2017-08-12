module Adrian.Madgo.AST where


type ModuleName = String

data Type =
    Type String
  | TypeFromModule ModuleName Type
  | StructTypeScalar Type
    deriving (Show, Eq)

data Name =
    Name String
  | NameFromModule ModuleName Name
    deriving (Show, Eq)

type Args = [Expr]
type Operator = String
data Expr =
    IntegerLiteral String
  | StructCall Type Args
  | SExpr Operator Expr Expr
  | Parentheses Expr
  | NameInExpr String
  | Dereference Expr
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