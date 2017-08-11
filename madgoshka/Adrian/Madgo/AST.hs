module Adrian.Madgo.AST where


type ModuleName = String

data Type =
    Type String
  | TypeFromModule ModuleName Type
    deriving (Show, Eq)

data Name = Name String
    deriving (Show, Eq)

type Args = [Expr]

type Operator = String

data Expr =
    IntegerLiteral String
  | StructCall Type Args
  | SExpr Operator Expr Expr
  | Parentheses Expr
  | NameInExpr String
    deriving (Show, Eq)

data Node =
    VarDecl {
        varName :: Name,
        varType :: Type,
        varExpr :: Expr}
    deriving (Show, Eq)

type AST = [Node]