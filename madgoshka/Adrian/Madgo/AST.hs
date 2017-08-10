module Adrian.Madgo.AST where


type ModuleName = String

data Type =
    Type String
  | TypeFromModule ModuleName Type
    deriving (Show)

data Name = Name String
    deriving (Show)

type Args = [Expr]

type Operator = String

data Expr =
    IntegerLiteral String
  | StructCall Type Args
  | SExpr Operator Expr Expr
  | Parentheses Expr
  | NameInExpr String
    deriving (Show)

data Node =
    VarDecl {
        varName :: Name,
        varType :: Type,
        varExpr :: Expr}
    deriving (Show)

type AST = [Node]