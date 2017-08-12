module Adrian.Madgo.CoreAST where


type ModuleName = String

data Name = Name String
    deriving (Show, Eq)

data Type =
    Type String
  | TypeFromModule ModuleName Type
    deriving (Show, Eq)

type Args = [Expr]
type Operator = String
data Expr =
    IntegerLiteral String
  | StructFunctionCall Type Name Args
  | SExpr Operator Expr Expr
  | Parentheses Expr
  | NameInExpr String
    deriving (Show, Eq)

data Node =
    VarDecl {
        varName :: Name,
        varType :: Type,
        varExpr :: Expr}
  | CFuncCall {
        cfuncName :: Name,
        cfuncArgs :: Args}
    deriving (Show, Eq)

type AST = [Node]