module Adrian.Madgo.AST where


type ModuleName = String

data Type = Type String | TypeFromModule ModuleName Type
    deriving (Show)

data Name = Name String
    deriving (Show)

data Expr = IntegerLiteral String
    deriving (Show)

data Node = VariableDeclaration Name Type Expr
    deriving (Show)

type AST = [Node]