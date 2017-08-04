module Adrian.Madgo.AST where


data Type = Type String
    deriving (Show)

data Name = Name String
    deriving (Show)

data Expr = IntegerLiteral String
    deriving (Show)

data Node = VariableDeclaration Name Type Expr
    deriving (Show)

type AST = [Node]