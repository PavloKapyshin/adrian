module Adrian.Madgo.AST where


type ModuleName = String

data Type = Type String | TypeFromModule ModuleName Type
    deriving (Show)

data Name = Name String
    deriving (Show)

type Args = [Expr]

data Expr = IntegerLiteral String | StructCall Type Args
    deriving (Show)

data Node = VariableDeclaration Name Type Expr
    deriving (Show)

type AST = [Node]