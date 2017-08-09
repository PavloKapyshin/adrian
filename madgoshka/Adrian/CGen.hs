{-# LANGUAGE FlexibleInstances #-}

module Adrian.CGen where

import Data.List (intercalate)
import Text.Printf (printf)


data Op = Plus | Minus | Slash | Star
data Type = Int | Char | Ptr Type
data Expr = Expr Op Expr Expr | Val String Type
data FuncArg = FuncArg String Type
data Node = Return Expr
          | Func {
                funcName :: String,
                funcRetType :: Type,
                funcArgs :: [FuncArg],
                funcBody :: AST}
          | FuncCall String [Expr]
          | Decl String Type
          | DeclE String Expr
type AST = [Node]


class ToString a where
    toS :: a -> String

instance ToString Op where
    toS Plus = "+"
    toS Minus = "-"
    toS Slash = "/"
    toS Star = "*"

instance ToString Type where
    toS Int = "int"
    toS Char = "char"
    toS (Ptr t) = printf "%s*" (toS t)

instance ToString Expr where
    toS (Val v Int) = v
    toS (Val v Char) = printf "'%s'" v
    toS (Val v (Ptr Char)) = printf "\"%s\"" v
    toS (Val v (Ptr t)) = printf "%s*" (toS $ Val v t)
    toS (Expr op expr1 expr2) = printf "%s %s %s" (toS expr1) (toS op) (toS expr2)

instance ToString FuncArg where
    toS (FuncArg name t) = formatTypedName name t

instance ToString [FuncArg] where  -- FlexibleInstances
    toS [] = "void"
    toS args = intercalate ", " $ map toS args


gens :: AST -> String
gens ast = intercalate "\n" (gen ast)


gen :: AST -> [String]
gen = concatMap genNode


genNode :: Node -> [String]
genNode Func {funcName = name, funcRetType = rt, funcArgs = args, funcBody = body} =
    concat [
        [printf "%s %s(%s) {" (toS rt) name (toS args)],
        concatMap genNode body,
        ["}"]]
genNode (FuncCall name args) = [printf "%s(%s)" name (intercalate ", " $ map toS args)]
genNode (Return expr) = [printf "return %s;" (toS expr)]
genNode (Decl name t) = [printf "%s %s;" (toS t) name]
genNode (DeclE name expr) = [printf "%s %s = %s;" (toS $ getExprType expr) name (toS expr)]


formatTypedName :: String -> Type -> String
formatTypedName name t = printf "%s %s" (toS t) name

getExprType :: Expr -> Type
getExprType (Val _ t) = t
getExprType (Expr _ expr1 _) = getExprType expr1


main0 :: AST -> Node
main0 body = Func {funcName = "main", funcRetType = Int,
    funcArgs = [], funcBody = body}


plus :: Expr -> Expr -> Expr
plus = Expr Plus

minus :: Expr -> Expr -> Expr
minus = Expr Minus

star :: Expr -> Expr -> Expr
star = Expr Star

slash :: Expr -> Expr -> Expr
slash = Expr Slash
