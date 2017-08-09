{-# LANGUAGE FlexibleInstances #-}

module Adrian.CGen where

import Data.List (intercalate)
import Text.Printf (printf)


data Type = Int | Char | Ptr Type
-- TODO: Val as subset of Expr
data Val = Val String Type
data FuncArg = FuncArg String Type
data Node = Return Val
          | Func {
                funcName :: String,
                funcRetType :: Type,
                funcArgs :: [FuncArg],
                funcBody :: AST}
          | FuncCall String [Val]
          | Decl String Type
          | DeclE String Val
type AST = [Node]


class ToString a where
    toS :: a -> String

instance ToString Type where
    toS Int = "int"
    toS Char = "char"
    toS (Ptr t) = printf "%s*" (toS t)

instance ToString Val where
    toS (Val v Int) = v
    toS (Val v Char) = printf "'%s'" v
    toS (Val v (Ptr Char)) = printf "\"%s\"" v
    toS (Val v (Ptr t)) = printf "%s*" (toS $ Val v t)

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
genNode (Return (Val v _)) = [printf "return %s;" v]
genNode (Decl name t) = [printf "%s %s;" (toS t) name]
genNode (DeclE name val@(Val _ t)) = [printf "%s %s = %s;" (toS t) name (toS val)]


formatTypedName :: String -> Type -> String
formatTypedName name t = printf "%s %s" (toS t) name


main0 :: AST -> Node
main0 body = Func {funcName = "main", funcRetType = Int,
    funcArgs = [], funcBody = body}
