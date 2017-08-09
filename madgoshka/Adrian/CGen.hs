{-# LANGUAGE FlexibleInstances #-}

module Adrian.CGen where

import Data.List (intercalate)
import Text.Printf (printf)


data Type = Int
data Val = Val String Type
data FuncArg = FuncArg String Type
data Node = Return Val
          | Func {
                funcName :: String,
                funcRetType :: Type,
                funcArgs :: [FuncArg],
                funcBody :: AST}
type AST = [Node]


class ToString a where
    toS :: a -> String

instance ToString Type where
    toS Int = "int"

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
genNode (Return (Val v _)) = [printf "return %s;" v]


formatTypedName :: String -> Type -> String
formatTypedName name t = printf "%s %s" (toS t) name


main0 :: AST -> Node
main0 body = Func {funcName = "main", funcRetType = Int,
    funcArgs = [], funcBody = body}
