module Adrian.CGen (
    ArraySize(..), Type(..), Expr(..), FuncArg(..), Node(..), AST,
    gens, main0, plus, minus, star, slash,
    malloc, free) where

import Data.List (intercalate)

import Adrian.CGen.AST
import Adrian.CGen.Generator
import Adrian.CGen.LibC
import Adrian.CGen.Utils


gens :: AST -> String
gens ast = intercalate "\n" (gen ast)
