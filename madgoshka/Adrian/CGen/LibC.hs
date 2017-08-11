module Adrian.CGen.LibC where

import Adrian.CGen.AST
import Adrian.CGen.Includes


malloc :: [Expr] -> Expr
malloc = FuncDescrCall $ FuncDescr {
    funcDescrName = "malloc",
    funcDescrRetType = Ptr Void,
    funcDescrArgs = [Size],
    funcDescrIncludes = [stdlib]}


free :: [Expr] -> Expr
free = FuncDescrCall $ FuncDescr {
    funcDescrName = "free",
    funcDescrRetType = Void,
    funcDescrArgs = [Ptr Void],
    funcDescrIncludes = [stdlib]}
