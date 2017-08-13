module Adrian.CGen.LibC where

import Adrian.CGen.AST
import Adrian.CGen.Includes


malloc :: [Expr] -> Expr
malloc = CallableDescrCall $ CallableDescr {
    callableDescrName = "malloc",
    callableDescrRetType = Ptr Void,
    callableDescrArgs = [Size],
    callableDescrIncludes = [stdlib]}


free :: [Expr] -> Expr
free = CallableDescrCall $ CallableDescr {
    callableDescrName = "free",
    callableDescrRetType = Void,
    callableDescrArgs = [Ptr Void],
    callableDescrIncludes = [stdlib]}
