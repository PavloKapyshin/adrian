{-# LANGUAGE FlexibleInstances #-}

module Adrian.CGen.Generator where

import qualified Data.List as List
import qualified Data.Set as Set
import Text.Printf (printf)
import qualified Control.Monad.State as ST

import Adrian.CGen.AST
import Adrian.CGen.Includes


data ASTGenState = ASTGenState (Set.Set Include)


gen :: AST -> [String]
gen ast = let
    (ASTGenState includes, rest) = accumulateState (ASTGenState Set.empty) ast
    in List.concat [map toS . Set.toList $ includes, rest]


mergeStates :: ASTGenState -> ASTGenState -> ASTGenState
mergeStates (ASTGenState aIncludes) (ASTGenState bIncludes) = let
    in ASTGenState $ aIncludes `Set.union` bIncludes

accumulateState :: ASTGenState -> AST -> (ASTGenState, [String])
accumulateState initialState ast = let
    accumState (oldState, oldLines) node = let
        (newLines, newState) = ST.runState (genNode node) oldState
        in (mergeStates oldState newState, concat [oldLines, newLines])
    in List.foldl' accumState (initialState, []) ast


addIncludes :: [Type] -> ST.State ASTGenState ()
addIncludes types = do
    let includes = concatMap typeToIncludes types
    oldState <- ST.get
    ST.put $ mergeStates oldState (ASTGenState $ Set.fromList includes)
    return ()

addIncludesFromExpr :: Expr -> ST.State ASTGenState ()
addIncludesFromExpr expr = do
    let includes = collectIncludes expr
    oldState <- ST.get
    ST.put $ mergeStates oldState (ASTGenState $ Set.fromList includes)
    return ()

collectIncludes :: Expr -> [Include]
collectIncludes (Var _) = []
collectIncludes (Val _ t) = typeToIncludes t
collectIncludes (Cast expr t) = concat [collectIncludes expr, typeToIncludes t]
collectIncludes (Ref expr) = collectIncludes expr
collectIncludes (DeRef expr) = collectIncludes expr
collectIncludes (SizeOf t) = typeToIncludes t
collectIncludes (FuncCall _ exprs) = concatMap collectIncludes exprs
collectIncludes (FuncDescrCall descr exprs) =
    concat [funcDescrIncludes descr, concatMap collectIncludes exprs]
collectIncludes (Expr _ expr1 expr2) =
    concat [collectIncludes expr1, collectIncludes expr2]
collectIncludes (InitList exprs) = concatMap collectIncludes exprs


genNode :: Node -> ST.State ASTGenState [String]
genNode Func {funcName = name, funcRetType = rt, funcArgs = args, funcBody = body} = do
    oldState <- ST.get
    let (newState, bodyLines) = accumulateState oldState body
    ST.put newState
    addIncludes $ concat [[rt], map (\(FuncArg _ t) -> t) args]
    return $ concat [
        [printf "%s %s(%s) {" (toS rt) name (toS args)],
        bodyLines,
        ["}"]]
genNode (Return expr) = do
    addIncludesFromExpr expr
    return [printf "return %s;" (toS expr)]
genNode (Decl name t) = do
    addIncludes [t]
    return [printf "%s;" $ formatDeclLValue t name]
genNode (DeclE name t expr) = do
    addIncludes [t]
    addIncludesFromExpr expr
    return [printf "%s = %s;" (formatDeclLValue t name) (toS expr)]
genNode (StmtE expr) = do
    addIncludesFromExpr expr
    return [printf "%s;" (toS expr)]
genNode (Assignment expr1 expr2) = do
    addIncludesFromExpr expr1
    addIncludesFromExpr expr2
    return [printf "%s = %s;" (toS expr1) (toS expr2)]


formatTypedName :: String -> Type -> String
formatTypedName name (Array t _) = printf "%s %s[]" (toS t) name
formatTypedName name t = printf "%s %s" (toS t) name

formatDeclLValue :: Type -> String -> String
formatDeclLValue (Array t ArrayNoSize) name = printf "%s %s[]" (toS t) name
formatDeclLValue (Array t (ArraySize size)) name = printf "%s %s[%d]" (toS t) name size
formatDeclLValue t name = printf "%s %s" (toS t) name


class ToString a where
    toS :: a -> String

instance ToString Op where
    toS Plus = "+"
    toS Minus = "-"
    toS Slash = "/"
    toS Star = "*"

instance ToString Type where
    toS UIntFast8 = "uint_fast8_t"
    toS IntFast8 = "int_fast8_t"
    toS Int = "int"
    toS Char = "char"
    toS Size = "size_t"
    toS Void = "void"
    toS (Ptr t) = printf "%s*" (toS t)
    toS (Array _ _) = undefined

instance ToString Expr where
    toS (Val v UIntFast8) = v
    toS (Val v IntFast8) = v
    toS (Val v Int) = v
    toS (Val v Char) = printf "'%s'" v
    toS (Val v Size) = v
    toS (Val _ Void) = undefined
    toS (Val v (Ptr Char)) = printf "\"%s\"" v
    toS (Val v (Ptr t)) = printf "%s*" (toS $ Val v t)
    toS (Val _ (Array _ _)) = undefined
    toS (InitList exprs) = printf "{%s}" (List.intercalate ", " $ map toS exprs)
    toS (Expr op expr1 expr2) = printf "%s %s %s" (toS expr1) (toS op) (toS expr2)
    toS (Var name) = name
    toS (Cast expr t) = printf "(%s)(%s)" (toS t) (toS expr)
    toS (Ref expr) = printf "&(%s)" (toS expr)
    toS (DeRef expr) = printf "*(%s)" (toS expr)
    toS (SizeOf t) = printf "sizeof(%s)" (toS t)
    toS (FuncCall name args) = printf "%s(%s)" name (List.intercalate ", " $ map toS args)
    toS (FuncDescrCall (FuncDescr {funcDescrName = name}) args) =
        toS $ FuncCall name args

instance ToString FuncArg where
    toS (FuncArg name t) = formatTypedName name t

instance ToString [FuncArg] where  -- FlexibleInstances
    toS [] = "void"
    toS args = List.intercalate ", " $ map toS args

instance ToString Include where
    toS (Include header) = printf "#include <%s>" header
