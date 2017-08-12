module Adrian.Madgo.Core where


import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.CoreAST as CAST


translateName :: AST.Name -> CAST.Name
translateName (AST.Name n) = CAST.Name n
translateName (AST.NameFromModule moduleName n) = CAST.NameFromModule moduleName (translateName n)


translateType :: AST.Type -> CAST.Type
translateType (AST.Type t) = CAST.StructTypePtr $ CAST.Type t
translateType (AST.TypeFromModule m t) = CAST.StructTypePtr $ CAST.TypeFromModule m (translateType t)
translateType (AST.StructTypeScalar t) = translateTypeWithoutPointers t


translateTypeWithoutPointers :: AST.Type -> CAST.Type
translateTypeWithoutPointers (AST.Type t) = CAST.Type t
translateTypeWithoutPointers (AST.TypeFromModule m t) = CAST.TypeFromModule m (translateTypeWithoutPointers t)
translateTypeWithoutPointers (AST.StructTypeScalar t) = translateTypeWithoutPointers t


translateArgs :: AST.Args -> CAST.Args
translateArgs args = map translateExpr args


translateExpr :: AST.Expr -> CAST.Expr
translateExpr (AST.IntegerLiteral literal) = CAST.IntegerLiteral literal
translateExpr (AST.StructCall struct args) =
    CAST.StructFuncCall
        (translateTypeWithoutPointers struct)
        (CAST.Name "__init__") (translateArgs args)
translateExpr (AST.SExpr op lexpr rexpr) =
    CAST.SExpr op (translateExpr lexpr) (translateExpr rexpr)
translateExpr (AST.Parentheses expr) = CAST.Parentheses (translateExpr expr)
translateExpr (AST.NameInExpr n) = CAST.NameInExpr n
translateExpr (AST.Dereference expr) = CAST.Dereference (translateExpr expr)
translateExpr (AST.CFuncCall {AST.cfuncName = n, AST.cfuncArgs = args}) =
    CAST.CFuncCall {
        CAST.cfuncName = (translateName n),
        CAST.cfuncArgs = (translateArgs args)}
translateExpr (AST.ExprT t) = CAST.ExprT (translateType t)


translateNode :: AST.Node -> CAST.Node
translateNode AST.VarDecl {AST.varName = n, AST.varType = t, AST.varExpr = e} =
    CAST.VarDecl {
        CAST.varName = (translateName n),
        CAST.varType = (translateType t),
        CAST.varExpr = (translateExpr e)}
translateNode (AST.Assignment lexpr op rexpr) =
    CAST.Assignment (translateExpr lexpr) op (translateExpr rexpr)
translateNode (AST.NodeE expr) = CAST.NodeE (translateExpr expr)


translateAST :: AST.AST -> CAST.AST
translateAST ast = map translateNode ast