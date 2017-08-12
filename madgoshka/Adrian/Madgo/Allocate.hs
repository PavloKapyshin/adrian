-- Allocates c types on the heap.
module Adrian.Madgo.Allocate where


import qualified Adrian.Madgo.AST as AST


translateNode :: AST.Node -> AST.AST
translateNode AST.VarDecl {AST.varName = (AST.Name n), AST.varType = t, AST.varExpr = e} =
    case e of
        AST.StructCall (AST.TypeFromModule "c" ctype) _ ->
            [AST.VarDecl {
                AST.varName = (AST.Name n),
                AST.varType = (AST.TypeFromModule "c" ctype),
                AST.varExpr = (
                    AST.CFuncCall {
                        AST.cfuncName = (AST.Name "malloc"),
                        AST.cfuncArgs = [
                            AST.CFuncCall {
                                AST.cfuncName = (AST.Name "sizeof"),
                                AST.cfuncArgs = [
                                    AST.ExprT $ AST.StructTypeScalar
                                        (AST.TypeFromModule "c" ctype)]
                            }]
                    })
            },
            AST.Assignment (AST.Dereference $ AST.NameInExpr n) "=" e]
        _ -> [AST.VarDecl {AST.varName = (AST.Name n), AST.varType = t, AST.varExpr = e}]


translateAST :: AST.AST -> AST.AST
translateAST ast = concat $ map translateNode ast