module Adrian.Madgo.Inference where


import qualified Adrian.Madgo.AST as AST


inferTypeFromExpr :: AST.Expr -> AST.Type
inferTypeFromExpr (AST.StructCall struct _) = struct