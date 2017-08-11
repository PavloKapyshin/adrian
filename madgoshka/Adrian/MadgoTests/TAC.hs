module Adrian.MadgoTests.TAC where

import Test.HUnit

import Adrian.Madgo.AST
import Adrian.Madgo.TAC (translateExpr, newContext)


testTranslationOfSExpr :: Test
testTranslationOfSExpr = TestCase $ do
    assertEqual "" (SExpr "+" (NameInExpr "t0") (NameInExpr "t1")) newExpr
    assertEqual "" [VarDecl {
                        varName = (Name "t0"),
                        varType = (TypeFromModule "c" (Type "IntFast8")),
                        varExpr = (StructCall (TypeFromModule "c" (Type "IntFast8")) [IntegerLiteral "20"])
                    },
                    VarDecl {
                        varName = (Name "t1"),
                        varType = (TypeFromModule "c" (Type "IntFast8")),
                        varExpr = (StructCall (TypeFromModule "c" (Type "IntFast8")) [IntegerLiteral "30"])
                    }] decls
    where
        (newExpr, decls, _) = (translateExpr (SExpr "+"
            (StructCall (TypeFromModule "c" (Type "IntFast8")) [IntegerLiteral "20"])
            (StructCall (TypeFromModule "c" (Type "IntFast8")) [IntegerLiteral "30"]))
            newContext)


tests :: Test
tests = TestList [testTranslationOfSExpr]
