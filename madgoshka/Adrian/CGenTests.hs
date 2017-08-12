module Adrian.CGenTests where

import Test.HUnit (Test(TestList))

import qualified Adrian.CGenTests.Main0 as Main0
import qualified Adrian.CGenTests.Func as Func
import qualified Adrian.CGenTests.FuncCall as FuncCall
import qualified Adrian.CGenTests.Decl as Decl
import qualified Adrian.CGenTests.LibC as LibC
import qualified Adrian.CGenTests.Expr as Expr
import qualified Adrian.CGenTests.Assignment as Assignment


tests :: Test
tests = TestList [
    Main0.tests, Func.tests, FuncCall.tests, Decl.tests, LibC.tests,
    Expr.tests, Assignment.tests]
