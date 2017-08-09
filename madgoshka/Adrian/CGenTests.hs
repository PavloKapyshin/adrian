module Adrian.CGenTests where

import Test.HUnit (Test(TestList))

import qualified Adrian.CGenTests.Main0 as Main0
import qualified Adrian.CGenTests.Func as Func
import qualified Adrian.CGenTests.FuncCall as FuncCall
import qualified Adrian.CGenTests.Decl as Decl


tests :: Test
tests = TestList [Main0.tests, Func.tests, FuncCall.tests, Decl.tests]
