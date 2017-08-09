module Adrian.CGenTests where

import Test.HUnit (Test(TestList))

import qualified Adrian.CGenTests.Main0 as Main0
import qualified Adrian.CGenTests.Func as Func


tests = TestList [Main0.tests, Func.tests]
