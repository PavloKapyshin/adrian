module Adrian.MadgoTests where

import Test.HUnit (Test(TestList))

import qualified Adrian.MadgoTests.TAC as TAC


tests :: Test
tests = TestList [TAC.tests]
