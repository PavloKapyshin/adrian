module Adrian.CGenTests.Assignment where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "myvar = some_other_var;" (C.gens [node])
    where
        node = C.Assignment (C.Var "myvar") (C.Var "some_other_var")


tests :: Test
tests = TestList [test1]
