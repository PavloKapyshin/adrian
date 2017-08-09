module Adrian.CGenTests.Main0 where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "int main(void) {\nreturn 0;\n}"
        (C.gens [C.main0 [C.Return (C.Val "0" C.Int)]])


tests :: Test
tests = TestList [test1]
