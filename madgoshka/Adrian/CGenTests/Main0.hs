module Adrian.CGenTests.Main0 where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "int main(void) {\nreturn 0;\n}"
        (C.gens [C.main0 [C.Return (C.Val "0" C.Int)]])

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "int main(void) {\nreturn 2 - 1 - 1;\n}" (C.gens [func])
    where
        expr = ((C.Val "2" C.Int) `C.minus` (C.Val "1" C.Int))
            `C.minus` (C.Val "1" C.Int)
        func = C.main0 [C.Return expr]

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "int main(void) {\nint res = 0;\nreturn res;\n}" (C.gens [func])
    where
        func = C.main0 [
            C.DeclE "res" C.Int (C.Val "0" C.Int),
            C.Return (C.Var "res")]


tests :: Test
tests = TestList [test1, test2, test3]
