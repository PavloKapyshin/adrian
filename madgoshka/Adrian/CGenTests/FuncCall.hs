module Adrian.CGenTests.FuncCall where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "add(2, \"3\");" (C.gens [C.StmtE call])
    where
        call = C.FuncCall "add" [C.Val "2" C.Int, C.Val "3" (C.Ptr C.Char)]

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "add(1, 2, '3');" (C.gens [C.StmtE call])
    where
        call = C.FuncCall "add" [C.Val "1" C.Int, C.Val "2" C.Int, C.Val "3" C.Char]


tests :: Test
tests = TestList [test1, test2]
