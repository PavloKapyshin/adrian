module Adrian.CGenTests.Decl where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "int test_me;" (C.gens [C.Decl "test_me" C.Int])

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "int something = 42;" (C.gens [node])
    where
        node = C.DeclE "something" C.Int (C.Val "42" C.Int)

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "char* something;" (C.gens [node])
    where
        node = C.Decl "something" (C.Ptr C.Char)

test4 :: Test
test4 = TestCase $ do
    assertEqual "" "char* something = \"different\";" (C.gens [node])
    where
        node = C.DeclE "something" (C.Ptr C.Char) (C.Val "different" (C.Ptr C.Char))

test5 :: Test
test5 = TestCase $ do
    assertEqual "" "int r = 1 + 3 * 5 - 8 / 4;" (C.gens [node])
    where
        expr = (C.Val "1" C.Int) `C.plus` (
            (C.Val "3" C.Int) `C.star` (
                (C.Val "5" C.Int) `C.minus` (
                    (C.Val "8" C.Int) `C.slash` (C.Val "4" C.Int))))
        node = C.DeclE "r" C.Int expr

test6 :: Test
test6 = TestCase $ do
    assertEqual "" "int a[] = {0, 1};" (C.gens [node])
    where
        initializer = C.InitList [C.Val "0" C.Int, C.Val "1" C.Int]
        node = C.DeclE "a" (C.Array C.Int C.ArrayNoSize) initializer

test7 :: Test
test7 = TestCase $ do
    assertEqual "" "int a[2] = {0, 1};" (C.gens [node])
    where
        initializer = C.InitList [C.Val "0" C.Int, C.Val "1" C.Int]
        node = C.DeclE "a" (C.Array C.Int (C.ArraySize 2)) initializer

test8 :: Test
test8 = TestCase $ do
    assertEqual "" "int a[3] = {0, 1};" (C.gens [node])
    where
        initializer = C.InitList [C.Val "0" C.Int, C.Val "1" C.Int]
        node = C.DeclE "a" (C.Array C.Int (C.ArraySize 3)) initializer

test9 :: Test
test9 = TestCase $ do
    assertEqual "" "int a[2];" (C.gens [node])
    where
        node = C.Decl "a" (C.Array C.Int (C.ArraySize 2))


tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9]
