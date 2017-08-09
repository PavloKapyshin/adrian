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
        node = C.DeclE "something" (C.Val "42" C.Int)

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "char* something;" (C.gens [node])
    where
        node = C.Decl "something" (C.Ptr C.Char)

test4 :: Test
test4 = TestCase $ do
    assertEqual "" "char* something = \"different\";" (C.gens [node])
    where
        node = C.DeclE "something" (C.Val "different" (C.Ptr C.Char))


tests :: Test
tests = TestList [test1, test2, test3, test4]
