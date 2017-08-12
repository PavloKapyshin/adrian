module Adrian.CGenTests.Expr where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "b;" (C.gens [C.StmtE expr])
    where
        expr = C.Var "b"

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "#include <stdint.h>\n(uint_fast8_t)(b);" (C.gens [C.StmtE expr])
    where
        expr = C.Cast (C.Var "b") C.UIntFast8

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "#include <stdint.h>\n(uint_fast8_t)(3 - 1);" (C.gens [C.StmtE expr])
    where
        expr = C.Cast ((C.Val "3" C.Int) `C.minus` (C.Val "1" C.Int)) C.UIntFast8

test4 :: Test
test4 = TestCase $ do
    assertEqual "" "#include <stdint.h>\n*((uint_fast8_t)(3 - 1));" (C.gens [C.StmtE expr])
    where
        expr = C.DeRef $ C.Cast ((C.Val "3" C.Int) `C.minus` (C.Val "1" C.Int)) C.UIntFast8

test5 :: Test
test5 = TestCase $ do
    -- (some C joke)(explicit cast with malloc and sizeof of char)
    assertEqual "" "#include <stdlib.h>\n\
        \char* thing = (char*)(malloc(sizeof(char)));" (C.gens [node])
    where
        expr = C.Cast (C.malloc [C.SizeOf C.Char]) (C.Ptr C.Char)
        node = C.DeclE "thing" (C.Ptr C.Char) expr


tests :: Test
tests = TestList [test1, test2, test3, test4, test5]
