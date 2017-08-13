module Adrian.CGenTests.Func where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "int add(int x, int y) {\nreturn 42;\n}" (C.gens [func])
    where
        args = [C.FuncArg "x" C.Int, C.FuncArg "y" C.Int]
        body = [C.Return $ C.Val "42" C.Int]
        func = C.Func {
            C.funcName = "add",
            C.funcRetType = C.Int,
            C.funcArgs = args,
            C.funcBody = body}

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "int add(int* x, char y) {\nreturn 42;\n}" (C.gens [func])
    where
        args = [C.FuncArg "x" (C.Ptr C.Int), C.FuncArg "y" C.Char]
        body = [C.Return $ C.Val "42" C.Int]
        func = C.Func {
            C.funcName = "add",
            C.funcRetType = C.Int,
            C.funcArgs = args,
            C.funcBody = body}

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "#include <stdint.h>\nint_fast8_t add(int_fast8_t a, \
        \int_fast8_t b) {\nreturn a + b;\n}\nint_fast8_t sub(int_fast8_t x, \
        \int_fast8_t y) {\nreturn x - y;\n}" (C.gens [add_func, sub_func])
    where
        add_func = C.Func {
            C.funcName = "add",
            C.funcRetType = C.IntFast8,
            C.funcArgs = [C.FuncArg "a" C.IntFast8, C.FuncArg "b" C.IntFast8],
            C.funcBody = [C.Return $ (C.Var "a") `C.plus` (C.Var "b")]}
        sub_func = C.Func {
            C.funcName = "sub",
            C.funcRetType = C.IntFast8,
            C.funcArgs = [C.FuncArg "x" C.IntFast8, C.FuncArg "y" C.IntFast8],
            C.funcBody = [C.Return $ (C.Var "x") `C.minus` (C.Var "y")]}

test4 :: Test
test4 = TestCase $ do
    assertEqual "" "int add(int x[], char* y[]) {\nreturn 42;\n}" (C.gens [func])
    where
        args = [
            C.FuncArg "x" (C.Array C.Int C.ArrayNoSize),
            C.FuncArg "y" (C.Array (C.Ptr C.Char) C.ArrayNoSize)]
        body = [C.Return $ C.Val "42" C.Int]
        func = C.Func {
            C.funcName = "add",
            C.funcRetType = C.Int,
            C.funcArgs = args,
            C.funcBody = body}


tests :: Test
tests = TestList [test1, test2, test3, test4]
