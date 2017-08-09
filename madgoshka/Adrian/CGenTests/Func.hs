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


tests :: Test
tests = TestList [test1, test2]
