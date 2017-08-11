module Adrian.CGenTests.LibC where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "#include <stdlib.h>\nint main(void) {\n\
        \void* chunk = malloc(1645);\nfree(chunk);\nreturn 0;\n}"
        (C.gens [func])
    where
        func = C.main0 [
            C.DeclE "chunk" (C.Ptr C.Void) (C.malloc [C.Val "1645" C.Size]),
            C.StmtE (C.free [C.Var "chunk"]),  -- yeah, no NULL check :)
            C.Return (C.Val "0" C.Int)]


tests :: Test
tests = TestList [test1]
