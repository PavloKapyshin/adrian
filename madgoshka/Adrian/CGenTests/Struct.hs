module Adrian.CGenTests.Struct where

import Test.HUnit

import qualified Adrian.CGen as C


test1 :: Test
test1 = TestCase $ do
    assertEqual "" "struct MyStruct {\nint data;\n};" (C.gens [node])
    where
        node = C.StructDecl "MyStruct" [C.StructMemberDecl "data" C.Int]

test2 :: Test
test2 = TestCase $ do
    assertEqual "" "#include <stdint.h>\nstruct Alphab {\n\
        \int_fast32_t abc;\nint_fast32_t def;\n};" (C.gens [node])
    where
        members = [
            C.StructMemberDecl "abc" C.IntFast32,
            C.StructMemberDecl "def" C.IntFast32]
        node = C.StructDecl "Alphab" members

test3 :: Test
test3 = TestCase $ do
    assertEqual "" "#include <stdint.h>\n#include <stdlib.h>\n\
        \struct MyStruct* initMyStruct(int_fast8_t data) {\n\
        \struct MyStruct* self = malloc(sizeof(struct MyStruct));\n\
        \self->data = data;\nreturn self;\n}" (C.gens [node])
    where
        body = [
            C.DeclE "self" (C.Ptr $ C.Struct "MyStruct") $ C.malloc [
                C.SizeOf $ C.Struct "MyStruct"],
            C.Assignment (C.StructPtrElem (C.Var "self") (C.Var "data"))
                (C.Var "data"),
            C.Return $ C.Var "self"]
        node = C.Func {
            C.funcName = "initMyStruct",
            C.funcRetType = C.Ptr $ C.Struct "MyStruct",
            C.funcArgs = [C.FuncArg "data" C.IntFast8],
            C.funcBody = body}

test4 :: Test
test4 = TestCase $ do
    assertEqual "" "#include <stdint.h>\nstruct MyS* lol(void) {\n\
        \return initMyS(23);\n}" (C.gens [node])
    where
        body = [C.Return $ C.FuncCall "initMyS" [C.Val "23" C.UIntFast32]]
        node = C.Func {
            C.funcName = "lol",
            C.funcRetType = C.Ptr $ C.Struct "MyS",
            C.funcArgs = [],
            C.funcBody = body}

test5 :: Test
test5 = TestCase $ do
    assertEqual "" "#include <stdint.h>\n#include <stdlib.h>\n\
        \struct MyStruct {\nint_fast8_t data;\n};\n\
        \struct MyStruct* initMyStruct(int_fast8_t data) {\n\
        \struct MyStruct* self = malloc(sizeof(struct MyStruct));\n\
        \self->data = data;\nreturn self;\n}\nstruct MyStruct* lol(void) {\n\
        \return initMyStruct(23);\n}" (C.gens [struct_decl, init_func, lol_func])
    where
        struct_decl = C.StructDecl "MyStruct" [C.StructMemberDecl "data" C.IntFast8]
        init_func = C.Func {
            C.funcName = "initMyStruct",
            C.funcRetType = C.Ptr $ C.Struct "MyStruct",
            C.funcArgs = [C.FuncArg "data" C.IntFast8],
            C.funcBody = [
                C.DeclE "self" (C.Ptr $ C.Struct "MyStruct") $ C.malloc [
                    C.SizeOf $ C.Struct "MyStruct"],
                C.Assignment (C.StructPtrElem (C.Var "self") (C.Var "data")) $ C.Var "data",
                C.Return $ C.Var "self"]}
        lol_func = C.Func {
            C.funcName = "lol",
            C.funcRetType = C.Ptr $ C.Struct "MyStruct",
            C.funcArgs = [],
            C.funcBody = [
                C.Return $ C.FuncCall "initMyStruct" [C.Val "23" C.IntFast8]]}


tests :: Test
tests = TestList [test1, test2, test3, test4, test5]
