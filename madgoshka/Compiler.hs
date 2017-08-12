module Main where


import System.Environment (getArgs)

import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.TAC as TAC
import qualified Adrian.Madgo.Error as Error
import qualified Adrian.Madgo.Parser as Parser
import qualified Adrian.Madgo.Allocate as Allocate


compile :: String -> Either Error.CompilationError AST.AST
compile source_code =
    let parseResult = Parser.parseSourceCode source_code in
    case parseResult of
        Left err -> Left err
        Right ast -> Right $ Allocate.translateAST (TAC.translateAST ast)


main :: IO ()
main = do
    args <- getArgs
    let fileName = (case args of
            [fn] -> fn
            _ -> error "Supply an argument.")
    fileContent <- readFile fileName
    case compile fileContent of
        Left e -> case e of
            Error.CompilationError err -> putStrLn $ err
        Right ast -> print $ ast
