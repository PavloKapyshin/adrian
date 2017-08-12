module Main where


import System.Environment (getArgs)

import qualified Adrian.Madgo.TAC as TAC
import qualified Adrian.Madgo.Error as Error
import qualified Adrian.Madgo.Parser as Parser
import qualified Adrian.Madgo.Allocate as Allocate
import qualified Adrian.Madgo.CoreAST as CoreAST
import qualified Adrian.Madgo.Core as Core


compile :: String -> Either Error.CompilationError CoreAST.AST
compile source_code =
    let parseResult = Parser.parseSourceCode source_code in
    case parseResult of
        Left err -> Left err
        Right ast -> Right $ Core.translateAST $ Allocate.translateAST (TAC.translateAST ast)


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
