module Main where


import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)

import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.Parser as Parser
import qualified Adrian.Madgo.TAC as TAC
import qualified Adrian.Madgo.Error as Error


compile :: String -> Either Error.CompilationError AST.AST
compile source_code =
    let parseResult = Parser.parseSourceCode source_code in
    case parseResult of
        Left err -> Left err
        Right ast -> Right $ TAC.translateAST ast


main :: IO ()
main = do
    args <- getArgs
    let file_name = (case args of
            [file_name] -> file_name
            otherwise -> error "Supply an argument.")
    file_content <- readFile file_name
    case compile file_content of
        Left e -> case e of
            Error.CompilationError err -> putStrLn $ err
        Right ast -> print $ ast
