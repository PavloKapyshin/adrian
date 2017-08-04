module Main where


import System.Environment (getArgs)

import qualified Adrian.Madgo.AST as AST
import qualified Adrian.Madgo.Parser as Parser


compile :: String -> AST.AST
compile source_code = Parser.parse source_code


main :: IO ()
main = do
    args <- getArgs
    let file_name = (case args of
            [file_name] -> file_name
            otherwise -> error "Supply an argument.")
    file_content <- readFile file_name
    print $ compile file_content
