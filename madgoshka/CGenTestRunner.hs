module Main where

import Test.HUnit (Counts(Counts), runTestTT)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

import Adrian.CGenTests (tests)


main :: IO ()
main = do
    Counts _ _ errors failures <- runTestTT tests
    let (msg, code) = case (errors, failures) of
         (0, 0) -> ("OK", ExitSuccess)
         (_, _) -> ("FAILED", ExitFailure 1)
    putStrLn msg
    exitWith code
