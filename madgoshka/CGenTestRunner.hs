module Main where

import Adrian.CGenTests (tests)
import Adrian.TestUtils (runTests)


main :: IO ()
main = runTests tests
