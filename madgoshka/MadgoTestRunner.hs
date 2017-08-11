module Main where

import Adrian.MadgoTests (tests)
import Adrian.TestUtils (runTests)


main :: IO ()
main = runTests tests
