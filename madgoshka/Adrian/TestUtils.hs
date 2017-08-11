module Adrian.TestUtils (runTests) where

import Test.HUnit (Test, Counts(Counts), runTestTT)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)


runTests :: Test -> IO ()
runTests tests = do
    Counts _ _ errors failures <- runTestTT tests
    let (msg, code) = case (errors, failures) of
         (0, 0) -> ("OK", ExitSuccess)
         (_, _) -> ("FAILED", ExitFailure 1)
    putStrLn msg
    exitWith code
