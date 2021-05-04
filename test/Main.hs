module Main (main) where

import Test.HUnit

-- This doesn't work in the current setup
import Deck

-- Just run the tests
main :: IO ()
main = runTestTTAndExit tests

test1 = TestCase (assertEqual "Always fails" 3 4)
test2 = TestCase (assertEqual "Never fails" 4 4)


tests = TestList [TestLabel "Test test" test1,
    TestLabel "testing testing" test2]