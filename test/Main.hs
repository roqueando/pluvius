module Main (main) where

import Test.HUnit


-- We will start some tests here to calculate features and will separate this
test1 :: Test
test1 = TestCase (assertEqual "testing something" (1 :: Int) (1 :: Int))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = runTestTT tests
